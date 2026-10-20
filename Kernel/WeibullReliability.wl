(* ::Package:: *)

BeginPackage["AntonAntonov`ReliabilityTools`WeibullReliability`"];

Begin["`Private`"];

Needs["AntonAntonov`ReliabilityTools`"];
Needs["AntonAntonov`MonadicQuantileRegression`"];

(********************************************************)
(* Weibull reliability model                            *)
(********************************************************)

WeibullFit[coords:{{_?NumericQ, _?NumericQ}..}] := 
    Module[{x, y},
        x=coords[[All,1]];
        y=coords[[All,2]];
        {a,beta}=LeastSquares[Transpose[{ConstantArray[1, Length[x]],x}],y];
        eta=Exp[-a/beta];
        {a, beta, eta}
    ];

ClearAll[WeibullReliabilityModel];

WeibullReliabilityModel::nargs = "The first argument is expected to be a list of three or more positive numbers.";

Options[WeibullReliabilityModel] = {"Breaks" -> True, "Knots" -> 6};

WeibullReliabilityModel[data_List, opts: OptionsPattern[]] := Module[
    {times, n, probabilities, coords, x, y, a, beta, eta, breaks, knots},

    (*Sort failure times*)
    times = Sort[N[data]];
    n=Length[times];
    If[n<3 || !AllTrue[times,#>0&],
        Message[WeibullReliabilityModel::nargs];
        Return[$Failed]
    ];

    (*Bernard's median-rank approximation*)
    probabilities=Table[(i-0.3)/(n+0.4),{i,n}];

    (*Weibull coordinate transformation*)
    pTrans[p_]:=Log[-Log[1-p]];
    coords=Transpose[{Log[times],pTrans/@probabilities}];

    (*Linear least-squares fit:y=a+beta x*)
    {a,beta,eta}=WeibullFit[coords];

    (*Structural breaks*)
    (*The paclet already depends/uses QRMon and the reliability data is relatively small.*)
    breaks = None;
    qrObj = None;
    If[TrueQ @ OptionValue[WeibullReliabilityModel, "Breaks"],
        
        (* Delegating possible error to QRMon **)
        knots = OptionValue[WeibullReliabilityModel, "Knots"];

        qrObj =
        Fold[
            QRMonBind,
            QRMonUnit[coords],
            {
                QRMonQuantileRegression[knots, 0.5],
                QRMonFindStructuralBreaks[]
            }
        ];

        (* Check for failure *)
        breaks = If[QRMonUnitQ[qrObj], QRMonBind[qrObj, QRMonTakeValue], (*ELSE*) $Failed];
    ];

    (*For every break find the top & bottom models*)
    If[MatchQ[breaks, {{_?NumericQ, _?NumericQ} ..}],
        breaks = Association @ Map[
            Module[{sp = #[[1]], coords1, coords2},
                coords1 = Select[coords, #[[1]] <= sp &]; 
                coords2 = Select[coords, #[[1]] >= sp &];
                # -> 
                <|
                    "Bottom" -> AssociationThread[{"a", "beta", "eta"}, WeibullFit[coords1]],
                    "Top" -> AssociationThread[{"a", "beta", "eta"}, WeibullFit[coords2]] 
                |>

            ]&,
            breaks
        ]
    ];

    ReliabilityModel @ <| 
        "OriginalData" -> times, 
        "Data" -> coords, 
        "WeibullParameters" -> <| "a" -> a, "eta" -> eta, "beta" -> beta |>, 
        "Breaks" -> breaks 
    |>
];

(********************************************************)
(* Reliability model.                                   *)
(********************************************************)

Clear[ReliabilityModel];

ReliabilityModel[a_?AssociationQ]["Properties"] :=
    {"Association", "Breaks", "Data", "MaxBreakPoint", "OriginalData", "Properties", "WeibullParameters"};
ReliabilityModel[a_?AssociationQ]["MaxBreakPoint"] := 
    Module[{aBreaks, break},
        aBreaks = KeySortBy[a["Breaks"], -#[[2]] &];
        break = Keys[aBreaks][[1, 1]];
        {break, aBreaks[[1]]["Bottom"]["a"] + aBreaks[[1]]["Bottom"]["beta"] * break}
    ];
ReliabilityModel[a_?AssociationQ]["Association"] := a;
ReliabilityModel[a_?AssociationQ][key_?StringQ] := Lookup[a, key];

(********************************************************)
(* Weibull reliability plot                             *)
(********************************************************)
ClearAll[WeibullReliabilityPlot];

Options[WeibullReliabilityPlot]=Join[
        {
                "PointColor"->RGBColor[.1, .3, .7],
                "FittedCurveColor"-> None,
                "TopCurveColor"-> StandardBlue,
                "BottomCurveColor"->Orange,
                "BreakPointColor"->Red,
                PlotTheme -> "Production",
                PointSize->0.018,
                "Subtitle"->Style["Weibull Reliability Probability Plot", 15,Bold],
                AxesLabel->Automatic
        },
        Options[Graphics]
];

WeibullReliabilityPlot[data_List, opts:OptionsPattern[]] :=
    WeibullReliabilityPlot[WeibullReliabilityModel[data], opts];

WeibullReliabilityPlot::naxlbl = "Cannot process the value given to AxesLabel.";
WeibullReliabilityPlot::nthe = "The value of the option PlotTheme is expected to be one of \"Production\", \"Failure\", or Automatic.";

WeibullReliabilityPlot[rmObj_ReliabilityModel, opts:OptionsPattern[]] := Module[
    {theme, aModel, times, coords, x, y, a, beta, eta,
     xmin, xmax, ymin, ymax,
     pTicks, tTicks, pTrans, fittedCurveColor, grid, axes, axesLabels, labels, fitLine, xMargin,
     aBreaks, break, break2D, topLine, bottomLine
    },

    theme = OptionValue[WeibullReliabilityPlot, PlotTheme];
    If[TrueQ[theme === Automatic], theme = "Production"];
    If[!MemberQ[{"Production", "Failure"}, theme],
        Message[WeibullReliabilityPlot::nthe];
        Return[$Failed]
    ];

    aModel = rmObj["Association"];
    times = aModel["OriginalData"];
    coords = aModel["Data"];
    a = aModel["WeibullParameters"]["a"];
    eta = aModel["WeibullParameters"]["eta"];
    beta = aModel["WeibullParameters"]["beta"];

    x=coords[[All,1]];
    y=coords[[All,2]];

    (*Weibull coordinate transformation*)
    pTrans[p_]:=Log[-Log[1-p]];

    (*Plot coordinate ranges*)
    xMargin=0.08*(Max[x]-Min[x]);
    xmin=Min[x]-xMargin;
    xmax=Max[x]+xMargin;

    (*Standard Weibull probability ticks*)
    pTicks={.01,.02,.05,.10,.20,.30,.50,1-Exp[-1],.70,.80,.90,.95,.99};
    ymin=Min[Min[y],pTrans[.01]]-0.15;
    ymax=Max[Max[y],pTrans[.99]]+0.15;

    (*Logarithmic time ticks: 10^{1, 2, 3, 5} time units *)
    tTicks=Select[Flatten[
            Table[{1,2,3,5} 10.^k,{k,Floor[Log[10,Min[times]]]-1,Ceiling[Log[10,Max[times]]]+1}]],xmin<=Log[#]<=xmax&];

    (*Grid lines*)
    grid={
        Directive[LightDarkSwitched[GrayLevel[.88],GrayLevel[.72]], Thin],
        Table[Line[{{xmin, pTrans[p]}, {xmax, pTrans[p]}}], {p, pTicks}],
        Table[Line[{{Log[t], ymin}, {Log[t], ymax}}], {t, tTicks}]
    };

    (*Axes and tick marks*)
    axes={
        LightDarkSwitched[Black,White],
        AbsoluteThickness[1.2],
        Line[{{xmin,ymin},{xmax,ymin}}], Line[{{xmin,ymin},{xmin,ymax}}],
        Table[ Line[{{xmin,pTrans[p]},{xmin+0.015 (xmax-xmin),pTrans[p]}}],{p,pTicks}],
        Table[Line[{{Log[t],ymin}, {Log[t], ymin+0.025}}],{t, tTicks}]
    };

    (*Axis labels and probability tick labels*)
    axesLabels=OptionValue[WeibullReliabilityPlot, AxesLabel];
    axesLabels=Which[
        
        ListQ[axesLabels]&&Length[axesLabels]>=2,
        OptionValue[WeibullReliabilityPlot,AxesLabel],

        TrueQ[axesLabels === Automatic] && theme == "Production", 
        {Style["Production output", 13], Style["Cumulative production (%)", 13]},

        TrueQ[axesLabels === Automatic],
        {Style["Time to failure", 13], Style["Cumulative failure (%)", 13]},

        True,
        Message[WeibullReliabilityPlot::naxlbl];
        Return[$Failed]
    ];

    labels = {
        Table[
        Text[
        Style[ToString[NumberForm[100 p, {4, 1}]] <> "%", 10],
        Offset[{-9, 0}, {xmin,pTrans[p]}], {1, 0}],
        {p, pTicks}],
        Table[Text[Style[ToString[Round[t]], 10], Offset[{0, -12}, {Log[t], ymin}], {0, 1}], {t, tTicks}],
        Text[axesLabels[[1]], Offset[{0, -48}, {(xmin + xmax) / 2, ymin}]],
        Text[Rotate[axesLabels[[2]], Pi / 2],Offset[{-70, 0}, {xmin, (ymin + ymax) / 2}]]
    };

    (*Fitted Weibull straight line*)
    fittedCurveColor = OptionValue[WeibullReliabilityPlot,"FittedCurveColor"];
    fittedCurveColor = 
        Which[
            ColorQ[fittedCurveColor], fittedCurveColor, 
            TrueQ[fittedCurveColor===Automatic], LightDarkSwitched[GrayLevel[0.7],GrayLevel[0.3]], 
            True, None
        ];
    fitLine = {Directive[fittedCurveColor, Thick], Line[{{xmin, a + beta xmin}, {xmax, a + beta xmax}}]};

    aBreaks = KeySortBy[aModel["Breaks"], -#[[2]] &];
    break = Keys[aBreaks][[1, 1]];
  
    topLine = {Directive[OptionValue[WeibullReliabilityPlot,"TopCurveColor"], Thick],
        Line[{
            {xmin, aBreaks[[1]]["Top"]["a"] + aBreaks[[1]]["Top"]["beta"] * xmin}, 
            {xmax,  aBreaks[[1]]["Top"]["a"] + aBreaks[[1]]["Top"]["beta"] * xmax}
        }]};

    bottomLine = {Directive[OptionValue[WeibullReliabilityPlot,"BottomCurveColor"], Thick],
        Line[{
            {xmin,  aBreaks[[1]]["Bottom"]["a"] + aBreaks[[1]]["Bottom"]["beta"] * xmin},
            {break, aBreaks[[1]]["Bottom"]["a"] + aBreaks[[1]]["Bottom"]["beta"] * break}
        }]};    

    break2D = {break, aBreaks[[1]]["Bottom"]["a"] + aBreaks[[1]]["Bottom"]["beta"] * break};

    Graphics[{
        (*Grid and axes*)
        grid, axes,

        (*Fitted curve*) 
        If[ColorQ[fittedCurveColor] || TrueQ[Head[fittedCurveColor] === LightDarkSwitched], fitLine,  Nothing],

        (*Observed data*)
        Directive[OptionValue[WeibullReliabilityPlot,"PointColor"], PointSize[OptionValue[WeibullReliabilityPlot,"PointSize"]]], Point[coords],

        (*Characteristic-life reference*)
        Directive[LightDarkSwitched[GrayLevel[.4],GrayLevel[.6]], Dashed], Line[{{Log[eta], ymin}, {Log[eta], 0}}], labels,
        Text[OptionValue[WeibullReliabilityPlot,"Subtitle"], Offset[{0, 60}, {(xmin + xmax) / 2, ymax}]],
        Text[Style[Row[{
            "Overall:", Spacer[10], 
            "\[Beta] = ", NumberForm[beta, {5, 3}], Spacer[10], 
            "\[Eta] = ", NumberForm[eta, {8, 2}]}], 11], Offset[{0, 42}, {(xmin + xmax) / 2, ymax}]],
        Text[Style[Row[{
            "    Top:", Spacer[10], 
            "\[Beta] = ", NumberForm[aBreaks[[1]]["Top"]["beta"], {5, 3}], Spacer[10], 
            "\[Eta] = ", NumberForm[aBreaks[[1]]["Top"]["eta"], {8, 2}]}], 11], Offset[{0, 28}, {(xmin + xmax) / 2, ymax}]],
        Text[Style[Row[{
            "Bottom:", Spacer[10], 
            "\[Beta] = ", NumberForm[aBreaks[[1]]["Bottom"]["beta"], {5, 3}], Spacer[10], 
            "\[Eta] = ", NumberForm[aBreaks[[1]]["Bottom"]["eta"], {8, 2}]}], 11], Offset[{0, 14}, {(xmin + xmax) / 2, ymax}]],            
        Dashing[{}],
        Tooltip[topLine, aBreaks[[1]]["Top"]], 
        Tooltip[bottomLine, aBreaks[[1]]["Bottom"]],
        {
            OptionValue[WeibullReliabilityPlot,"BreakPointColor"], 
            PointSize[0.02], Tooltip[Point[break2D], break2D]
        }
      },
      FilterRules[{opts},Options[Graphics]],
      PlotRange -> {{xmin, xmax}, {ymin, ymax}},
      PlotRangeClipping -> False,
      ImagePadding-> {{105, 35}, {85, 95}},
      ImageSize -> 700,
      AspectRatio->1/GoldenRatio,
      Background -> LightDarkSwitched[White,GrayLevel[0.1]]
    ]
];

(*=========================================================*)
(* Summary Box                                             *)
(*=========================================================*)

ReliabilityModel /: MakeBoxes[obj_ReliabilityModel, form : StandardForm] :=
    Block[{ctx = First[obj], above, below},
      
      (* 1. Construct the Always Visible Items *)
      above = If[KeyExistsQ[ctx, "Data"],
        {BoxForm`SummaryItem[{"Data dimensions: ", Dimensions[ctx["Data"]]}]},
        {BoxForm`SummaryItem[{"No data"}]}
      ];
      
      (* 2. Construct the Hidden/Expanded Items *)
      below = DeleteCases[Flatten[{
        If[KeyExistsQ[ctx, "WeibullParameters"],
          BoxForm`SummaryItem[{"Weibull parameters: ", ctx["WeibullParameters"]}],
          Nothing
        ],
        If[KeyExistsQ[ctx, "OriginalData"],
          BoxForm`SummaryItem[{"OriginalData dimensions: ", Dimensions[ctx["OriginalData"]]}] ,
          Nothing
        ],
        If[KeyExistsQ[ctx, "Breaks"],
          BoxForm`SummaryItem[{"Breaks: ", Dataset@ctx["Breaks"]}],
          Nothing
        ]
      }], Nothing];

      (* 3. Arrange and return valid boxes *)
      BoxForm`ArrangeSummaryBox[
        ReliabilityModel, obj, None, 
        above, below, 
        form, 
        "Interpretable" -> Automatic
      ]
    ];


End[]; (* `Private` *)

EndPackage[]
