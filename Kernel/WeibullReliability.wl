(* ::Package:: *)

BeginPackage["AntonAntonov`ReliabilityTools`WeibullReliability`"];

Begin["`Private`"];

Needs["AntonAntonov`ReliabilityTools`"];

ClearAll[WeibullReliabilityModel];
WeibullReliabilityModel[___]:=(Echo["Not implemented yet."]; $Failed);


ClearAll[WeibullReliabilityPlot];

Options[WeibullReliabilityPlot]=Join[
        {
                "PointColor"->RGBColor[.1, .3, .7],
                "FittedCurveColor"->RGBColor[.85,.2, .15],
                PointSize->0.018,
                "Subtitle"->Style["Weibull Reliability Probability Plot", 15,Bold],
                AxesLabel->Automatic
        },
        Options[Graphics]
];

WeibullReliabilityPlot[data_List,opts:OptionsPattern[]] := Module[
    {times, n, probabilities, coords, x, y, a, beta, eta,
     xmin, xmax, ymin, ymax,
     pTicks, tTicks, pTrans, grid, axes, axesLabels, labels, fitLine, xMargin
    },

    (*Sort failure times*)
    times = Sort[N[data]];
    n=Length[times];
    If[n<3||!AllTrue[times,#>0&],
    Return[$Failed]
    ];

    (*Bernard's median-rank approximation*)
    probabilities=Table[(i-0.3)/(n+0.4),{i,n}];

    (*Weibull coordinate transformation*)
    pTrans[p_]:=Log[-Log[1-p]];
    coords=Transpose[{Log[times],pTrans/@probabilities}];
    x=coords[[All,1]];
    y=coords[[All,2]];

    (*Linear least-squares fit:y=a+beta x*)
    {a,beta}=LeastSquares[Transpose[{ConstantArray[1,n],x}],y];
    eta=Exp[-a/beta];

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
    axesLabels=OptionValue[WeibullReliabilityPlot,AxesLabel];
    axesLabels=Which[
        ListQ[axesLabels]&&Length[axesLabels]>=2,
        OptionValue[WeibullReliabilityPlot,AxesLabel],

        TrueQ[axesLabels===Automatic],
        {Style["Time to failure", 13],Style["Cumulative failure (%)", 13]},

        True,
        Echo["Cannot process the value given to AxesLabel."];
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
    fitLine = {Directive[OptionValue[WeibullReliabilityPlot,"FittedCurveColor"], Thick],Line[{{xmin, a + beta xmin}, {xmax, a + beta xmax}}]};

    Graphics[{grid, axes, fitLine,
        (*Observed data*)
        Directive[OptionValue[WeibullReliabilityPlot,"PointColor"], PointSize[OptionValue[WeibullReliabilityPlot,"PointSize"]]], Point[coords],
        (*Characteristic-life reference*)
        Directive[LightDarkSwitched[GrayLevel[.4],GrayLevel[.6]], Dashed],Line[{{Log[eta], ymin}, {Log[eta], 0}}], labels,
        Text[OptionValue[WeibullReliabilityPlot,"Subtitle"], Offset[{0, 32}, {(xmin + xmax) / 2, ymax}]],
        Text[Style[Row[{"\[Beta] = ", NumberForm[beta, {5, 3}], Spacer[10],"\[Eta] = ", NumberForm[eta, {8, 2}]}], 11], Offset[{0, 14}, {(xmin + xmax) / 2, ymax}]]
      },
      FilterRules[{opts},Options[Graphics]],
      PlotRange -> {{xmin, xmax}, {ymin, ymax}},
      PlotRangeClipping -> False,
      ImagePadding-> {{105, 35}, {85, 65}},
      ImageSize -> 700,
      AspectRatio->1/GoldenRatio,
      Background -> LightDarkSwitched[White,GrayLevel[0.1]]
    ]
];

End[]; (* `Private` *)

EndPackage[]
