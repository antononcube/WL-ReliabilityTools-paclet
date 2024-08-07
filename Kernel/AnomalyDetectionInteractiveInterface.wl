(* Mathematica Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: AnomalyDetectionInteractiveInterface *)
(* :Context: AnomalyDetectionInteractiveInterface` *)
(* :Author: Anton Antonov *)
(* :Date: 2024-08-02 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 14.0 *)
(* :Copyright: (c) 2024 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["AntonAntonov`ReliabilityTools`AnomalyDetectionInterface`"];
(* Exported symbols added here with SymbolName::usage *)

AnomalyDetectionInterface::usage = "Make an anomaly detection interface for given time series data.";

Begin["`Private`"];

Needs["AntonAntonov`MonadicQuantileRegression`"];
Needs["AntonAntonov`OutlierIdentifiers`"];

ClearAll[AnomalyDetectionInterface];

Options[AnomalyDetectionInterface] = {AspectRatio -> 1 / 3, ImageSize -> 800};

aNameToParamFinder = <|
  "Hampel" -> HampelIdentifierParameters,
  "SPLUS" -> SPLUSQuartileIdentifierParameters,
  "Quartile" -> QuartileIdentifierParameters
|>;

AnomalyDetectionInterface[tsDataArg_?AssociationQ, optsArg : OptionsPattern[]] :=
    With[{
      tsData = tsDataArg,
      opts = FilterRules[Join[{optsArg}, Options[AnomalyDetectionInterface]], Options[DateListPlot]]
    },
      DynamicModule[{
        dataId, rescaleQ, method, knots, topProb, bottomProb, oi, re, th, thQ,
        data, foundOutliers, foundOutliersPos, qrObj,
        errsAbsolute, errsRelative, gr11, gr12, gr21, gr22,
        monPlotFunc = QRMonDateListPlot, plotFunc = DateListPlot},

        Manipulate[

          data = tsData[dataId];
          data = QuantityMagnitude[data["Path"]];

          qrObj = QRMonUnit[data];

          If[rescaleQ,
            qrObj = Fold[QRMonBind, qrObj, {QRMonRescale[Axes -> {True, False}]}];
            monPlotFunc = QRMonPlot;
            plotFunc = ListPlot;
          ];

          If[method == "ByRegressionQuantiles",
            qrObj =
                Fold[QRMonBind,
                  qrObj, {QRMonQuantileRegression[knots, {bottomProb, topProb}],
                  QRMonOutliers[]}]
            ,
            (*ELSE*)
            qrObj =
                Fold[QRMonBind,
                  qrObj,
                  {QRMonQuantileRegression[knots, {0.5}],
                    QRMonFindAnomaliesByResiduals["Threshold" -> If[thQ, th, None], "OutlierIdentifier" -> oi, "RelativeErrors" -> re],
                    QRMonSetDataPlotOptions[{PlotStyle -> Gray}]}
                ]
          ];

          foundOutliers = QRMonBind[qrObj, QRMonTakeValue];
          gr11 =
              Fold[QRMonBind,
                qrObj,
                {
                  monPlotFunc[Sequence @@ opts, "Echo" -> False, PerformanceGoal -> "Speed"],
                  QRMonTakeValue}
              ];

          If[method == "ByRegressionQuantiles",
            foundOutliersPos = Flatten[Map[Position[QRMonBind[qrObj, QRMonTakeData], #] &, Join @@ Values[foundOutliers]]];
            gr21 =
                Fold[
                  QRMonBind,
                  qrObj,
                  {
                    QRMonOutliersPlot["DateListPlot" -> Not[rescaleQ], "Echo" -> False, Sequence @@ opts],
                    QRMonTakeValue
                  }];
            {gr12, gr22} =
                Values@Fold[QRMonBind,
                  qrObj, {QRMonErrorPlots["Echo" -> False,
                    "RelativeErrors" -> False, Sequence @@ opts], QRMonTakeValue}],

            (*ELSE*)

            foundOutliersPos =
                Flatten[Position[QRMonBind[qrObj, QRMonTakeData], #] & /@
                    foundOutliers];
            errsAbsolute =
                First[Fold[
                  QRMonBind,
                  qrObj,
                  {
                    QRMonErrors["RelativeErrors" -> False],
                    QRMonTakeValue
                  }]];
            errsRelative =
                First[Fold[
                  QRMonBind,
                  qrObj,
                  {QRMonErrors["RelativeErrors" -> True], QRMonTakeValue}]];
            gr21 =
                DateListPlot[{QRMonBind[qrObj, QRMonTakeData],
                  If[Length[foundOutliersPos] > 0, foundOutliers, Nothing]}, Joined -> {True, False}, PlotStyle -> {{}, {PointSize[0.012]}}, Sequence @@ opts];
            gr12 =
                DateListPlot[{errsAbsolute,
                  If[Length[foundOutliersPos] > 0,
                    Part[errsAbsolute, foundOutliersPos], Nothing]},
                  GridLines -> {{}, {-th, th}}, PlotLabel -> "Absolute errors",
                  Sequence @@ opts, Joined -> False, Filling -> Axis];
            gr22 =
                DateListPlot[{errsRelative,
                  If[Length[foundOutliersPos] > 0,
                    Part[errsRelative, foundOutliersPos], Nothing]},
                  GridLines -> {{}, {-th, th}}, PlotLabel -> "Relative errors",
                  Sequence @@ opts, Joined -> False, Filling -> Axis];
          ];

          Grid[{
            {gr11, gr12},
            {gr21, gr22},
            {Panel@ Pane[foundOutliers, ImageSize -> {1200, 100}, Scrollbars -> {True, True}], SpanFromLeft}
          }],
          {{dataId, "Financial", "Data:"}, {"Financial", "Temperature"}},
          {{rescaleQ, False, "Rescale?:"}, {True, False}},
          {{knots, 12, "Knots:"}, 2, 100, 1, Appearance -> "Open"},
          Delimiter,
          {{method, "ByResiduals", "Method:"}, {"ByRegressionQuantiles", "ByResiduals"}},
          Delimiter,
          {{topProb, 0.90, "Top probability:"}, 0, 1, Appearance -> "Open"},
          {{bottomProb, 0.1, "Bottom probability:"}, 0, 1, Appearance -> "Open"},
          Delimiter,
          {{oi, HampelIdentifierParameters, "Outlier identifier:"}, Map[Reverse, Normal@aNameToParamFinder]},
          {{re, True, "Relative errors:"}, {True, False}},
          {{thQ, False, "Threshold based?:"}, {True, False}},
          {{th, 0.05, "Threshold:"}, 0, 100, 0.005, Appearance -> "Open"}
        ]
      ]
    ];

End[]; (* `Private` *)

EndPackage[]