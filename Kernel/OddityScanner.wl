(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: Anton Antonov *)
(* :Date: 2024-03-15 *)


BeginPackage["AntonAntonov`ReliabilityTools`OddityScanner`"];

Begin["`Private`"];

Needs["AntonAntonov`ReliabilityTools`"];
Needs["AntonAntonov`OutlierIdentifiers`"];
Needs["AntonAntonov`MonadicGeometricNearestNeighbors`"];


(********************************************************************)
(* Shortcuts                                                        *)
(********************************************************************)

aNameToParamFinder = <|
  "Hampel" -> HampelIdentifierParameters,
  "SPLUS" -> SPLUSQuartileIdentifierParameters,
  "Quartile" -> QuartileIdentifierParameters|>;


(********************************************************************)
(* GetTrainingWindow                                                *)
(********************************************************************)

Clear[GetTrainingWindow];

GetTrainingWindow::nspec = "Do not know how to process the given specification.";

GetTrainingWindow[data_TemporalData, spec_] :=
    Block[{res},
      res = Switch[spec,
        Automatic, {Min@data["Times"], Min[data["Times"]] + (Max[data["Times"]] - Min[data["Times"]]) * 0.75},
        _?DateObjectQ, {Min@data["Times"], spec},
        {_?DateObjectQ, _?DateObjectQ}, Sort@spec,
        _?NumericQ, {Min@data["Times"], DateObject[spec]},
        {_?NumericQ, _?NumericQ}, Sort@spec,
        _, Message[GetTrainingWindow::nspec];
        GetTrainingWindow[data, Automatic];
      ];
      Map[If[DateObjectQ[#], AbsoluteTime[#], #] &, res]
    ];

GetTrainingWindow[data_ : {_?NumericQ ...}, spec_] :=
    Switch[spec,
      Automatic, {1, Round[Length[data] * 0.75]},
      _Integer, {1, spec},
      {_Integer, _Integer}, Sort@spec,
      _, Message[GetTrainingWindow::nspec];
    GetTrainingWindow[data, Automatic];
    ];


(********************************************************************)
(* SimpleAnomalyDetection                                           *)
(********************************************************************)

Clear[SimpleAnomalyDetection];

Options[SimpleAnomalyDetection] = {"OutlierIdentifier" -> "Hampel"};

SimpleAnomalyDetection[ts_TemporalData, window_ : Automatic, opts : OptionsPattern[]] :=
    Block[{split, trainingData, testingData},
      split = GetTrainingWindow[ts, window];
      trainingData = Pick[ts["Values"], Map[split[[1]] <= # <= split[[2]] &, ts["Times"]]];
      testingData = Pick[ts["Values"], Map[# <= split[[1]] || split[[2]] <= # &, ts["Times"]]];
      SimpleAnomalyDetection[{trainingData, testingData}, opts]
    ];

SimpleAnomalyDetection[vals : {_?NumberQ ..}, window_ : Automatic, opts : OptionsPattern[]] :=
    Block[{split},
      split = GetTrainingWindow[vals, window];
      SimpleAnomalyDetection[{Take[vals, split], Drop[vals, split]}, opts]
    ];

SimpleAnomalyDetection[{training_ : {_?NumberQ ..}, new_ : {_?NumberQ ..}}, opts : OptionsPattern[]] :=
    Block[{oiParamsFunc, params},

      oiParamsFunc = OptionValue[SimpleAnomalyDetection, "OutlierIdentifier"];
      oiParamsFunc = If[TrueQ[oiParamsFunc === Automatic], "Hampel", oiParamsFunc];
      oiParamsFunc = oiParamsFunc /. aNameToParamFinder;

      params = oiParamsFunc[training];

      <|"Positions" -> OutlierPosition[new, params &],
        "Outliers" -> OutlierIdentifier[new, params &]|>
    ];

(********************************************************************)
(* GNNMonAnomalyDetector                                            *)
(********************************************************************)

Clear[GNNMonAnomalyDetector];

Options[GNNMonAnomalyDetector] = {
  WindowSize -> 8,
  "OutlierIdentifier" -> "Hampel",
  DistanceFunction -> EuclideanDistance,
  "AggregationFunction" -> Mean,
  "NearestNeighbors" -> 10
};

GNNMonAnomalyDetector::nowsize = "The value of the option \"WindowSize\" is expected to be a integer greater than 1.";
GNNMonAnomalyDetector::nonns = "The value of the option \"NearestNeighbors\" is expected to be a positive integer.";

GNNMonAnomalyDetector[data : {_?NumberQ ..}, opts : OptionsPattern[]] :=
    Block[{windowSize, distFunc, oi, aggFunc, nns, trainingData},

      (*Window size*)
      windowSize = OptionValue[GNNMonAnomalyDetector, WindowSize];
      If[ TrueQ[windowSize === Automatic], distFunc = 10 ];

      If[ !(IntegerQ[windowSize] && windowSize > 1),
        Message[GNNMonAnomalyDetector::nowsize];
        Return[$Failed];
      ];

      (*Distance function*)
      distFunc = OptionValue[GNNMonAnomalyDetector, DistanceFunction];
      If[ TrueQ[distFunc === Automatic], distFunc = EuclideanDistance ];

      (*Outlier identifier*)
      oi = OptionValue[GNNMonAnomalyDetector, "OutlierIdentifier"];

      (*Aggregation function*)
      aggFunc = OptionValue[GNNMonAnomalyDetector, "AggregationFunction"];

      (*Nearest neighbors*)
      nns = OptionValue[GNNMonAnomalyDetector, "NearestNeighbors"];

      If[ !(IntegerQ[nns] && nns > 0),
        Message[GNNMonAnomalyDetector::nonns];
        Return[$Failed];
      ];

      (* Partition *)
      trainingData = Partition[data, windowSize, 1];

      (*Pipeline*)
      Fold[
        GNNMonBind,
        GNNMonUnit[trainingData],
        {
          GNNMonMakeNearestFunction[DistanceFunction -> distFunc],
          GNNMonComputeThresholds[nns, "AggregationFunction" -> aggFunc, "OutlierIdentifier" -> oi],
          GNNMonAddToContext[<|"windowSize" -> windowSize|>]
        }
      ]
    ];

(********************************************************************)
(* GNNMonAnomalyDetection                                           *)
(********************************************************************)

Clear[GNNMonAnomalyDetection];

GNNMonAnomalyDetection[training : {_?NumberQ ..}, opts : OptionsPattern[]] :=
    GNNMonAnomalyDetection[training, training, opts];

GNNMonAnomalyDetection[training : {_?NumberQ ..}, new : {_?NumberQ ..}, opts : OptionsPattern[]] :=
    Block[{oiParamsFunc, params},
      $Failed
    ];

GNNMonAnomalyDetection[gnnObj_GNNMon, data : {_?NumberQ ..}, opts : OptionsPattern[]] :=
    Block[{testingData},

      testingData = Partition[data, Fold[GNNMonBind, gnnObj, {GNNMonTakeContext}]["windowSize"], 1];

      Fold[
        GNNMonBind,
        gnnObj,
        {
          GNNMonFindAnomalies[testingData, "AnomalyPositions"],
          GNNMonTakeValue
        }
      ]
    ];

(********************************************************************)
(* OddityScanner                                                    *)
(********************************************************************)

Clear[OddityScanner];

Options[OddityScanner] = {Method -> Automatic, "OutlierIdentifier" -> "Hampel"};

OddityScanner[ts_TemporalData, window_ : Automatic, opts : OptionsPattern[]] :=
    Block[{split, trainingData, testingData},
      split = GetTrainingWindow[ts, window];
      trainingData = Pick[ts["Values"], Map[split[[1]] <= # <= split[[2]] &, ts["Times"]]];
      testingData = Pick[ts["Values"], Map[# <= split[[1]] || split[[2]] <= # &, ts["Times"]]];
      OddityScanner[{trainingData, testingData}, opts]
    ];

OddityScanner[vals : {_?NumberQ ..}, window_ : Automatic, opts : OptionsPattern[]] :=
    Block[{split},
      split = GetTrainingWindow[vals, window];
      OddityScanner[{Take[vals, split], Drop[vals, split]}, opts]
    ];

OddityScanner[{training : {_?NumberQ ..}, new : {_?NumberQ ..}}, opts : OptionsPattern[]] :=
    Block[{oiParamsFunc, params},
      $Failed
    ];

End[]; (* `Private` *)

EndPackage[]