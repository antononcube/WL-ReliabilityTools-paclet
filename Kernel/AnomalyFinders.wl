(* ::Package:: *)

(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: Anton Antonov *)
(* :Date: 2024-03-15 *)


BeginPackage["AntonAntonov`ReliabilityTools`AnomalyFinders`"];

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

GetTrainingWindow[data : {_?NumericQ ...}, spec_] :=
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

SimpleAnomalyDetection[ts_TemporalData, opts : OptionsPattern[]] :=
    SimpleAnomalyDetection[ts, Automatic, opts];

SimpleAnomalyDetection[ts_TemporalData, window_, opts : OptionsPattern[]] :=
    Block[{split, trainingData, testingData},
      split = GetTrainingWindow[ts, window];
      trainingData = Pick[ts["Values"], Map[split[[1]] <= # <= split[[2]] &, ts["Times"]]];
      testingData = Pick[ts["Values"], Map[# <= split[[1]] || split[[2]] <= # &, ts["Times"]]];
      SimpleAnomalyDetection[{trainingData, testingData}, opts]
    ];

SimpleAnomalyDetection[vals : {_?NumberQ ..},opts : OptionsPattern[]] :=
    SimpleAnomalyDetection[vals, Automatic, opts];

SimpleAnomalyDetection[vals : {_?NumberQ ..}, window_, opts : OptionsPattern[]] :=
    Block[{split},
      split = GetTrainingWindow[vals, window];
      SimpleAnomalyDetection[{Take[vals, split], Drop[vals, split]}, opts]
    ];

SimpleAnomalyDetection[{training : {_?NumberQ ..}, new : {_?NumberQ ..}}, opts : OptionsPattern[]] :=
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
  "NumberOfNearestNeighbors" -> 10
};

GNNMonAnomalyDetector::nowsize = "The value of the option \"WindowSize\" is expected to be a integer greater than 1.";
GNNMonAnomalyDetector::nonns = "The value of the option \"NumberOfNearestNeighbors\" is expected to be a positive integer.";

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
      If[ TrueQ[oi === Automatic], oi = "Hampel" ];

      (*Aggregation function*)
      aggFunc = OptionValue[GNNMonAnomalyDetector, "AggregationFunction"];

      (*Nearest neighbors*)
      nns = OptionValue[GNNMonAnomalyDetector, "NumberOfNearestNeighbors"];

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
(* AnomalyPropSpecQ                                                 *)
(********************************************************************)

lsKnownProperties = {"Anomalies", "AnomalyCount", "AnomalyBooleanList", "AnomalyPositions", "NonAnomalies"};

Clear[KnownAnomalyPropQ];
KnownAnomalyPropQ[s_String] := MemberQ[lsKnownProperties, s];
KnownAnomalyPropQ[_] := False;

Clear[AnomalyPropSpecQ];
AnomalyPropSpecQ[x_String] := KnownAnomalyPropQ[x];
AnomalyPropSpecQ[Automatic] := True;
AnomalyPropSpecQ[x_List] := And @@ Map[KnownAnomalyPropQ, x];
AnomalyPropSpecQ[_] := False;

(********************************************************************)
(* GNNMonAnomalyDetection                                           *)
(********************************************************************)

Clear[GNNMonAnomalyDetection];

GNNMonAnomalyDetection::noprop =
    "Do not know how to process the given property spec. " <>
    "The property spec can be Automatic, one on the strings `1`, or a list of those strings.";

GNNMonAnomalyDetection::noargs =
    "The first argument is expected to be a list of numbers or a GNNMon object. " <>
        "The second argument is expected to be a list of numbers or a property spec. " <>
        "The third argument is expected to be a property spec.";

GNNMonAnomalyDetection[training : {_?NumericQ ..}, opts : OptionsPattern[]] :=
    GNNMonAnomalyDetection[training, "Anomalies", opts];

GNNMonAnomalyDetection[training : {_?NumericQ ..}, prop_?AnomalyPropSpecQ, opts : OptionsPattern[]] :=
    GNNMonAnomalyDetection[training, training, prop, opts];

GNNMonAnomalyDetection[training : {_?NumericQ ..}, new : {_?NumericQ ..}, opts : OptionsPattern[]] :=
    GNNMonAnomalyDetection[training, new, "Anomalies", opts];

GNNMonAnomalyDetection[training : {_?NumericQ ..}, new : {_?NumericQ ..}, prop_, opts : OptionsPattern[]] :=
    Block[{gnnObj},
      gnnObj = GNNMonAnomalyDetector[training, opts];
      GNNMonAnomalyDetection[gnnObj, new, prop, opts]
    ];

GNNMonAnomalyDetection[gnnObj_GNNMon, data : {_?NumericQ ..}, opts : OptionsPattern[]] :=
    GNNMonAnomalyDetection[gnnObj, data, "Anomalies", opts];

GNNMonAnomalyDetection[gnnObj_GNNMon, data : {_?NumericQ ..}, propArg_, opts : OptionsPattern[]] :=
    Block[{prop = propArg, testingData, resPositions, res, dr},

      If[AtomQ[prop], prop = {prop}];
      If[!AnomalyPropSpecQ[prop],
        Message[GNNMonAnomalyDetection::noprop, lsKnownProperties];
        Return[$Failed];
      ];

      testingData = Partition[data, Fold[GNNMonBind, gnnObj, {GNNMonTakeContext}]["windowSize"], 1];

      resPositions = Fold[
        GNNMonBind,
        gnnObj,
        {
          GNNMonFindAnomalies[testingData, "AnomalyPositions"],
          GNNMonTakeValue
        }
      ];

      res =
          Switch[#,
            Automatic, testingData[[resPositions]],
            "Anomalies", testingData[[resPositions]],
            "AnomalyCount", Length@resPositions,
            "AnomalyBooleanList", dr = Dispatch[Append[Thread[resPositions -> True], _Integer -> False]]; Range[1, Length@testingData] /. dr,
            "AnomalyPositions", resPositions,
            "NonAnomalies", Complement[Range[1, Length@testingData], resPositions],
            _, Message[GNNMonAnomalyDetection::noprop, lsKnownProperties]; testingData
          ]& /@ prop;

      If[AtomQ[propArg], res[[1]], res]
    ];

(*GNNMonAnomalyDetection[___] :=*)
(*    (Message[GNNMonAnomalyDetection::noargs]; $Failed);*)

(********************************************************************)
(* AnomalyFinder                                                    *)
(********************************************************************)

Clear[AnomalyFinder];

Options[AnomalyFinder] = {Method -> Automatic, "OutlierIdentifier" -> "Hampel"};

AnomalyFinder[ts_TemporalData, window_, opts : OptionsPattern[]] :=
    Block[{split, trainingData, testingData},
      split = GetTrainingWindow[ts, window];
      trainingData = Pick[ts["Values"], Map[split[[1]] <= # <= split[[2]] &, ts["Times"]]];
      testingData = Pick[ts["Values"], Map[# <= split[[1]] || split[[2]] <= # &, ts["Times"]]];
      AnomalyFinder[{trainingData, testingData}, opts]
    ];

AnomalyFinder[vals : {_?NumberQ ..}, window_, opts : OptionsPattern[]] :=
    Block[{split},
      split = GetTrainingWindow[vals, window];
      AnomalyFinder[{Take[vals, split], Drop[vals, split]}, opts]
    ];

AnomalyFinder[{training : {_?NumberQ ..}, new : {_?NumberQ ..}}, opts : OptionsPattern[]] :=
    Block[{method},

      method=OptionValue[AnomalyFinder, Method];
      If[SameQ[method, Automatic], method = "GNNMonAnomalyDetection"];

      Which[
        MemberQ[{"GNNMonAnomalyDetection", "GNNMon", "NearestNeighbors", Nearest, Automatic}, method],
        GNNMonAnomalyDetection[training, new, opts],

        MemberQ[{"SimpleAnomalyDetection", "1D", "1DOutliers"}, method],
        SimpleAnomalyDetection[training, new, opts],

        True, $Failed
      ]
    ];

End[]; (* `Private` *)

EndPackage[]
