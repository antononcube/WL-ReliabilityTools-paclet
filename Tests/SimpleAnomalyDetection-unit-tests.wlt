(* Mathematica Test File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(*BeginTestSection["SimpleAnomalyDetection-unit-tests.wlt"];*)

tsData = TemporalData[TimeSeries,
  List[List[
    List[65.415771484375`, 65.13607788085938`, 65.83523559570312`,
      65.83523559570312`, 65.97503662109375`, 65.97503662109375`,
      66.04492950439453`, 66.2546157836914`, 66.2546157836914`,
      66.46427154541016`, 66.11483001708984`, 66.46427154541016`,
      66.74378204345703`, 66.81365966796875`, 66.67390441894531`,
      66.6040267944336`, 66.32450103759766`, 66.74378204345703`,
      66.67390441894531`, 66.74378204345703`, 66.81365966796875`,
      67.02325439453125`, 66.81365966796875`, 66.9533920288086`,
      66.6040267944336`, 67.02325439453125`, 66.81365966796875`,
      66.9533920288086`, 67.02325439453125`, 66.9533920288086`,
      66.74378204345703`, 66.2546157836914`, 66.3943862915039`,
      66.18472290039062`, 66.18472290039062`, 66.2546157836914`,
      66.2546157836914`, 66.18472290039062`, 66.11483001708984`,
      66.18472290039062`, 66.04492950439453`, 65.97503662109375`,
      65.83523559570312`, 65.76532745361328`, 65.48568725585938`,
      65.76532745361328`, 65.83523559570312`, 65.83523559570312`,
      65.6255111694336`, 65.90513610839844`, 65.6255111694336`,
      65.76532745361328`, 65.55560302734375`, 65.6255111694336`,
      65.83523559570312`, 65.06615447998047`, 65.06615447998047`,
      64.99622344970703`, 64.99622344970703`, 64.99622344970703`,
      64.64653778076172`, 64.78641510009766`, 64.50664520263672`,
      64.71647644042969`, 64.29679107666016`, 64.36674499511719`,
      64.0869140625`, 64.29679107666016`, 63.94698715209961`,
      63.73707580566406`, 63.59712219238281`, 63.59712219238281`,
      63.247196197509766`, 63.177207946777344`, 63.107215881347656`,
      63.31719207763672`, 62.967220306396484`, 62.68720245361328`,
      62.967220306396484`, 62.617191314697266`, 62.4771614074707`,
      62.40714645385742`, 62.54718017578125`, 62.4771614074707`,
      62.26710510253906`, 62.197078704833984`, 62.40714645385742`,
      62.40714645385742`, 62.40714645385742`, 62.54718017578125`,
      62.617191314697266`, 62.4771614074707`, 62.75720977783203`,
      62.40714645385742`, 62.617191314697266`, 62.4771614074707`,
      62.967220306396484`, 63.247196197509766`, 64.2268295288086`,
      65.20600891113281`, 66.53414916992188`, 67.7217788696289`,
      68.69932556152344`, 70.23455047607422`, 71.6292495727539`,
      72.46562957763672`, 73.78923034667969`, 74.06778717041016`,
      74.06778717041016`, 73.44099426269531`, 72.25656127929688`]],
    List[List[3.9187179`*^9, 3.9188169`*^9, 900.`]], 1,
    List["Continuous", 1], List["Discrete", 1], 1,
    List[Rule[ResamplingMethod,
      List["Interpolation", Rule[InterpolationOrder, 1]]],
      Rule[ValueDimensions, 1]]], False, 14.`];

(* Test 1 *)
VerificationTest[
  Quiet[Check[Needs["AntonAntonov`ReliabilityTools`"], $Failed]],
  Null,
  SameTest -> MatchQ,
  TestID -> "Load-ReliabilityTools"
];

(* Test 2 *)
VerificationTest[
  Head[tsData],
  TemporalData,
  SameTest -> MatchQ,
  TestID -> "Head-of-tsData"
];

(* Test 3 *)
VerificationTest[
  {trainingData, testingData} = TakeDrop[tsData["Path"], 95];
  Length /@ {trainingData, testingData},
  {95, 16},
  TestID -> "tsData-split"
];

(* Test 4 *)
VerificationTest[
  SimpleAnomalyDetection[trainingData, testingData, "Anomalies"],
  $Failed,
  {SimpleAnomalyDetection::noargs},
  TestID -> "Anomalies-nargs"
];

(* Test 5 *)
VerificationTest[
  SimpleAnomalyDetection[trainingData[[All, 2]], testingData[[All, 2]], "AnomalyCount"],
  _Integer,
  SameTest -> MatchQ,
  TestID -> "Anomaly-count-call-1"
];

(* Test 6 *)
VerificationTest[
  SimpleAnomalyDetection[{trainingData[[All, 2]], testingData[[All, 2]]}, "AnomalyCount"],
  $Failed,
  {SimpleAnomalyDetection::noargs},
  TestID -> "Anomaly-count-call-2"
];

(* Test 7 *)
VerificationTest[
  SimpleAnomalyDetection[trainingData[[All, 2]], testingData[[All, 2]], "AnomalyCount"],
  14,
  TestID -> "Anomaly-count-1"
];

(* Test 8 *)
VerificationTest[
  SimpleAnomalyDetection[trainingData[[All, 2]], testingData[[All, 2]], "Anomalies"],
  _List,
  SameTest -> MatchQ,
  TestID -> "Anomalies-call-1"
];

(* Test 9 *)
VerificationTest[
  SimpleAnomalyDetection[trainingData[[All, 2]], testingData[[All, 2]], "Anomalies"],
  List[62.4771614074707`, 62.967220306396484`, 63.247196197509766`,
    64.2268295288086`, 67.7217788696289`, 68.69932556152344`,
    70.23455047607422`, 71.6292495727539`, 72.46562957763672`,
    73.78923034667969`, 74.06778717041016`, 74.06778717041016`,
    73.44099426269531`, 72.25656127929688`],
  SameTest -> (Norm[#1 - #2, 1] < 0.002 &),
  TestID -> "Anomalies-1"
];

(*EndTestSection[]*)
