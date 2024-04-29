(* Mathematica Test File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(*BeginTestSection["IngestSeeqData-unit-tests.wlt"];*)

pObj = PacletObject["AntonAntonov/ReliabilityTools"];

(* Test 1 *)
VerificationTest[
  Quiet[Check[Needs["AntonAntonov`ReliabilityTools`"], $Failed]],
  Null,
  SameTest -> MatchQ,
  TestID -> "Load-ReliabilityTools"
];

(* Test 2 *)
VerificationTest[
  fileName = FileNameJoin[{pObj["Location"], "Resources", "sample.xml"}];
  FileExistsQ[fileName],
  True,
  TestID -> "Test-Sample-XML-file-found"
];

(* Test 3 *)
VerificationTest[
  IngestXMLData[3],
  $Failed,
  {IngestXMLData::noargs},
  TestID -> "Invocation-1"
];

(* Test 4 *)
VerificationTest[
  IngestXMLData[fileName],
  _Dataset,
  SameTest -> MatchQ,
  TestID -> "Invocation-2"
];

(*EndTestSection[]*)
