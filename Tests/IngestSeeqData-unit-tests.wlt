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
  fileName = FileNameJoin[{pObj["Location"], "Resources", "SeeqFakeSample.xlsx"}];
  FileExistsQ[fileName],
  True,
  TestID -> "Test-Seeq-file-found"
];

(* Test 3 *)
VerificationTest[
  IngestSeeqData[3],
  $Failed,
  {IngestSeeqData::noargs},
  TestID -> "Invocation-1"
];

(*EndTestSection[]*)
