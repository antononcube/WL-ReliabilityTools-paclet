(* Mathematica Test File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(*BeginTestSection["IngestSeeqData-unit-tests.wlt"];*)


(* Test 1 *)
VerificationTest[
  Quiet[Check[Needs["AntonAntonov`ReliabilityTools`"], $Failed]],
  Null,
  SameTest -> MatchQ,
  TestID -> "Load-ReliabilityTools"
];

(* Test 2 *)
VerificationTest[
  IngestSeeqData[3],
  $Failed,
  {IngestSeeqData::noargs},
  TestID -> "Invocation-1"
];

(*EndTestSection[]*)
