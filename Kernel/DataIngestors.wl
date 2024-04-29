(* Mathematica Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: DataIngestors *)
(* :Context: DataIngestors` *)
(* :Author: Anton Antonov *)
(* :Date: 2024-03-31 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 14.0 *)
(* :Copyright: (c) 2024 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["AntonAntonov`ReliabilityTools`DataIngestors`"];

Begin["`Private`"];

Needs["AntonAntonov`ReliabilityTools`"];

(************************************************************)
(* IngestSeeqData                                           *)
(************************************************************)

ClearAll[IngestSeeqData];

SyntaxInformation[IngestXMLData] = { "ArgumentsPattern" -> { _, _., _. } };

IngestSeeqData::nofile = "The file \"`1`\" does not exist";
IngestSeeqData::nofrm = "Do not know how to process the 2nd argument (format specification.)";
IngestSeeqData::noargs = "The first argument is expected to be a file object or a file name. " <>
    "The second argument is expected to be a format spec, one of Automatic, All, \"Dataset\" or \"TimeSeries\". " <>
        "The third argument is expected to be file type spec (for Import).";

IngestSeeqData[fileName : (_String | _File), formArg_ : "Dataset", fileType_ : Automatic] :=
    Block[{form = formArg, lsSheets, dsData, infoSheet},

      If[MemberQ[{Automatic, TimeSeries, TemporalData}, form],
        form = "TimeSeries";
      ];

      If[!FileExistsQ[fileName],
        Message[IngestSeeqData::nofile, ToString[fileName]];
        Return[$Failed];
      ];

      lsSheets = Import[fileName, fileType];

      dsData = Last[lsSheets];
      (* dsData = DeleteCases[dsData,List["",""]];*)
      dsData = Dataset[Rest@dsData][All, AssociationThread[First@dsData, #] &];

      infoSheet = DeleteCases[Map[DeleteCases[#,""]&,lsSheets[[1]]],{}];

      Which[
        TrueQ[form == "TimeSeries"],
        TimeSeries[Normal@dsData[Values]],

        MemberQ[{Dataset, "Dataset"}, form],
        dsData,

        SameQ[All, form],
        <|
          "Information" -> KeyMap[StringReplace[StringTrim[#], ":" ~~ EndOfString -> ""] &, Association[Rule @@@ infoSheet]],
          "Items" -> AssociationThread @@ lsSheets[[2]],
          "Statistics" -> AssociationThread @@ lsSheets[[3]],
          "Samples" -> dsData
        |>,

        True,
        Message[IngestSeeqData::nofrm];
        dsData
      ]
    ];

IngestSeeqData[___] := (
  Message[IngestSeeqData::noargs];
  $Failed
);

(************************************************************)
(* IngestXMLData                                            *)
(************************************************************)

Clear[GetXMLRecord];
GetXMLRecord[xmlData_] :=
    Block[{res},
      res = Cases[xmlData, XMLElement[{_, "properties"}, __], Infinity];
      If[Length[res] > 0,
        Flatten@Map[Association@Cases[#, XMLElement[{x_String /; StringEndsQ[x, "dataservices"], fname_String}, {}, {d_}] :> (fname -> d), Infinity] &, res[[All, 3]]],
        {}
      ]
    ];

(*---------------------------------------------------------*)

Clear[IngestXMLData];

SyntaxInformation[IngestXMLData] = { "ArgumentsPattern" -> { _ } };

IngestXMLData::noargs = "The first argument is expected to be a file object, a file name, or a URL.";

IngestXMLData[fileName : (_?StringQ | _File | _URL)] :=
    Module[{data},
      data = Import[fileName, "XML"];
      IngestXMLData[data]
    ];

IngestXMLData[data_] :=
    Module[{lsRecords, dsRecords, recLength},

      lsRecords = Flatten[GetXMLRecord /@ Cases[data, XMLElement["entry", __], Infinity]];
      
      recLength = First @ Flatten @ TakeLargestBy[Tally[Length /@ lsRecords], Last, 1];
      dsRecords = Dataset[Select[lsRecords, Length[#] == recLength &]];

      dsRecords[All, Join[ToExpression /@ KeyDrop[#, "timestamp"], <|"DateObject" -> DateObject[#timestamp]|>]&]

    ] /; MatchQ[data, XMLObject[__][___]];

IngestXMLData[___] := (
  Message[IngestXMLData::noargs];
  $Failed
);

End[]; (* `Private` *)

EndPackage[]