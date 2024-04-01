(* Mathematica Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: DataIngestors *)
(* :Context: DataIngestors` *)
(* :Author: antonov *)
(* :Date: 2024-03-31 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 14.0 *)
(* :Copyright: (c) 2024 antonov *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["AntonAntonov`ReliabilityTools`DataIngestors`"];

Begin["`Private`"];

Needs["AntonAntonov`ReliabilityTools`"];

(************************************************************)
(* IngestSeeqData                                           *)
(************************************************************)

ClearAll[IngestSeeqData];

IngestSeeqData::nofile = "The file \"`1`\" does not exist";
IngestSeeqData::nfrm = "Do not know how to process the 2nd argument (format specification.)";
IngestSeeqData::nargs = "The first argument is expected to be a file object or a file name. " <>
    "The second argument is expected to be a format spec, one of Automatic, All, \"Dataset\" or \"TimeSeries\".";

IngestSeeqData[fileName : (_String | _File), formArg_ : "Dataset"] :=
    Block[{form = formArg, lsSheets, dsData},

      If[MemberQ[{Automatic, TimeSeries, TemporalData}, form],
        form = "TimeSeries";
      ];

      If[!FileExistsQ[fileName],
        Message[IngestSeeqData::nofile, ToString[fileName]];
        Return[$Failed];
      ];

      lsSheets = Import[fileName];

      dsData =
          Dataset[Rest@Last[lsSheets]][All,
            AssociationThread[First@Last[lsSheets], #] &];

      Which[
        TrueQ[form == "TimeSeries"],
        TimeSeries[Normal@dsData[Values]],

        MemberQ[{Dataset, "Dataset"}, form],
        dsData,

        SameQ[All, form],
        <|
          "Information" -> KeyMap[StringReplace[StringTrim[#], ":" ~~ EndOfString -> ""] &, Association[Rule @@@ lsSheets[[1]]]],
          "Items" -> AssociationThread @@ lsSheets[[2]],
          "Statistics" -> AssociationThread @@ lsSheets[[3]],
          "Samples" -> dsData
        |>,

        True,
        Message[IngestSeeqData::nfrm];
        dsData
      ]
    ];

IngestSeeqData[___] := (
  Message[IngestSeeqData::nargs];
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

End[]; (* `Private` *)

EndPackage[]