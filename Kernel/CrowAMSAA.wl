(* ::Package:: *)

BeginPackage["AntonAntonov`ReliabilityTools`CrowAMSAA`"];

Begin["`Private`"];

Needs["AntonAntonov`ReliabilityTools`"];

Clear[numPart];
numPart[0. | 0, _] := 0;
numPart[T_, beta_] := T^beta * Log[T];

Clear[iFaults];
iFaults[list_List][beta_] :=
    With[{n = list[[All, 2]], T = Prepend[list[[All, 1]] - list[[1 , 1]] + 1, 0]},
      N[Total[n * (((numPart[#, beta] & /@ Rest[T]) - (numPart[#, beta] & /@ Most[T])) / (Rest[T]^beta - Most[T]^beta ) - Log[Last[T]])]]
    ];

Clear[faults];
faults[list_List] :=
    faults[list] =
        Block[{b, sol},
          sol = Flatten @ FindRoot[iFaults[list][b], {b, 1}];
          b /. sol
        ];

Clear[lambda];
lambda[list_List] := Total[list[[All, 2]]] / (list[[-1, 1]] - list[[1 , 1]] + 1) ^ faults[list];

Clear[chiSq];
chiSq[list_List] :=
    Module[{n = list[[All, 2]], T = Prepend[list[[All, 1]] - list[[1 , 1]] + 1, 0], e,
      lambda = lambda[list],
      betta = faults[list],
      chi},
      e = lambda * Differences[T ^ betta];
      chi = Total[(n - e)^2 / e];
      chi
    ];

Clear[CrowAMSAAForecast];

SyntaxInformation[CrowAMSAAForecast] = { "ArgumentsPattern" -> { _ } };

CrowAMSAAForecast[list : { _?NumericQ .. }] :=
    CrowAMSAAForecast[Transpose[{Range[Length[list]], list}]];

CrowAMSAAForecast[list_?MatrixQ] :=
    <|
      "Beta" -> faults[list],
      "Lambda" -> lambda[list],
      "ChiSquared" -> chiSq[list]
    |> /; MatrixQ[list, NumericQ] && Dimensions[list][[2]] == 2;

End[];
EndPackage[];
