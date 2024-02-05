(* ::Package:: *)

BeginPackage["AntonAntonov`ReliabilityTools`GrowthTracking`"];

Begin["`Private`"];

Needs["AntonAntonov`ReliabilityTools`"];

Clear[GrowthTrackingPlot];

GrowthTrackingPlot::args = "The first argument is expected to be a list of numbers.";

Options[GrowthTrackingPlot] = Options[LogLogPlot];

GrowthTrackingPlot[counts : {_?NumericQ ...}, opts : OptionsPattern[]] :=
    Block[{lsTimes, lsPath, aCAParams, degreeOfFreedom, lsExpectedCounts, lsObservedCounts},

      (* created paired data, for time...since this is interval data,
      use a simple index for months*)
      lsTimes = Range[Length[counts]];
      lsPath = Transpose[{lsTimes, counts}];

      (* these are the regressed parameters from the Crow-AMSAA function *)
      aCAParams = CrowAMSAAForecast[lsPath];

      degreeOfFreedom = Length[counts] - 2;

      (* created expected values with these parameters and accumulate for CA plot*)
      lsExpectedCounts = Transpose[{lsTimes, aCAParams["Lambda"] * lsTimes^aCAParams["Beta"]}];

      (* accumulate observed pse3 for CA plot *)
      lsObservedCounts = Transpose[{lsTimes, Accumulate[counts]}];

      ListLogLogPlot[{lsExpectedCounts, lsObservedCounts},
        opts,
        GridLines -> Full, PlotLegends -> {"Expected", "Observed"},
        Frame -> True,
        FrameLabel -> {"Cumulative time units", "Cumulative values"}]
    ];

GrowthTrackingPlot[___] := (Message[GrowthTrackingPlot::args];$Failed);

End[]; (* `Private` *)

EndPackage[]