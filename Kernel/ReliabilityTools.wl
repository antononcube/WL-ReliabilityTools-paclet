(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["AntonAntonov`ReliabilityTools`"];

CrowAMSAAForecast::usage = "Faults forecast using the Crow AMSAA method.";
GrowthTrackingPlot::usage = "Growth tracking plot for a given list of counts.";

Begin["`Private`"];

Needs["AntonAntonov`ReliabilityTools`CrowAMSAA`"];
Needs["AntonAntonov`ReliabilityTools`GrowthTracking`"];

End[];
EndPackage[];