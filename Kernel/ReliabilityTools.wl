(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["AntonAntonov`ReliabilityTools`"];

CrowAMSAAForecast::usage = "Faults forecast using the Crow AMSAA method.";
GrowthTrackingPlot::usage = "Growth tracking plot for a given list of counts.";
GNNMonAnomalyDetector::usage = "Makes GNNMon anomaly detector.";
GNNMonAnomalyDetection::usage = "GNNMon anomaly detection.";
SimpleAnomalyDetection::usage = "Anomaly detection with 1D outlier identifiers.";
AnomalyFinder::usage = "Overall anomaly detection function.";

Begin["`Private`"];

Needs["AntonAntonov`ReliabilityTools`CrowAMSAA`"];
Needs["AntonAntonov`ReliabilityTools`GrowthTracking`"];
Needs["AntonAntonov`ReliabilityTools`AnomalyFinders`"];

End[];
EndPackage[];