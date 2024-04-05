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
IngestSeeqData::usage = "Ingesting Seeq data from different sources into different formats.";
IngestXMLData::usage = "Ingesting XML data into a dataset.";

PacletInstall["AntonAntonov/OutlierIdentifiers", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/MonadicGeometricNearestNeighbors", AllowVersionUpdate -> False];

Begin["`Private`"];

Needs["AntonAntonov`ReliabilityTools`CrowAMSAA`"];
Needs["AntonAntonov`ReliabilityTools`GrowthTracking`"];
Needs["AntonAntonov`ReliabilityTools`AnomalyFinders`"];
Needs["AntonAntonov`ReliabilityTools`DataIngestors`"];

End[];
EndPackage[];