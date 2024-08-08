(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["AntonAntonov`ReliabilityTools`"];

AnomalyDetectionInterface::usage = "Make an anomaly detection interface for given time series data.";
AnomalyFinder::usage = "Overall anomaly detection function.";
CrowAMSAAForecast::usage = "Faults forecast using the Crow AMSAA method.";
GNNMonAnomalyDetection::usage = "GNNMon anomaly detection.";
GNNMonAnomalyDetector::usage = "Makes GNNMon anomaly detector.";
GrowthTrackingPlot::usage = "Growth tracking plot for a given list of counts.";
IngestSeeqData::usage = "Ingesting Seeq data from different sources into different formats.";
IngestXMLData::usage = "Ingesting XML data into a dataset.";
SimpleAnomalyDetection::usage = "Anomaly detection with 1D outlier identifiers.";

PacletInstall["AntonAntonov/OutlierIdentifiers", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/MonadicGeometricNearestNeighbors", AllowVersionUpdate -> False];

Begin["`Private`"];

Needs["AntonAntonov`ReliabilityTools`AnomalyDetectionInterfaces`"];
Needs["AntonAntonov`ReliabilityTools`AnomalyFinders`"];
Needs["AntonAntonov`ReliabilityTools`CrowAMSAA`"];
Needs["AntonAntonov`ReliabilityTools`DataIngestors`"];
Needs["AntonAntonov`ReliabilityTools`GrowthTracking`"];

End[];
EndPackage[];