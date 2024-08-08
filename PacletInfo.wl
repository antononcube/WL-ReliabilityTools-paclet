(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "AntonAntonov/ReliabilityTools",
    "Description" -> "Reliability-related algorithms and tools",
    "Creator" -> "Anton Antonov",
    "License" -> "MIT",
    "PublisherID" -> "AntonAntonov",
    "Version" -> "1.0.1",
    "WolframVersion" -> "13+",
    "PrimaryContext" -> "AntonAntonov`ReliabilityTools`",
    "DocumentationURL" -> "https://resources.wolframcloud.com/PacletRepository/resources",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {"AntonAntonov`ReliabilityTools`"},
        "Symbols" -> {
          "AntonAntonov`ReliabilityTools`AnomalyDetectionInterface",
          "AntonAntonov`ReliabilityTools`AnomalyFinder",
          "AntonAntonov`ReliabilityTools`CrowAMSAAForecast",
          "AntonAntonov`ReliabilityTools`GNNMonAnomalyDetection",
          "AntonAntonov`ReliabilityTools`GNNMonAnomalyDetector",
          "AntonAntonov`ReliabilityTools`GetTrainingWindow",
          "AntonAntonov`ReliabilityTools`GrowthTrackingPlot",
          "AntonAntonov`ReliabilityTools`IngestSeeqData",
          "AntonAntonov`ReliabilityTools`IngestXMLData",
          "AntonAntonov`ReliabilityTools`SimpleAnomalyDetection"
        }
      },
      {
        "Documentation",
        "Root" -> "Documentation",
        "Language" -> "English"
      },
      {
        "Resource",
        "Root" -> "Resources",
        "Resources" ->
            {
              {"Resource", "sample.xml"},
              {"Resource", "SeeqFakeSample.xlsx"}
            }
      }
    }
  |>
]
