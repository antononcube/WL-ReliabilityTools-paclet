# GNNMonAnomalyDetector

- `GNNMonAnomalyDetector[vec]`
generates an anomaly detection GNNMon object for the list of numbers vec.

- `GNNMonAnomalyDetector[data, opts]`
generate the GNNMon object with the given options opts.

The anomaly detector object is based on the functionalities of ["MonadicGeometricNearestNeighbors"](https://resources.wolframcloud.com/PacletRepository/resources/AntonAntonov/MonadicGeometricNearestNeighbors/) paclet. 

"GNNMon" is short for "**G**eometric **N**earest **N**eighbors **Mon**ad".

The given list of numbers is partitioned into an overlapping list of lists using the specified window size.

The main `GNNMon` pipeline is built upon that list of lists.

The outlier identifiers used are implemented and described in the paclet ["OutlierIdentifiers"](https://resources.wolframcloud.com/PacletRepository/resources/AntonAntonov/OutlierIdentifiers/).

The following options can be given: 

| name | default | description |
|---|---|---|
| WindowSize | 8 | Length of the sub-lists derived by partitioning |
| "OutlierIdentifier" | "Hampel" | Outlier identifier |
| DistanceFunction | EuclideanDistance | Distance value to assume between any two specified points |
| "AggregationFunction" | Mean | Function to aggregate the distances of the nearest neighbors |
| "NumberOfNearestNeighbors" | 10 | Number of nearest neighbors |

See Also:
`GNNMonAnomalyDetection`

----

## Examples Initialization

```wl
Needs["AntonAntonov`ReliabilityTools`"]
```

----

## Basic Examples

Here is a time series and its length:

```wl
tsData = ...
```

Here is a date list plot with a vertical grid line that marks the training and testing split date:

```wl
splitDate = DateObject[{2024, 3, 7, 12, 0, 0}];
pl = DateListPlot[tsData, AspectRatio -> 1/3, ImageSize -> Large, PlotLabel -> "GE stock", GridLines -> {{splitDate}, Automatic}]
```

Here we split the data into training and testing parts:

```wl
splitPos = Length@Select[tsData["Times"], # <= AbsoluteTime[splitDate] &];
{training, testing} = TakeDrop[QuantityMagnitude[tsData["Path"]], splitPos];
Length /@ {training, testing}
```

```wl
(* {173, 115} *)
```

Here we make (train) an anomaly detector object:

```wl
obj = GNNMonAnomalyDetector[training[[All, 2]], "NumberOfNearestNeighbors" -> 10]
```

Here we get the positions of the anomalies:

```wl
pos = GNNMonAnomalyDetection[obj, testing, "AnomalyPositions"]
```

```wl
(* $Failed *)
```

Here is a plot of the time series and the found outliers:

```wl
Append[pl, Epilog -> {Red, PointSize[0.012], Point[testing[[pos]]]}]
```