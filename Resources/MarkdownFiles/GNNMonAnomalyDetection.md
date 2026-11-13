# GNNMonAnomalyDetection

- `GNNMonAnomalyDetection[vec]`
anomaly detection using an geometric nearest neighbors for the list of numbers vec.

- `GNNMonAnomalyDetection[gnnObj,vec, opts]`
anomaly detection using an GNNMon object for the list of numbers vec.
   
- `GNNMonAnomalyDetection[training,new, opts]`
generate the GNNMon object with the given options opts.

[GNNMonAnomalyDetection](https://reference.wolfram.com/language/AntonAntonov/ReliabilityTools/ref/GNNMonAnomalyDetection) finds anomalies with using Geometric Nearest Neighbors Monad (GNNMon).


See Also `GNNMonAnomalyDetector`

---

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

Here we get the positions of the anomalies:

```wl
pos = GNNMonAnomalyDetection[training[[All, 2]], testing[[All, 2]], "AnomalyPositions", "NumberOfNearestNeighbors" -> 10];
Shallow[pos]
```

```wl
(* {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, <<98<<} *)
```

Here is a plot of the time series and the found outliers:

```wl
Append[pl, Epilog -> {Red, PointSize[0.012], Point[testing[[pos]]]}]
```