# AnomalyFinder

- `AnomalyFinder[data]`
finds anomalies in data.

- `AnomalyFinder[trainingData,newData]`
finds anomalies in newData based on observations from trainingData.

- `AnomalyFinder[trainingData,newData,props]`
gives the specified properties.

[AnomalyFinder](https://reference.wolfram.com/language/AntonAntonov/ReliabilityTools/ref/AnomalyFinder) is an overall anomaly detection function -- it is a general interface to several algorithms invoked via its `Method` option. 

The following options can be given: 

| option name | default value | description |
|---|---|---|
| Method | Automatic | Method is an option for various algorithm-intensive functions that specifies what internal methods they should use. |
| "OutlierIdentifier" | "Hampel" | Outlier identifier |

Possible properties include:

|  |  |
|---|---|
| "Anomalies" | examples that are considered anomalous |
| "AnomalyCount" | number of examples that are considered anomalous |
| "AnomalyBooleanList" | Boolean values indicating whether examples are anomalous |
| "AnomalyPositions" | list of anomaly positions |
| "NonAnomalies" | examples that are considered nonanomalous |

See Also

`GNNMonAnomalyDetector` 

---

## Examples Initialization

```wl
Needs["AntonAntonov`ReliabilityTools`"]
```

---

## Basic examples

Here is a time series and its length:

```wl
tsData = TimeSeries[...]
```

Here is a date list plot with a vertical grid line that marks the training and testing split date:

```wl
splitDate = DateObject[{2024, 3, 7, 12, 0, 0}];
pl = DateListPlot[tsData, AspectRatio -> 1/3, ImageSize -> Large, PlotLabel -> "Failures", GridLines -> {{splitDate}, Automatic}]
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
AnomalyFinder[{training[[All, 2]], testing[[All, 2]]}, Method -> {Automatic, "OutlierIdentifier" -> "Quartile"}]
```

```wl
AnomalyFinder[{training[[All, 2]], testing[[All, 2]]}, "OutlierIdentifier" -> "Quartile"]
```

```wl
pos = AnomalyFinder[{training[[All, 2]], testing[[All, 2]]}, "AnomalyPositions", Method -> {"GNNMon", "NumberOfNearestNeighbors" -> 10, "OutlierIdentifier" -> "Quartile"}];
 Shallow[pos]
```

```wl
(* {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, <<98>>} *)
```

Here is a plot of the time series and the found outliers:

```wl
Append[pl, Epilog -> {Red, PointSize[0.012], Point[testing[[pos]]]}]
```

---

## More Examples

The Method option takes the values "SimpleAnomalyDetection" (or just "Simple") and "GNNMonAnomalyDetection" (or just "GNNMon"). Here is an example using the simple method (based on one-dimensional outlier identification):

```wl
pos = AnomalyFinder[{training[[All, 2]], testing[[All, 2]]}, "AnomalyPositions", Method -> {"Simple", "OutlierIdentifier" -> "Hampel"}];
 Shallow[pos]
```

```wl
(* {1, 2, 7, 8, 9, 10, 11, 12, 13, 14, <<101>>} *)
```

```wl
Append[pl, Epilog -> {Red, PointSize[0.012], Point[testing[[pos]]]}]
```

### "OutlierIdentifier"

Both methods take the option "OutlierIdentifier". The value of that option is expected to be `Automatic`, "Hampel", "SPLUSQuartile", or "Quartile".