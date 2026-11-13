# Outlier detection for financial time series

## Data

Here is a time series and its length:

```wl
tsData = FinancialData["GE", {{2019, 1, 1}, {2024, 1, 1}, "Week"}];
Length[tsData["Path"]]
```

```wl
(* 261 *)
```

Here is a date list plot with a vertical grid line that marks the training and testing split date:

```wl
splitDate = DateObject[{2022, 1, 1}];
pl = DateListPlot[tsData, AspectRatio -> 1/3, ImageSize -> Large, PlotLabel -> "GE stock", GridLines -> {{splitDate}, Automatic}]
```

### Via geometric nearest neighbors

Here we split the data into training and testing parts:

```wl
splitPos = Length@Select[tsData["Times"], # <= AbsoluteTime[splitDate] &];
{training, testing} = TakeDrop[QuantityMagnitude[tsData["Path"]], splitPos];
Length /@ {training, testing}
```

```wl
(* {157, 104} *)
```

Here we get the positions of the anomalies:

```wl
pos = GNNMonAnomalyDetection[training[[All, 2]], testing[[All, 2]], "AnomalyPositions", "NumberOfNearestNeighbors" -> 10];
Shallow[pos]
```

```wl
(* {1, 2, 3, 4, 5, 10, 11, 12, 13, 14, <<50>>} *)
```

Here is a plot of the time series and the found outliers:

```wl
Append[pl, Epilog -> {Red, PointSize[0.012], Point[testing[[pos]]]}]
```