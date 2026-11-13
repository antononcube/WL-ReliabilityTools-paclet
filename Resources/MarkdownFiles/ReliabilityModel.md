# ReliabilityModel

- `ReliabilityModel[data]`
reliability model object.

- `ReliabilityModel[data][prop]`
give the property prop of the model.

An object representing a reliability model.

Can be obtained with the function [WeibullReliabilityModel](https://reference.wolfram.com/language/AntonAntonov/ReliabilityTools/ref/WeibullReliabilityModel).

The following properties can be used to query the model object:

![1w37yz5mmiont](img/1w37yz5mmiont.png)

See Also:
[WeibullReliabilityModel](https://reference.wolfram.com/language/AntonAntonov/ReliabilityTools/ref/WeibullReliabilityModel) ▪ [WeibullReliabilityPlot](https://reference.wolfram.com/language/AntonAntonov/ReliabilityTools/ref/WeibullReliabilityPlot)

----

## Examples Initialization

```wl
Needs["AntonAntonov`ReliabilityTools`"]
```

----

## Basic Examples

```wl
obj["Properties"] // Sort
```

```wl
(* {"Association", "Breaks", "Data", "MaxBreakPoint", "OriginalData", "Properties", "WeibullParameters"} *)
```

Make a reliability model for production data (a list numbers):

```wl
failureTimes = {410, 780, 1120, 1400, 1680, 1970, 2300, 2750};
obj = WeibullReliabilityModel[failureTimes]
```


Get the fitting training data:

```wl
obj["Data"]
```

```wl
(* {{6.01616, -2.44172}, {6.65929, -1.48667}, {7.02108, -0.947354}, {7.24423, -0.543574}, {7.42655, -0.198574}, {7.58579, 0.126615}, {7.74066, 0.468505}, {7.91936, 0.910235}} *)
```

Get the original data:

```wl
obj["OriginalData"]
```

```wl
(* {410., 780., 1120., 1400., 1680., 1970., 2300., 2750.} *)
```

----

## More Examples

### Scope

Show all properties with which a `ReliabilityModel` object can be queried with:

```wl
obj["Properties"]
```

```wl
(* {"Association", "OriginalData", "Data", "WeibullParameters", "Breaks", "Properties", "MaxBreakPoint"} *)
```

Get the `Association` that has model's ingredients:

```wl
obj["Association"]
```

```wl
(* <|"OriginalData" -> {410., 780., 1120., 1400., 1680., 1970., 2300., 2750.}, "Data" -> {{6.01616, -2.44172}, {6.65929, -1.48667}, {7.02108, -0.947354}, {7.24423, -0.543574}, {7.42655, -0.198574}, {7.58579, 0.126615}, {7.74066, 0.468505}, {7.91936, 0.910235}}, "WeibullParameters" -> <|"a" -> -13.0316, "eta" -> 1803.34, "beta" -> 1.73815|>, "Breaks" -> <|{7.42655, 128.527} -> <|"Bottom" -> <|"a" -> -11.9384, "beta" -> 1.57341, "eta" -> 1973.5|>, "Top" -> <|"a" -> -16.9175, "beta" -> 2.24882, "eta" -> 1849.77|>|>|>|> *)
```

Get the max (or top) structural break point:

```wl
obj["MaxBreakPoint"]
```

```wl
(* {7.42655, -0.253345} *)
```

That point is shown in red here:

```wl
WeibullReliabilityPlot[obj, ImageSize -> 500]
```