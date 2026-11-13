# WeibullReliabilityPlot

- `WeibullReliabilityPlot[data]`
makes Weibull (inverted) reliability plot for given `data`.
   
- `WeibullReliabilityPlot[rbObj]`
plot for a [ReliabilityModel](https://reference.wolfram.com/language/AntonAntonov/ReliabilityTools/ref/ReliabilityModel) object `rbObj`.

The following options can be given: 

| option name | default value | description |
|---|---|---|
| "BottomCurveColor" | Orange | color of the bottom curve of the structural break |
| "Breakpoint" | Automatic | which break point to use |
| "BreakPointColor" | Red | color of the break point |
| "FittedCurveColor" | None | color of the overall fitted curve |
| "PointColor" | RGBColor[.1, .3, .7] | color of the data points |
| PointSize | 0.018 | size of the data points |
| PlotTheme | "Production" | plot theme, one of "Production" or "Failure" |
| "Subtitle" | Weibull Reliability Probability Plot | subtitle of the plot |
| "TopCurveColor" | StandardBlue | color of the top curve of the structural break |
| AxesLabel | Automatic | AxesLabel is an option for graphics functions that specifies labels for axes. |

If any of the options "BottomCurveColor", "BreakPointColor", "FittedCurveColor", or "TopCurveColor" is set to `None` then the corresponding graphics element is not shown.

See Also:

[ReliabilityModel](https://reference.wolfram.com/language/AntonAntonov/ReliabilityTools/ref/ReliabilityModel) ▪ [WeibullReliabilityModel](https://reference.wolfram.com/language/AntonAntonov/ReliabilityTools/ref/WeibullReliabilityModel)

---

## Examples Initialization

```wl
Needs["AntonAntonov`ReliabilityTools`"]
```

----

## Basic Examples

Here is production output data:

```wl
production = {1135, 1249, 1267, 1240, 1268, 1092, 1280, 1206, 1247, 
   1141, 894, 1261, 1223, 1211, 1262, 1210, 1024, 1243, 1016, 1267, 
   1233, 1190, 1253, 1127, 1282, 1199, 1134, 1240, 1261, 1259, 1087, 
   1300, 1239, 1180, 1225, 1164, 1212, 1229, 1227, 1296, 1235, 1183, 
   1224, 1211, 1169, 1192, 1087, 1257, 1080, 1227, 1093, 1228, 1256, 
   866, 1117, 1225, 1188, 1256, 1219, 1281, 1144, 1257, 1202, 1076, 
   1150, 1278, 1284, 812, 1262, 1130, 1282, 1213, 953, 1250, 1256, 
   1255, 1240, 1246, 1084, 1034, 1100, 1244, 1152, 1253, 1278, 1238, 
   1239, 1248, 1232, 1248, 1244, 1067, 1169, 1190, 1175, 1215, 1257, 
   1156, 1251, 1248};
```

Here a reliability model object is created:

```wl
rmObj = WeibullReliabilityModel[production]
```

![0h3l242mmaly3](img/0h3l242mmaly3.png)

Here is a corresponding plot using the model object:

```wl
WeibullReliabilityPlot[rmObj, ImageSize -> 500]
```

----

## More Examples

### Scope

Show the data points without any of model elements:

```wl
WeibullReliabilityPlot[rmObj, "FittedCurveColor" -> None, "TopCurveColor" -> None, "BottomCurveColor" -> None, "BreakPointColor" -> None, ImageSize -> 500]
```

### Options

Which break to show on the plot is controlled with the option "BreakPoint", which takes as values a break point index (a positive integer), or one of `Automatic`, `Max`, `First`, `Last`:

```wl
Table[
   WeibullReliabilityPlot[rmObj, "BreakPoint" -> bp, ImageSize -> Medium, 
     ImagePadding -> {{100, 10}, {10, 70}}, PlotLabel -> Row[Style[#, StandardBlue, Bold, FontSize -> 16] & /@ {"Break point spec:", Spacer[10], bp}]] 
   , {bp, {1, 2, Automatic, Max, First, Last}}]
```

#### "PointColor"

The color of the data points is controlled with the option "PointColor":

```wl
WeibullReliabilityPlot[rmObj, "PointColor" -> GrayLevel[0.4], ImageSize -> 500]
```

The color of the *overall* fitted curve (line) is controlled with the option "FittedCurveColor":

```wl
WeibullReliabilityPlot[rmObj, "FittedCurveColor" -> GrayLevel[0.4], ImageSize -> 500]
```

#### "FittedCurveColor"

By default "FittedCurveColor" is set to ![1objc7772cf3p](img/1objc7772cf3p.png), which means the overall fitted curve is not plotted.

The color of the data points is controlled with the option ![1dmtxxet5e06a](img/1dmtxxet5e06a.png):

```wl
WeibullReliabilityPlot[rmObj, PointSize -> 0.04, ImageSize -> 500]
```

#### "Subtitle"

The plot subtitle can be specified with the option "Subtitle":

```wl
WeibullReliabilityPlot[rmObj, "Subtitle" -> Style["Example Subtitle", Bold, Pink, FontSize -> 20], ImageSize -> 500]
```

#### AxesLabel

The plot axes labels can be specified with the option `AxesLabel`:

```wl
WeibullReliabilityPlot[rmObj, AxesLabel -> {Style["horizontal", Pink], Style["vertical", Pink]}, ImageSize -> 500]
```

The values of the `AxesLabel` option override the values of the option `PlotTheme`. (For this plot function.)