# WeibullReliabilityModel

- `WeibullReliabilityModel[dta]`
make a Weibull reliability model for given reliability (or production) data dta.

For a given list of numbers the function applies the Weibull distribution heuristic for modeling failures or production output.

The function returns a [ReliabilityModel](https://reference.wolfram.com/language/AntonAntonov/ReliabilityTools/ref/ReliabilityModel) object which can be used as an argument to [WeibullReliabilityPlot](https://reference.wolfram.com/language/AntonAntonov/ReliabilityTools/ref/WeibullReliabilityPlot).

The function finds the structural breaks in the data using the [Quantile Regression software monad ](https://resources.wolframcloud.com/PacletRepository/resources/AntonAntonov/MonadicQuantileRegression/)[QRMon](https://resources.wolframcloud.com/PacletRepository/resources/AntonAntonov/MonadicQuantileRegression/).

The function `WeibullReliabilityModel` takes the following options:

| option name | default value | description |
|---|---|---|
| "Breaks" | True | whether to compute structural breaks or not |
| "MedianRankMethod" | "Bernard" | one of "Bernard" or BetaDistribution |
| "NearestWithOutliers" | False | whether to use nearest neighbors with outliers or not (for finding local extrema) |
| "NumberOfProximityPoints" | Automatic | number of proximity points (for finding local extrema) |
| "Knots" | 10 | number of knots (for Quantile Regression) |
| Probabilities | 0.5 | probabilities (for Quantile Regression) |
| InterpolationOrder | 3 | interpolation order (for Quantile Regression) |
| Method | Automatic | minimization method (for Quantile Regression) |

See Also: [WeibullReliabilityPlot](https://reference.wolfram.com/language/AntonAntonov/ReliabilityTools/ref/WeibullReliabilityPlot)

----

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

Here is a corresponding plot using the model object:

```wl
WeibullReliabilityPlot[rmObj, PlotTheme -> "Production", ImageSize -> 500]
```

----

## More Examples

### Properties and Relations

The structural breaks in the data are found with a Quantile Regression monadic pipeline similar to this one:

```wl
PacletInstall["AntonAntonov/MonadicQuantileRegression"];
 Needs["AntonAntonov`MonadicQuantileRegression`"];
 qrObj = 
    QRMonUnit[rmObj["Data"]]==>
     QRMonFindStructuralBreaks[]==>
     QRMonPlotStructuralBreakSplits[ImageSize -> Small]==>
     QRMonTakeValue;
```

For more details on structural breaks finding see the [Wolfram Community](https://community.wolfram.com) post ["Finding all structural breaks in time series"](https://community.wolfram.com/t/finding-all-structural-breaks-in-time-series/14540).

### Possible Issues

With certain data the automatic determination of structural breaks has to be tweaked in order to get expected results. Here a list of production data numbers:

```wl
data = {1000, 1001, 1002, 1003, 1004, 1005, 1006, 1007, 1007, 1008, 
   1009, 1010, 1011, 1012, 1013, 1014, 1015, 1016, 1017, 1001, 982, 
   964, 946, 928, 911, 894, 877, 861, 845, 829, 813, 798, 783, 769, 
   754, 740, 726, 713, 700, 686, 674, 661, 649, 637, 625, 613, 602, 
   590, 579, 569, 558, 548, 537};
```

Here is the corresponding Weibull model with default option values:

```wl
mod1 = WeibullReliabilityModel[data]
```

Here is the corresponding plot:

```wl
WeibullReliabilityPlot[mod1, ImageSize -> 500]
```

![1lcjb6wv6ehsh](img/1lcjb6wv6ehsh.png)

Assume that the right-most break point (not shown on the plot above) is desired to found and plotted. One way is to remove the "bottom" points; another is to change the model- and plotting arguments. Consider the latter approach first. The structural breaks are computed with a larger number of knots and using a different method for finding proximity points ("NearestWithOutliers" is set to ![042oe562k1ezx](img/042oe562k1ezx.png)):

```wl
mod2 = WeibullReliabilityModel[data, "Knots" -> 20, "NearestWithOutliers" -> True]
```

Here is the corresponding plot with "BreakPoint" set to `Last`:

```wl
WeibullReliabilityPlot[mod2, "BreakPoint" -> Last, ImageSize -> 500]
```

By removing a few of the "bottom" points we get the expected result immediately:

```wl
mod3 = WeibullReliabilityModel[Drop[Sort@data, 3]];
WeibullReliabilityPlot[mod3, ImageSize -> 500]
```