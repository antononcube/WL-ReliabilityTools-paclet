# CrowAMSAAForecast

- `CrowAMSAAForecast[data]`
	 forecast faults from data using the Crow AMSAA method.

---

## Examples Initialization

```wl
Needs["AntonAntonov`ReliabilityTools`"]
```

---

## Basic Examples

Here is faults data:

```wl
data = {0.7, 2.4, 8.2, 11.3, 12.1, 17.6, 18.9, 20.4, 21.9, 23.2, 25.7,42.8, 48.0, 56.3, 65.1};
 ListPlot[data, PlotRange -> All, PlotTheme -> "Detailed"]
```

Here the Crow AMSAA method is applied:

```wl
CrowAMSAAForecast[data]
```

```wl
(* <|"Beta" -> 2.32683, "Lambda" -> 0.687065, "ChiSquared" -> 12.0623|> *)
```
