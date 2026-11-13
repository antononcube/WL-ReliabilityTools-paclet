# AnomalyDetectionInterface

- `AnomalyDetectionInterface[ats,opts___]`
es an anomaly detection interface for a given association of time series ats.

Gives an interactive interface object based on `Manipulate`.

See Also: `AnomalyFinder`, `GNNAnomalyDetector`

---

## Examples Initialization 

```wl
Needs["AntonAntonov`ReliabilityTools`"]
```

---

## Basic Examples

Here is a couple of random time series data:

```wl
SeedRandom[33];
aTS = AssociationThread[{"first", "second"}, {TimeSeries[RandomReal[{10, 30}, 300]], TimeSeries[RandomReal[10, 300]]}]
```

Make an anomaly detection interactive interface for that data:

```wl
AnomalyDetectionInterface[aTS, ImageSize -> 300]
```