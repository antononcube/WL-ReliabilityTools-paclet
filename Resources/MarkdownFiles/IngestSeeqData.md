IngestSeeqData

- `IngestSeeqData[file]`
ingests a Seeq XLSX file.

- `IngestSeeqData[file, frm]`
gives the result in the specified form frm.

The files are expected to be MicroSoft Excel XLSX files.

The format specifications are:

| option | description |
|---|---|
| "Dataset" | dataset of the samples sheet |
| "TimeSeries" | time series of the samples sheet |
| Automatic | same as "Dataset" |
| All | gives an association of all sheets |

