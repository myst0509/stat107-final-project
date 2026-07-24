# Weekly Cycles in US Urban Air Quality (2000-2023)

**Class:** UCR STAT 107

**Due date:** July 28, 2026

**Group members:** Soham

## Project description

This project uses 24 years of EPA air quality monitoring data to ask whether
the weekly rhythm of human activity is visible in urban air pollution. We
model daily nitrogen dioxide and ozone concentrations on the log scale,
controlling for month, year, and monitoring location, and compare weekends
against weekdays. We find that nitrogen dioxide falls roughly 22% on
weekends while ozone rises roughly 10%, a mirror-image pattern consistent
with ozone being chemically destroyed by the same vehicle emissions that
produce nitrogen dioxide.

## Repository contents

| Path | Description |
| --- | --- |
| `Analysis.qmd` | The Quarto report. Render this to reproduce the analysis. |
| `README.md` | This file. |
| `data/pollution_2000_2023.csv.gz` | The dataset, gzip-compressed. |
| `R/load_data.R` | Reading, validating, and cleaning the raw data. |
| `R/models.R` | Model fitting and coefficient extraction. |
| `R/plots.R` | All figure-building functions. |
| `R/summaries.R` | Data dictionary and summary tables. |

## How to reproduce

1. Install R and [Quarto](https://quarto.org/docs/get-started/).
2. Install the one required package:

   ```r
   install.packages("tidyverse")
   ```

3. From the repository root, render the report:

   ```bash
   quarto render Analysis.qmd
   ```

   Alternatively, open `Analysis.qmd` in RStudio and click **Render**.

The report reads the compressed CSV directly, so nothing needs to be
unzipped first.

## Data source

US Air Quality Metrics, 2000-2023, originally published by the US
Environmental Protection Agency and distributed via Kaggle.

- Kaggle: <https://www.kaggle.com/datasets/guslovesmath/us-pollution-data-200-to-2022>
- EPA source: <https://aqs.epa.gov/aqsweb/airdata/download_files.html>
