# Weekly Cycles in US Urban Air Quality (2000-2023)

**Class:** UCR STAT 107

**Due date:** July 28, 2026

**Group members:** Soham (solo)

## Project description

This project uses 24 years of EPA air quality monitoring data to ask whether
the weekly rhythm of human activity shows up in urban air pollution. I model
daily nitrogen dioxide and ozone concentrations on the log scale, controlling
for month, year, and monitoring location, and compare weekends against
weekdays. Nitrogen dioxide falls about 22% on weekends while ozone rises about
10%, a mirror-image pattern that lines up with ozone being chemically destroyed
by the same vehicle emissions that produce nitrogen dioxide.

## Notes for opening the project

- Open `Analysis.qmd` and render it to reproduce the full report. The helper
  functions live in the `R/` folder and are pulled in automatically with
  `source()`, so there is nothing to run by hand first.
- The dataset is stored compressed at `data/pollution_2000_2023.csv.gz`. The
  code reads the compressed file directly, so it does not need to be unzipped.
- The only package required is `tidyverse`.
- A full render takes roughly 30 seconds.

## Data source

US Air Quality Metrics, 2000-2023, originally published by the US Environmental
Protection Agency and distributed via Kaggle.

- Kaggle: <https://www.kaggle.com/datasets/guslovesmath/us-pollution-data-200-to-2022>
- EPA source: <https://aqs.epa.gov/aqsweb/airdata/download_files.html>
