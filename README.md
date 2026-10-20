# Statistical Evaluation of the AEX Stock Index

<p align="center">
  <img alt="R" src="https://img.shields.io/badge/Language-R-276DC3?style=for-the-badge&logo=r&logoColor=white">
</p>

This project investigate the **AEX** stock index for the course Mathematical Statistics 1 using R.

## Repository Structure

```text
.
├── data/
│   ├── AEX_1995_2024.csv                         # AEX dataset
├── figures/
├── Import.R                                      # Data import and initial validation
├── Exploratory_data_analysis.R                   # Exploratory analysis and figure generation
└── README.md
```
***

## Requirements

- R
- RStudio
- Git for cloning the repository and tracking changes

***

## Installation

Clone the repository directly through RStudio:

```text
File → New Project → Version Control → Git
```

***

## Import and data checks

Run the following commands from the project root in the RStudio.

### Import and data validation

```r
source("Import.R")
```

This script:

- Lists the available data files.
- Displays the first lines of the CSV file.
- Imports the dataset.
- Converts the date column to R's `Date` type.
- Sorts observations chronologically.
- Checks column types and the date range.
- Counts missing values and duplicated dates.

## Dataset

The dataset contains 7,660 observations from 2 January 1995 to
31 December 2024.

Available columns:

| Column | Description |
|--------|-------------|
| `Date` | Observation date |
| `Open` | Opening index value |
| `High` | Highest index value during the day |
| `Low` | Lowest index value during the day |
| `Close` | Closing index value |
| `Volume` | Volume field supplied in the dataset |
| `Change` | Change field supplied in the dataset |


***
