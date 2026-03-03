# DSScalculator

This script automates the processing of drug screening data, fitting four-parameter logistic (4PL) regression models to calculate IC50, AUC, and specialized Drug Sensitivity Scores (DSS).

## Requirements

* R ≥ 4.1 (recommended)
* macOS / Linux / Windows supported

## Project Structure

```text
DSSCalculator/
│
├── R/
│   ├── install_deps.R
│   └── run_dss.R
│
├── data/
│   ├── corrected_example.xlsx
│   └── trend_example.xlsx
│
├── LICENSE
└── README.md

```

## Input Files

Place the following files inside the data/ directory:
* corrected_example.xlsx
* trend_example.xlsx
You may replace them with your own data as long as:
* File names remain the same
* Column structure is compatible with the script

## Quick Start

### 1. Clone the repository

```r
git clone <YOUR_REPOSITORY_URL>
cd DSSCalculator
```

### 2. Install required R packages

```r
Rscript R/install_deps.R
```
This installs all required packages automatically.

### 3. Run the DSS calculation

```r
Rscript R/run_dss.R
```

## Output Files

The script automatically writes results into the data/ directory:
* normalized_trend.xlsx
* formatted_trend.xlsx
* DSS_auc_ic50_all_reserved.xlsx


### Warnings During Curve Fitting

You may see warnings such as:

```r
source("./scripts/DSScalculator.R")

```

> **Note:** Ensure your local file paths in the script are updated to match your directory structure (pointing to `./data/`) before running.
