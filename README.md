# DSScalculator

This repository provides an automated, end-to-end pipeline for processing high-throughput drug screening data. It performs per-plate normalization against controls, fits four-parameter logistic (4PL) regression models, and calculates absolute IC50, Area Under the Curve (AUC), and specialized Drug Sensitivity Scores (DSS).

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
│   └── trend_example.xlsx     # Trend direction matrix
│
├── LICENSE
└── README.md

```

## Input Files

Place the following required files inside the data/ directory:

1. met_plate.xlsx: Your raw screening data. It must contain a sheet named "Background Subtracted", and include "Activated" and "Not Activated" in the DRUG_NAME column for per-plate normalization.

2. trend_example.xlsx: A matrix defining the expected trend directions (+1 or -1) for each feature.

Note: You may replace these with your own data, provided the file names remain the same and the column structures match the expected format.

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

The script automatically generates and writes the following results into the data/ directory:

1. Intermediate Files (Preprocessing & Normalization):

* corrected_example.xlsx: Data normalized against plate controls (Activated/Not Activated).

* normalized_trend.xlsx: Normalized data multiplied by trend directions.

* formatted_trend.xlsx: Long-format data ready for curve fitting.

2. Final Results:

* DSS_auc_ic50_all_original.xlsx: Raw calculation results (DSS, AUC, IC50) before trend adjustment.

* DSS_auc_ic50_all_reserved.xlsx: Final results with trend signs applied.

* dose_response_curves_perplate.pdf: High-quality visualizations of all fitted dose-response curves for quality control.


## Warnings During Curve Fitting

You may see warnings such as:

```r
Convergence failure
non-finite value supplied by optim
```
These originate from nonlinear curve fitting (nls / nlsLM), and the code will continue running. This behavior is intentional to ensure robustness across large drug panels.

## Running Inside R (Optional)

If you prefer running inside an interactive R session:

```r
source("R/install_deps.R")
source("R/run_dss.R")
```

Make sure you are in the repository root before running.
