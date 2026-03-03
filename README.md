# DSScalculator

This script automates the processing of drug screening data, fitting four-parameter logistic (4PL) regression models to calculate IC50, AUC, and specialized Drug Sensitivity Scores (DSS).

### Main Features

* **Data Normalization:** Multiplies raw inhibition data by trend coefficients (e.g., to handle directionality).
* **4PL Curve Fitting:** Uses the `nls` (Port algorithm) and `minpack.lm::nlsLM` to robustly fit dose-response data, even with noisy samples.
* **DSS Calculation (BREEZE-based):** Implements three types of DSS based on the area under the fitted curve relative to tested concentration ranges.
* **Visualization:** Automatically generates dose-response plots with `ggplot2` and exports them to a multi-page PDF.
* **Batch Processing:** Handles multiple drugs and screen conditions simultaneously using `dplyr` grouping and `lapply`.

### Requirements

* R ≥ 4.1 (recommended)
* macOS / Linux / Windows supported

### Project Structure

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

### Input Files

Place the following files inside the data/ directory:
* corrected_example.xlsx
* trend_example.xlsx
You may replace them with your own data as long as:
* File names remain the same
* Column structure is compatible with the script



### Usage Note

To run the analysis, ensure your R working directory is set to the root of this project. You can execute the script via terminal or RStudio:

```r
source("./scripts/DSScalculator.R")

```

> **Note:** Ensure your local file paths in the script are updated to match your directory structure (pointing to `./data/`) before running.
