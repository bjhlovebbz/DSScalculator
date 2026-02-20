# DSScalculator

This script automates the processing of drug screening data, fitting four-parameter logistic (4PL) regression models to calculate IC50, AUC, and specialized Drug Sensitivity Scores (DSS).

### Main Features

* **Data Normalization:** Multiplies raw inhibition data by trend coefficients (e.g., to handle directionality).
* **4PL Curve Fitting:** Uses the `nls` (Port algorithm) and `minpack.lm::nlsLM` to robustly fit dose-response data, even with noisy samples.
* **DSS Calculation:** Implements three types of DSS based on the area under the fitted curve relative to tested concentration ranges.
* **Visualization:** Automatically generates dose-response plots with `ggplot2` and exports them to a multi-page PDF.
* **Batch Processing:** Handles multiple drugs and screen conditions simultaneously using `dplyr` grouping and `lapply`.

### Prerequisites

The script requires several R libraries, primarily:

* **Data Wrangling:** `tidyverse` (dplyr, tidyr, tibble), `readxl`, `writexl`.
* **Modeling:** `drc`, `minpack.lm`, `caTools`, `MESS`.
* **Plotting:** `ggplot2`, `gridExtra`.

### Project Structure

```text
.
├── scripts/
│   └── DSS_calculation_script.R    # Main calculation script
├── data/
│   ├── corrected_example.xlsx      # Raw inhibition data
│   └── trend_example.xlsx          # Directionality/weights
└── README.md

```

### Pipeline Workflow

1. **Data Ingestion:** Loads `corrected_example.xlsx` and `trend_example.xlsx` from the `data/` directory.
2. **Formatting:** Reshapes data into a long-format "tidy" data frame.
3. **Parameter Estimation:** * Executes the core logic located in `./scripts/DSS_calculation_script.R`.
* Estimates initial parameters (Slope, Min, Max, IC50).
* Applies constraints to IC50 and Max values to ensure biological relevance.


4. **Scoring:** Calculates **DSS_type 2** (default) which incorporates the area under the curve and normalization.
5. **Output:** * Saves formatted results to `formatted_trend.xlsx`.
* Saves final scores (DSS, AUC, IC50abs) to `DSS_auc_ic50_all_reserved.xlsx`.
* Exports curve plots to `dose_response_curves.pdf`.



### Usage Note

To run the analysis, ensure your R working directory is set to the root of this project. You can execute the script via terminal or RStudio:

```r
source("./scripts/DSS_calculation_script.R")

```

> **Note:** Ensure your local file paths in the script are updated to match your directory structure (pointing to `./data/`) before running.
