# Risk Analysis Template

This repository contains a template for performing risk analysis with multivariate Monte Carlo simulations in R using the [mc2d](https://cran.r-project.org/web/packages/mc2d/vignettes/docmcEnglish.pdf) package.

## Files

-   `data.csv`: Input data file containing disease information and
    parameters.
-   `custom_functions.R`: Custom R functions for data manipulation and
    visualization.
-   `model.R`: Main R script for running the risk analysis model.

## Setup

1.  Open `risk_analysis_template.Rproj` in RStudio (recommended).

2.  Alternatively, set your working directory to the project folder.

3.  Install required packages:

    ``` r
    install.packages(c("mc2d", "ggplot2", "dplyr", "tidyr"))
    ```

## Usage

You can modify the `data.csv` and `model.R` script to adjust parameters, add new variables, or create additional visualizations as needed for your specific risk analysis scenario.

## Custom Functions

The `custom_functions.R` file contains helper functions for:

-   Summarizing Monte Carlo nodes

-   Converting Monte Carlo data to long format

-   Creating boxplots for Monte Carlo results

## License

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Contact

[mail\@nataliaciria.com](mailto:mail@nataliaciria.com)
