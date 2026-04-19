# Regression Discontinuity Analysis of Book Prices and Copyright

This project analyzes the impact of copyright status on book prices and availability using a Regression Discontinuity Design (RDD) around the 1923 copyright threshold.

## Research Objective
The goal is to estimate how copyright protection affects:
- Book prices on Amazon
- Availability of different book formats
- Number of editions per title

The analysis exploits the discontinuity at the year **1923**, when works transition into the public domain.

## Data
The project uses the following datasets:
- `amazon_data.dta` – prices, sales, formats, and demand
- `edition_details.dta` – edition-level characteristics (pages, prices, publication info)
- `gutenberg_downloads.dta` – download counts from Project Gutenberg

## Methodology
- Data cleaning and feature engineering (dummy variables, transformations)
- Descriptive statistics and t-tests
- Regression Discontinuity Design (RDD)
- Polynomial regressions around the cutoff (1923)
- Density testing (McCrary test)
- Cluster-robust standard errors

## Key Steps
- Construction of treatment variable (`post1923`)
- Log transformation of prices
- Bandwidth selection and sensitivity analysis
- Estimation of multiple regression models
- Separate analysis by format (Hardcover, Paperback, E-book)
- Aggregation at title and edition level

## Tools & Packages
- R
- `tidyverse`, `dplyr`, `ggplot2`
- `haven`
- `rdd`, `rddensity`, `rddapp`
- `sandwich`, `lmtest`
- `patchwork`
- `officer`, `flextable`
- `jtools`

## How to Run
1. Place all `.dta` files in your working directory
2. Install required packages in R
3. Run the script in RStudio or R

##  Output
- Summary statistics and t-test results
- RDD estimates and regression outputs
- Visualizations of discontinuities
- Exported regression tables (`.docx`)
