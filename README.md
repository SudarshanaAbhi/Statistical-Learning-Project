# COVID-19 Patient Outcome Prediction

This repository contains statistical and machine learning analyses of COVID-19 patient data, focusing on predicting various clinical outcomes. The project was developed as part of a Statistical Learning course in Spring 2021.

## Project Overview

This project analyzes clinical data from COVID-19 patients to predict several important outcomes:
- Death
- Inpatient admission
- ICU admission
- Critical condition
- Ventilation requirement

## Analysis Components

The project consists of two main analysis files:

1. **Basic statistical analysis & visualization**
   - Exploratory data analysis of patient demographics
   - Distribution analysis of age and BMI
   - Normality tests and visualizations
   - Missing data analysis

2. **Data prep & ML model implementation**
   - Data balancing for each outcome
   - Implementation of three types of models:
     - Logistic Regression
     - Lasso Regression
     - Ridge Regression
   - Model evaluation using confusion matrices
   - Cross-validation for regularization parameters

## Requirements

- R programming language
- Required R packages:
  - dplyr
  - tidyverse
  - caret
  - glmnet
  - ggplot2

## Usage

1. Clone the repository
2. Install required R packages
3. Run the analysis files in sequence:
   - First run the basic statistical analysis
   - Then run the machine learning implementation

## Results

The analysis provides:
- Statistical summaries of patient demographics
- Visualizations of key variables
- Predictive models for different clinical outcomes
- Model performance metrics and comparisons

## Author

Abhinand S.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
