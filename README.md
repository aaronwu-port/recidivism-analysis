Exploring Causes of Recidivism and Rehabilitative Inefficiencies
Project Overview
This project analyzes criminal recidivism data to examine the effectiveness of current rehabilitation programs and identify factors that contribute to reoffending. The analysis explores whether the U.S. criminal justice system should pivot from traditional rehabilitation methods to more education and employment-focused re-entry programs.
Research Question
How can rehabilitative programs be improved to reduce recidivism rates and better protect society from reoffending criminals?
Key Findings

Traditional rehabilitation programs show only modest effects (10% reduction in recidivism probability)
Longer prison sentences are associated with 18% reduction in recidivism
Higher education reduces recidivism probability by 16%
Employment opportunities provide the strongest protective factor (29% reduction)
Gang affiliation is the strongest risk factor (26% increase in recidivism probability)

Data
Source: U.S. Department of Justice NIJ Recidivism Challenge Dataset (2021)

Observations: 25,835 individuals
Time Period: 3-year follow-up
Variables: 57 variables including demographics, criminal history, program participation, employment, education, and recidivism outcomes

Repository Structure
├── Recid-Analysis-Code.r          # Main R analysis script
├── Analytics Project Write-Up.pdf  # Complete research paper
├── README.md                      # This file
└── data/                          # Data directory (not included)
    └── NIJ Challenge2021_Full Dataset.sav
Methodology
Statistical Approach

Primary Method: Logistic regression analysis
Dependent Variable: Recidivism within 3 years (binary)
Key Independent Variables:

Program attendance (continuous)
Prison sentence length (categorical)
Education level (binary: college vs. no college)
Employment percentage (continuous)
Gang affiliation (binary)

Models Analyzed

Rehabilitation Model: Effect of program attendance on recidivism
Sentence Length Model: Impact of incarceration duration
Education Model: College education vs. no college
Employment Model: Percentage of days employed post-release
Gang Affiliation Model: Gang membership effects
Full Model: Combined effects with controls

Key Variables
Dependent Variable

Recidivism_Within_3years: Binary indicator of reoffending within 3 years

Independent Variables

Program_Attendances: Number of rehabilitation programs attended (0-10)
Prison_Years_Ordinal: Sentence length categories (<1, 1-2, 2-3, >3 years)
Education_Ordinal: Education level (Without College, Some College)
Percent_Days_Employed: Proportion of post-release days employed (0-1)
Gang_Affiliated: Gang membership status (Non-Gang, Gang)

Results Summary
Probability of Recidivism by Factor
FactorBaselineImproved ConditionReductionRehabilitation60.6% (0 programs)50.7% (10 programs)9.9%Sentence Length64.4% (<1 year)46.6% (>3 years)17.8%Education60.5% (no college)44.8% (some college)15.7%Employment72.2% (0% employed)43.9% (100% employed)28.3%Gang Affiliation53.8% (non-gang)79.2% (gang)+25.4%
Policy Implications

Education Integration: Incorporate higher education opportunities into prison programs
Employment Focus: Prioritize job training and placement services
Sentence Structure: Consider longer sentences to allow completion of educational programs
Gang Intervention: Develop targeted interventions for gang-affiliated individuals
Holistic Approach: Combine traditional rehabilitation with education and employment support

Limitations

Missing Socioeconomic Data: Limited information on poverty, family structure, and neighborhood effects
Cross-sectional Design: Cannot establish causation definitively
Selection Bias: Self-selection into programs may confound results
Omitted Variable Bias: Gang affiliation may proxy for unmeasured social factors

Requirements
R Packages
rlibrary(tidyverse)
library(stargazer)
library(ggplot2)
library(ggeffects)
library(haven)
System Requirements

R version 4.0 or higher
RStudio (recommended)

Usage

Data Setup: Download the NIJ dataset and update the file path in the script
Run Analysis: Execute Recid-Analysis-Code.r to reproduce all results
View Output: The script generates:

Summary statistics tables
Logistic regression models
Probability prediction plots
LaTeX regression table (for academic papers)


Visualizations
The analysis produces five main plots:

Recidivism probability by program attendance
Recidivism probability by sentence length
Recidivism probability by education level
Recidivism probability by employment level
Recidivism probability by gang affiliation

Academic Context
This project was completed as part of an econometrics course (EB-020) and demonstrates:

Applied econometric analysis
Policy evaluation methodology
Data visualization techniques
Academic writing and presentation

Author
Aaron Wu
Course: EB-020
Date: April 23, 2025
References
Complete bibliography available in the full research paper (Analytics Project Write-Up.pdf).
License
This project is for educational purposes. Please cite appropriately if using any methods or findings.
