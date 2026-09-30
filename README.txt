# Forest Value Perceptions Across Europe

This repository contains the reproducible analysis workflow for the study:

**“Explaining forest value perceptions across Europe: A multilevel analysis of individual and geographic variation.”**

The study examines how socio-cultural, provisioning, and regulating/supporting forest values vary across individuals and geographic contexts in Europe. The analysis draws on survey data from 12,460 adults across 12 European countries and five Forest Europe macro-regions.

## Study overview

The study addresses three related questions:

1. How much variation in forest values occurs among individuals, countries, and broader European macro-regions?
2. To what extent are observed macro-regional differences associated with individual-level characteristics?
3. How are socio-demographic characteristics, forest ownership, and social proximity to forest ownership associated with different forest-value domains?

Three forest-value domains are analysed:

- **Socio-cultural values**
- **Provisioning values**
- **Regulating/supporting values**

The analysis combines weighted linear mixed-effects models with multilevel variance partitioning to distinguish differences in average forest-value perceptions from the extent to which residual variation is geographically clustered.

## Geographic coverage

The analysis includes respondents from 12 European countries:

- Croatia
- Czechia
- Denmark
- France
- Germany
- Italy
- Netherlands
- Romania
- Scotland
- Serbia
- Spain
- Sweden

Countries are additionally grouped into five Forest Europe macro-regions for comparative analysis.

## Repository contents

The main R script provides the complete analytical workflow used for the study, including:

- data preparation and validation;
- construction and validation of analytical predictors;
- post-stratification weight normalisation;
- descriptive statistics;
- country- and macro-region-level summaries;
- production of study figures;
- weighted linear mixed-effects models;
- comparison of region-only and adjusted models;
- three-level variance partitioning;
- bootstrap confidence intervals for variance components;
- hierarchical-model sensitivity analysis;
- weighted versus unweighted sensitivity analysis;
- missing-category sensitivity analysis;
- model diagnostics;
- country-clustered CR2 robust inference;
- reliability analysis; and
- generation of manuscript and supplementary tables.

### Main analysis script

`Forest_value_perceptions_FINAL_reproducible_analysis.R`

The script is intended to be run from a fresh R session from the project root.

## Software and R packages

The analysis was conducted in R.

The principal packages used in the reproducible workflow include:

- `dplyr`
- `tidyr`
- `lme4`
- `lmerTest`
- `broom.mixed`
- `openxlsx`
- `ggplot2`
- `clubSandwich`
- `sf`
- `rnaturalearth`
- `rnaturalearthdata`
- `ggspatial`
- `psych`

Package and R version information can be recorded using `sessionInfo()` at the end of the analytical workflow.

## Data availability

The analyses use respondent-level survey data collected as part of the EU Horizon 2020 SUPERB project.

The analytical dataset is not included directly in this GitHub repository. This avoids redistributing respondent-level information, including geographic information, outside the designated research-data repository.

Information on access to the archived dataset and its associated documentation will be provided through the study's research-data record.

Users wishing to reproduce the analysis should obtain the authorised analytical dataset and place it in the project directory expected by the R workflow.

## Reproducibility

To reproduce the analysis:

1. Clone or download this repository.
2. Open the project from its root directory.
3. Obtain the analytical dataset from the designated research-data repository.
4. Place the data in the directory specified in the R script.
5. Install the required R packages.
6. Start a fresh R session.
7. Run `Forest_value_perceptions_FINAL_reproducible_analysis.R` from beginning to end.

The workflow contains validation checks for key features of the analytical dataset and major analytical outputs. It also records R session information to support reproducibility.

## Important note on interpretation

The study is based on cross-sectional survey data. Estimated coefficients therefore represent statistical associations and should not be interpreted as causal effects.

The multilevel variance components identify the geographic levels at which residual variation is clustered; they do not, by themselves, identify the mechanisms generating that variation.

## Funding and project context

This research was conducted within the **SUPERB – Systemic solutions for upscaling of urgent ecosystem restoration for forest-related biodiversity and ecosystem services** project, funded by the European Union's Horizon 2020 Research and Innovation Programme.

## Citation

A complete citation for the associated article will be added following publication.

If you use the analytical workflow or associated research outputs, please cite the published article and the corresponding research-data record.

## Authors

**Moses Kazungu and Marcel Hunziker**

Author affiliations and the complete citation will be provided with the published article.

## Licence

Licence information for the analysis code will be specified separately. The licence applying to the code does not automatically apply to the underlying survey data, which are subject to the terms of their designated research-data repository.

## Contact

For questions concerning the analysis or reproducibility materials, please contact the corresponding author through the contact information provided in the associated publication.