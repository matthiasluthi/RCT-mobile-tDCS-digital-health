# Analysis of the Psylect Trial: Home-Use tDCS & Digital Health for Depression

This repository contains the analysis scripts for a **phase-II compliant, double-blinded, randomized controlled trial** (ClinicalTrials.gov ID [NCT04889976](https://clinicaltrials.gov/ct2/show/NCT04889976)) evaluating the efficacy of self-administered, home-use transcranial direct current stimulation (tDCS) with or without a digital health intervention for major depressive disorder.

The full study protocol is available [here](https://www.tandfonline.com/doi/10.1080/14737175.2022.2083959).

> 📄 Results from the primary analysis were published in [JAMA Psychiatry](https://jamanetwork.com/journals/jamapsychiatry/fullarticle/2813623)

> 📄 Ancillary analysis on symptom clusters was published in the [Brazilian Journal of Psychiatry](https://www.bjp.org.br/details/3592/en-US/effects-of-home-use-transcranial-direct-current-stimulation-on-clusters-of-depressive-symptoms--an-ancillary-analysis-of-the-psylect-study)

# 📊 Analysis Overview
**1-data-cleaning.R**
- Initial formatting, harmonization/merging and organization of trial data
- Handling of missing values
- Preparation of analysis-ready datasets

**2-main-analyses.Rmd**
- Summary tables of demographic and clinical variables
- Geographical mapping of participant locations
- Primary outcome analysis using linear mixed models
- Analysis of response and remission rates with logistic regressions
- Evaluation of patient blinding integrity with chi-square tests
- Adverse event comparison using chi-square tests and negative binomial regression
- Generation of plots and tables and automated saving to Word
- [**Rendered report (HTML)**](https://matthiasluthi.github.io/RCT-mobile-tDCS-digital-health/2-primary-analyses.html) - Full output with tables, figures, and statistical results

**3-secondary-analyses.R**
- Additional cleaning and imputation of missing values using Random Forest (missForest)
- Hierarchical clustering of HDRS-17 symptom items to identify data-driven clusters
- Confirmatory analysis using k-means clustering
- Mixed-effects models applied to cluster scores to test for differential treatment response
- Visualizations of clustering solutions and model effects

**0-custom-functions.R**
- Collection of custom utility functions used throughout the project
- Testing for overdispersion in GLMs and automated selection of Poisson or negative-binomial regression for count data
- Testing independence between 2 categorical variables with automated selection of chi-square test or Fisher's exact test
- Fitting tables optimally to A4 format for saving to Word
- Custom publication-ready ggplot themes

# 📁 Study Data
The study data is publicly available in reduced form at [OSF](https://osf.io/jn5st/). 
