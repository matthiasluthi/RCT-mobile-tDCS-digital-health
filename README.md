# Analysis of the Psylect Trial: Home-Use tDCS for Depression

This repository contains the analysis scripts for a **phase-II compliant, double-blinded, randomized controlled trial** (ClinicalTrials.gov ID [NCT04889976](https://clinicaltrials.gov/ct2/show/NCT04889976)) evaluating the efficacy of self-administered, home-use transcranial direct current stimulation (tDCS) with or without a digital health intervention for major depressive disorder.

The full study protocol is available [here](https://www.tandfonline.com/doi/10.1080/14737175.2022.2083959).

> 📄 Results from the primary analysis were published in [**JAMA Psychiatry**](https://jamanetwork.com/journals/jamapsychiatry/fullarticle/2813623)

> 📄 Ancillary analysis on symptom clusters was published in the [**Brazilian Journal of Psychiatry**](https://www.bjp.org.br/details/3592/en-US/effects-of-home-use-transcranial-direct-current-stimulation-on-clusters-of-depressive-symptoms--an-ancillary-analysis-of-the-psylect-study)

# 📊 Analysis Overview
1_data_cleaning.R
- Initial formatting, harmonization/merging and organization of trial data
- Handling of missing values
- Preparation of analysis-ready datasets

2_main_analyses.Rmd
- Summary tables of demographic and clinical variables
- Geographical mapping of participant locations
- Primary outcome analysis using linear mixed models
- Logistic regression for response and remission
- Chi-square tests for blinding integrity
- Adverse event comparison using chi-square tests and negative binomial regression
- Generation of plots and tables and automated saving to Word
- [**Rendered report (HTML)**](https://matthiasluthi.github.io/rct-brain-stimulation-elderly/2_primary_analyses.html) - – Full output with tables, figures, and statistical results

3_secondary_analyses.R
- Additional cleaning and imputation of missing values using Random Forest (missForest)
- Hierarchical clustering of HDRS-17 symptom items to identify data-driven clusters
- Confirmatory analysis using k-means clustering
- Mixed-effects models applied to cluster scores to test for differential treatment response
- Visualizations of clustering solutions and model effects

# 📁 Study Data
The study data is publicly available in reduced form at [OSF](https://osf.io/jn5st/). 
