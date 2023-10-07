# SDOH_Asthma: A Population-based Study on Severe Asthma Patients

This repository hosts the scripts and related files used for the research poster titled "Impact of Social Determinants of Health (SDOH) on Severe Asthma Patients: A Population-based Study."

## Repository Structure

The repository is organized into two main subfolders:

### 1. `r_script`

This folder contains R scripts executed in a sequential manner for various stages of data processing and analysis:

- **Code #01 & #02**: Construct longitudinal data for all patients. This includes severe exacerbation criteria, visit details, and patient demographics/medical history.
  
- **Code #03**: Implement 1:1 matching of non-severe to severe asthma patients based on attributes like gender, race, GINA steps, and last visit dates.

- **Code #04**: Wrangle 2015 household survey and 2020 census data (sourced from SLA OneMap) to fit into 28 postal districts.
  
- **Code #04**: Perform t-tests on the matched cohort to examine statistical differences between groups.

- **Code #05**: Run logistic regression models to explore the impact of SDOH factors on the matched cohort.

### 2. `Onemap`

This folder houses the script specifically designed for scraping data from OneMap, which is crucial for the SDOH analysis.

### Exclusions

The repository currently does not include scripts for examining the impact of weather and air quality data on asthma patients.
