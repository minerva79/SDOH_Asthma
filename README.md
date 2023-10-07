# SDOH_Asthma
Scripts used for the poster on "Impacrt of Social Determinants of Health (SDOH) on Severe Asthma Patients: A Population-based Study".

The repository contains two subfolders:
- r_script: contains the scripts used sequentially to (i) construct longitudinal data for all patients with available severe exacerbation criteria, visit details and patient demographics/medical history (codes #01 and #02); (ii) matching non-severe patient to severe patient in 1:1 ratio based on gender, race, GINA and last visit dates (code #03); (iii) wrangling of 2015 household survey and 2020 census data (SLA OneMap) to fit into 28 postal districts; (iv) t-test of matched cohort (code #04); (v) matching of working data with social determinant data and running of logistic regression to examine impact of the SDOH on matched cohort.

- onemap: contains the script used for scraping OneMap data

- codes exclude examination of weather and air quality data
