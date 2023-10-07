###########################################################################
### Construct Exacerbation Longitudinal Data                            ###
### 2023-10-07 / Adam Quek (SingHealth)                                 ###
###########################################################################

# Load Libraries ----------------------------------------------------------
required_libraries <- c('dplyr', 'reshape')
invisible(lapply(required_libraries, require, character=TRUE))

# Helper Function ---------------------------------------------------------
convert_to_binary <- function(column, yes_value = "Yes", no_value = "No") {
  ifelse(column == yes_value, 1, ifelse(column == no_value, 0, NA))
}

save_data <- function(data, path, ext = "rds") {
  if (ext == "rds") {
    saveRDS(data, path)
  } else if (ext == "csv") {
    write.csv(data, path, row.names = FALSE)
  }
}

# Read Data
dat <- readRDS("./int/exacerbation_matched_visit_cases.rds")

# Data Preprocessing
dat <- dat %>% 
  arrange(date) %>% 
  mutate(
    severe_exac_count = ave(severe_exacerbation, as.numeric(as.factor(pid)), FUN = cumsum),
    visits_count = ave(as.numeric(as.factor(pid)), as.numeric(as.factor(pid)), FUN = seq_along) - 1,
    day_difference = ave(as.numeric(date), pid, FUN = function(x) c(0, diff(x)))
  )

# Data Inspection (for first seven pid)
unique_pids <- unique(dat$pid)
for(pid in unique_pids[1:7]){
  tmp <- dat[dat$pid == pid, c("date", "day_difference", "severe_exac_count", "visits_count")]
  plot(severe_exac_count ~ day_difference, tmp)
  plot(severe_exac_count ~ date, tmp)
  plot(day_difference ~ visits_count, tmp)
}

# Merge dataset -----------------------------------------------------------
# left join exacerbation dataset with master dataset by pid
master$pid <- master$Patient.ID
merged_dat <- merge(dat, master, by = "pid", all.x = TRUE)

# Main Code
# Feature Engineering
merged_dat <- merged_dat %>%
  mutate(
    age_on_visit = as.numeric((date - as.Date(Date.of.Birth)) / 365.25),
    female = as.numeric(Gender == "FEMALE"),
    race_chinese = as.numeric(Race == "Chinese"),
    race_indian = as.numeric(Race == "Indian"),
    race_malay = as.numeric(Race == "Malay"),
    race_others = as.numeric(Race == "Others"),
    allergic_conjunctivitis = convert_to_binary(Allergic.conjunctivitis),
    allergic_rhinitis = convert_to_binary(Allergic.rhinitis),
    anxiety_disorder = convert_to_binary(Anxiety.disorder),
    atopic_dermatitis = convert_to_binary(Atopic.dermatitis),
    copd = convert_to_binary(COPD),
    depressive_disorder = convert_to_binary(Depressive.disorder),
    gerd = convert_to_binary(GERD),
    heart_failure = convert_to_binary(Heart.failure),
    hypertension = convert_to_binary(Hypertension),
    osa = convert_to_binary(Obstructive.sleep.apnoea),
    pneumonia = convert_to_binary(Pneumonia),
    pulmonary_tuberculosis = convert_to_binary(Pulmonary.tuberculosis),
    smoker = convert_to_binary(Smoking.History, yes_value = "smoker", no_value = ""),
    bmi = BMI,
    gina_step = Final.GINA.STEP
  )


d3 <- merged_dat[, c("pid", "date", "severe_exacerbation", "severe_exac_count", "visits_count", 
             "day_difference", "female", "race_chinese", "race_indian", "race_malay",
             "race_others", "Postal.Code", "age_on_visit", 
             "allergic_conjunctivitis", "allergic_rhinitis", "anxiety_disorder", 
             "atopic_dermatitis", "copd", "depressive_disorder", "gerd",
             "heart_failure", "hypertension", "osa", "pneumonia",
             "pulmonary_tuberculosis", "smoker", "bmi", "gina_step")]

# Arrange and Save Data
d3 <- d3 %>% arrange(pid, date)
save_data(d3, "./rds/merge_2_cleaned_int.rds")
save_data(d3, "./int/working_dat.csv", "csv")