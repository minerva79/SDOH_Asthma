###########################################################################
### MSc Report                                                          ###
### 2023-05-02 / Adam Quek (SingHealth)                                 ###
###########################################################################

# Load Libraries ----------------------------------------------------------
required_libraries <- c('plyr', 'dplyr', 'tidyr', 'lubridate')
invisible(lapply(required_libraries, require, character=TRUE))

# Read data
d3 <- readRDS("./int/working_data.rds")
postal <- readRDS("./int/postal&region_match.rds")
sdf_tab <- readRDS("./int/sdf_data.rds")
classification <- readRDS("./int/matched_patient_cohort.rds")

# Data Preprocessing
d3 <- d3 %>% 
  select(pid, date:gina_step, severe_exac_count:day_from_last_visit) %>%
  left_join(classification %>% select(pid, category)) %>%
  left_join(postal) %>%
  left_join(sdf_tab)

# Filtering and Selecting
d4 <- d3 %>% 
  filter(date <= as.Date("2019-12-31")) %>%
  select(-date) %>%
  group_by(pid) %>% 
  slice(1)

# Summarize by Categories
d4 %>% 
  group_by(category) %>% 
  summarise_at(vars(starts_with("district_")), mean) %>% 
  data.frame

# Perform t-tests
# Define t-test summary function
t_test_summary <- function(data, var_name){
  
  # filter data for each group
  severe_data <- data %>% filter(category == "severe_patient")
  non_severe_data <- data %>% filter(category == "non-severe patient")
  
  # compute means for each group
  mean_severe <- mean(severe_data[[var_name]])
  mean_non_severe <- mean(non_severe_data[[var_name]])
  
  # perform t-test
  t_test_result <- t.test(severe_data[[var_name]], non_severe_data[[var_name]])
  
  # extract required values
  t_statistic <- t_test_result$statistic
  standard_error <- t_test_result$stderr
  p_value <- t_test_result$p.value
  mean_diff <- mean_severe - mean_non_severe
  
  # create a data.frame
  summary <- data.frame(var_name, mean_severe, mean_non_severe, t_statistic, standard_error, p_value, mean_diff)
  
  return(summary)
}

# Execute t-tests for variables starting with 'district_'
vars <- d4 %>% 
  ungroup %>% 
  select(starts_with("district_")) %>% 
  names
output_ttest <- ldply(vars, function(x) t_test_summary(d4, x))


# Data Scaling
variables_to_scale <- names(select(d4, age_on_visit, visit_count, severe_exac_count, daily_rainfall, mean_temp, max_wind_spd, pm10:district_buddhism_taoism_level))
d4 <- d4 %>% 
  mutate_at(vars(one_of(variables_to_scale)), scale)

# Convert severe patient category as 1
d4 <- d4 %>% mutate(patient = as.numeric(category == "severe_patient"))

# Define and Fit Models
# Linear Model
model <- lm(patient ~ district_edn_level + district_hh_income_level +
               district_employment_level + district_hh_size +
               district_blue_occp_collar_level + district_transport_public_level + 
               district_buddhism_taoism_level + 
               female + race_chinese + 
               age_on_visit + gina_step + severe_exac_count + visit_count,
             data = d4) 

# Logistic Regression Model
model <- glm(patient ~ district_edn_level + district_hh_income_level +
               district_employment_level + district_hh_size +
               district_blue_occp_collar_level + district_transport_public_level + 
               district_buddhism_taoism_level + 
               female + race_chinese + 
               age_on_visit + gina_step,
             data = d4, family = "binomial") 

# view summary model
summary(model)

# calculate odds ratio and confidence inervals
exp_coef <- exp(coef(model))

# Model Diagnostic
null_deviance <- model$null.deviance
residual_deviance <- model$deviance

# McFadden's pseudo R-squared
mcfadden_r2 <- 1 - (residual_deviance/null_deviance)
sample_size <- length(model$y)

predicted_probs <- predict(model, d4, type="response")
predicted_outcomes <- ifelse(predicted_probs > 0.5, 1, 0)
confusion_mtx <- table(Predicted = predicted_outcomes, Actual = d4$severe_exacerbation)

precision <- confusion_mtx[2,2]/ (confusion_mtx[2,2] + confusion_mtx[2,1])
recall <- confusion_mtx[2,2]/ (confusion_mtx[2,2] + confusion_mtx[1,2])
f1_score <- 2*(precision*recall)/(precision + recall)
