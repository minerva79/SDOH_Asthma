###########################################################################
### Exploration of matched cohort                                       ###
### 2023-04-18 / Adam Quek (SingHealth)                                 ###
###########################################################################

# Load Libraries ----------------------------------------------------------
required_libraries <- c('plyr', 'dplyr')
invisible(lapply(required_libraries, require, character=TRUE))


# working data
dat <- readRDS("./rds/merge_2_cleaned_int.rds")
visit <- readRDS("./int/visit_asthma_cohort.rds")

dat <- dat %>% mutate(year = lubridate::year(date))
dat <- dat %>% mutate(month = lubridate::month(date))
dat %>% group_by(year) %>% summarise(total_exacerbation = sum(severe_exacerbation), n = n()) %>% mutate(total = total_exacerbation + n) %>% mutate(prop = total_exacerbation/total)
dat %>% group_by(month) %>% summarise(total_exacerbation = sum(severe_exacerbation), n = n()) %>% mutate(total = total_exacerbation + n) %>% mutate(prop = total_exacerbation/total)

# matching_cohort ids
identified <- readRDS("./int/match_pairs_id.rds") %>% unlist %>% setNames(., NULL)
identified <- unique(identified)
# data.frame(pid = identified) %>% group_by(pid) %>% tally %>% arrange(desc(n))

d2 <- dat %>%
  filter(pid %in% identified)

d2 <- d2 %>% select(-c(severe_exac_count:day_difference))
d2 <- d2 %>% 
  group_by(pid) %>% 
  mutate(severe_exac_count = cumsum(severe_exacerbation),
         visit_count = seq_len(n()),
         day_from_last_visit = date - lag(date))

d2 <- d2 %>% 
  ungroup %>% 
  mutate(day_from_last_visit = ifelse(is.na(day_from_last_visit), 0, as.numeric(day_from_last_visit)))


# Classification of matched cohort patients -------------------------------

classification <- d2 %>% 
  group_by(pid) %>% 
  summarise(dummy = sum(severe_exacerbation)) %>% 
  mutate(category = ifelse(dummy == 0, "non-severe patient", "severe_patient")) %>%
  ungroup %>% select(-dummy) 

d3 <- d2 %>% left_join(classification)
saveRDS(d3, "./int/working_data.rds")


classification <- d3 %>% 
  group_by(pid, category) %>% 
  summarise(female = mean(female), 
            race_chinese = mean(race_chinese), 
            age_on_visit = mean(age_on_visit),
            gina_step = mean(gina_step),
            last_visit = max(date))

classification <- data.frame(pid = identified) %>% left_join(classification)

saveRDS(classification, "./int/matched_patient_cohort.rds")
write.csv(classification, "./int/matched_patient_cohort.csv", row.names=FALSE)

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

vars <- c("female", "race_chinese", "age_on_visit", "gina_step")
output_ttest <- ldply(vars, function(x) t_test_summary(classification, x))