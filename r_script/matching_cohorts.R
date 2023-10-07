###########################################################################
### Matching Non-severe to Severe Asthma Patient                        ###
### 2023-10-07 / Adam Quek (SingHealth)                                 ###
###########################################################################

# Load Libraries ----------------------------------------------------------
required_libraries <- c('dplyr', 'reshape')
invisible(lapply(required_libraries, require, character=TRUE))

# Read data
dat <- readRDS("./rds/merge_2_cleaned_int.rds")

# Identify cohorts
patients_severe <- unique(dat$pid[dat$severe_exacerbation == 1])
all_patients <- unique(dat$pid)
patients_non_severe <- setdiff(all_patients, patients_severe)


# generate covariate averages for matching purposes -----------------------
means <- function(x) mean(x, na.rm=TRUE)
covariate_averages <- aggregate(cbind(female, race_chinese, age_on_visit, gina_step) ~ pid,
                                data = dat, FUN = means)
last_visit_dates <- aggregate(date ~ pid, data = dat, FUN = max)
covariate_dates <- merge(covariate_averages, last_visit_dates, by = "pid")


# Main function - perform matching
find_nearest_match <- function(treated, control){
  
  # determine pairwise distance based on covariate averages
  covariate_distance <- sapply(1:nrow(control), function(x){
    sqrt(sum((control[x, c("female", "race_chinese", "age_on_visit", "gina_step")] - 
                treated[, c("female", "race_chinese", "age_on_visit", "gina_step")])^2))
  })
  
  # determine the pairwise day difference of last-visit date
  date_difference <- abs(as.numeric(difftime(control$date, treated$date, units = "days")))
  
  # define a weight to balance the importance of covariate distance and date difference
  date_weight <- 0.5
  
  # combined scoring of covariate averages (0.5) and day difference in last visit day (0.5)
  combined_score <- date_weight*date_difference + (1 - date_weight)*covariate_distance
  
  # return closest control
  nearest_index <- which.min(combined_score)
  return(control[nearest_index, ])
}

# Initialise an empty data frame to store matched pairs
matched_pairs <- data.frame()

# Find the nearest match for each patient with the condition and add it to matched pairs
for(i in seq_len(length(patients_severe))){
  treated_unit <- covariate_dates[covariate_dates$pid == patients_severe[i],]
  control_units <- covariate_dates[covariate_dates$pid %in% patients_non_severe,]
  nearest_control_unit <- find_nearest_match(treated_unit, control_units)
  
  # Remove the matched control unit from the pool of available control units
  patients_non_severe <- setdiff(patients_non_severe, nearest_control_unit$pid)
  
  # Add the matched pair to the data.frame
  matched_pairs <- rbind(matched_pairs, treated_unit$pid, nearest_control_unit$pid)
}

matched_pairs2 <- c(identified, unlist(matched_pairs))
saveRDS(matched_pairs2, "./int/match_pairs_id.rds")