###########################################################################
### Construct Longitudinal Dataset of Severe Exacerbation               ###
### 2023-10-07 / Adam Quek (SingHealth)                                 ###
###########################################################################

# Load Libraries ----------------------------------------------------------
required_libraries <- c('plyr', 'magrittr', 'lubridate', 'ggplot2', 'reshape2')
invisible(lapply(required_libraries, require, character=TRUE))

# Helper Function ---------------------------------------------------------
read_csv_from_path <- function(path, filename) {
  tryCatch({
    read.csv(paste0(path, "/", filename))
  }, warning = function(w) {
    print(paste0("Warning: ", conditionMessage(w)))
  }, error = function(e) {
    print(paste0("Error: ", conditionMessage(e)))
    return(NULL)
  })
}

convert_to_binary <- function(column, yes_value = "Yes", no_value = "No") {
  ifelse(column == yes_value, 1, ifelse(column == no_value, 0, NA))
}

convert_date <- function(date_col) {
  date <- as.Date(date_col, format = "%d/%m/%y")
  date <- ifelse(!is.na(date), date, as.Date(date_col, format="%Y%m%d"))
  as.Date(date, origin = "1970-01-01")
}

save_data <- function(data, path, ext = "rds") {
  if (ext == "rds") {
    saveRDS(data, path)
  } else if (ext == "csv") {
    write.csv(data, path, row.names = FALSE)
  }
}

check_date_overlap <- function(pid, exac_data, visit_data) {
  exac_dates <- exac_data[exac_data$pid == pid, "date"]
  visit_dates <- visit_data[visit_data$Patient.ID == pid, "date"]
  return(any(exac_dates %in% visit_dates == FALSE))
}

keep_data <- function(pid, exac_data, visit_data) {
  exac_sub <- exac_data[exac_data$pid == pid, ]
  visit_dates <- visit_data[visit_data$Patient.ID == pid, "date"]
  return(exac_sub[exac_sub$date %in% visit_dates, ])
}

# Data Import -------------------------------------------------------------
# Initialize variables
gsk_file_path <- "your_path_here"
file2 <- "master.csv"
file3 <- "exac.csv"
file4 <- "visits.csv"

master <- read_csv_from_path(gsk_file_path, file2)
exac <- read_csv_from_path(gsk_file_path, file3)
visits <- read_csv_from_path(gsk_file_path, file4)

# Data Cleaning ----------------------------------------------
# Master Data
master$Index.Date <- as.Date(master$Index.Date)

# Visit Date
visits$date <- convert_date(visits$Visit.Date)

# Exac Data
str(exac)
exac$Date <- as.Date(exac$Date)

binary_columns <- c('Exacerbations.Hydrocort.Final', 'Exacerbations.Rescue.Therapy', 
                    'Exacerbations.Diagnosis...J459..A.E..inpatient', 'SAMA', 'Exacerbations.Final')

exac[binary_columns] <- lapply(exac[binary_columns], convert_to_binary)

# Additional binary conversions based on specific conditions
exac$Diagnosis.Institution.Code <- ifelse(exac$Diagnosis.Institution.Code == "SGH", 1, 0)
exac$ed_case <- ifelse(exac$Case.Type.Desc == "A&E", 1, 0)
exac$inpatient_case <- ifelse(exac$Case.Type.Desc == "Inpatient", 1, 0)
exac$outpatient_case <- ifelse(exac$Case.Type.Desc == "", 1, 0)
exac$Diagnosis.Code <- ifelse(exac$Diagnosis.Code == "J459", 1, 0)
exac$Rescue.Therapy.Code <- ifelse(exac$Rescue.Therapy.Code == "N30", 1, 0)

# Create a new dataframe 'exac_2' while renaming columns
exac_2 <- exac[,-1]
colnames(exac_2) <- c("pid", "date", "hydrocort", "rescue_therapy", "sama", "sgh", 
                      "case_type", "diag_j459", "diag_desc", "exac_via_diag", 
                      "rescue_therapy_code", "exac_via_rescue_thp", "exac_final",
                      "ed_case", "inpatient_case", "outpatient_case")

# Apply criteria-based filters
# criteria 1: Rescue Therapy (N30) given at SHP
exac_2$shp_exac <- ifelse(exac_2$outpatient_case == 1 & exac_2$rescue_therapy_code == 1, 1, 0)
# criteria 2: ED admissions with J459 diagnosis
exac_2$ed_adm_exac <- ifelse(exac_2$ed_case == 1 & exac_2$diag_j459 == 1, 1, 0)
# criteria 3: Inpatient admission with J459 diagnosis
exac_2$inp_adm_exac <- ifelse(exac_2$inpatient_case == 1 & exac_2$diag_j459 == 1, 1, 0)

# Calculate 'severe' and 'severe2' based on criteria
exac_2$severe <- rowSums(exac_2[, c("shp_exac", "ed_adm_exac", "inp_adm_exac")])
exac_2[is.na(exac_2$hydrocort), "hydrocort"] <- 0
exac_2$severe2 <- rowSums(exac_2[, c("shp_exac", "ed_adm_exac", "inp_adm_exac", "hydrocort")])

# Define 'severe_exacerbation' based on 'severe2'
exac_2$severe_exacerbation <- ifelse(exac_2$severe2 >= 1, 1, 0)

# Save the cleaned and processed data
save_data(master, "./int/cleaned_master.rds")
save_data(exac_2, "./int/cleaned_exac.rds")
save_data(visits, "./int/cleaned_visits.rds")

# Merging Data  --------------------------------------------------------
# Unique patients in asthma cohort
asthma_cohort <- unique(exac_2$pid)
save_data(asthma_cohort, "./int/asthma_cohort.rds")

# Filter and rearrange visits
visits_filtered <- visits %>%
  filter(Patient.ID %in% asthma_cohort) %>%
  select(Patient.ID, date, LOS, Institution.Code, Case.Type.Description) %>%
  mutate(
    sgh = ifelse(Institution.Code == "SGH", 1, 0),
    case_ae = ifelse(Case.Type.Description == "A&E", 1, 0),
    case_ip = ifelse(Case.Type.Description == "Inpatient", 1, 0),
    case_others = ifelse(!Case.Type.Description %in% c("Inpatient", "A&E"), 1, 0)
  )

# Reshape the data
melted_data <- melt(visits_filtered, id.vars = c("Patient.ID", "date"), na.rm = TRUE)
visits_reshaped <- dcast(melted_data, Patient.ID + date ~ variable, fun.aggregate = sum, na.rm = TRUE)
save_data(visits_reshaped, "./int/visit_asthma_cohort.rds")

# Check for date overlap and save matching data
date_overlap_check <- ldply(asthma_cohort, check_date_overlap, exac_2, visits_reshaped)

# Save only matched data
matched_data <- ldply(asthma_cohort, keep_data, exac_2, visits_reshaped)
save_data(matched_data, "./int/exacerbation_matched_visit_cases.rds")
save_data(matched_data, "./int/exacerbation_matched_visit_cases.csv", "csv")
