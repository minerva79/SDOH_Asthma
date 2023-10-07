###########################################################################
### Social Determinants Factors (Open Data)                             ###
### 2023-10-07 / Adam Quek (SingHealth)                                 ###
###########################################################################

# Load Libraries ----------------------------------------------------------
required_libraries <- c('plyr', 'dplyr', 'tidyr', 'readr', 'lubridate')
invisible(lapply(required_libraries, require, character=TRUE))


# Custom Function ---------------------------------------------------------
# custom function for sum with missing value
sums <- function(x) sum(x, na.rm=TRUE)



# Reference File and Filepath for OneMap Data -----------------------------
# load reference table for 55 planning areas (in OneMap files) to the 28 postal districts
ref_table <- read.csv("./int/onemap_planning_area.csv")

# get filenames 2015 Household Survey Data and 2020 Population Census Data scraped form OneMap
sdf_filenames <- list.files(path="./rds", pattern="OneMap", full.names=TRUE) %>%
  grep("\\.rds", ., value=TRUE)


# Education Level by District ---------------------------------------------
# file containing ranking of individual education level to edn_val, e.g. pre-primary = 1 to university = 7
edn_meta <- read.csv("./int/onemap_education.csv")

# OneMap file on Education "OneMap_education.rds"
education <- readRDS(sdf_filenames[4])

# adding postal district code based on the planning area
education <- education %>% left_join(ref_table)

# summarise total n for each education level by district (1 - 28) and survey (2015, 2020)
edn_1 <- education %>% select(-planning_area) %>% group_by(District, year) %>% summarise_all(sums)

# get total n for each district (1 - 28) and survey (2015, 2020)
edn_2 <- edn_1 %>% group_by(District, year) %>% transmute(total = pre_primary + primary + secondary + post_secondary + polytechnic + prof_qualification_diploma + university)

# generate for national total for each education level 
edn_nat <- readRDS(sdf_filenames[4]) %>% select(-planning_area) %>% group_by(year) %>% summarise_all(sums)

# Calculate education weight for each district
# convert to long format 
edn_long <- edn_1 %>% gather(education, individual, -District, -year) %>% na.exclude
# merge to get ranking value (edn_val) for each education level
edn_long <- edn_long %>% left_join(edn_meta)
# merge total number of individuals in districts (n1 + n2 + ... + n7)
edn_long <- edn_long %>% left_join(edn_2)
# get weight for each education level: e.g. (edn_val(n1)*n1)/(n1 + n2 +... + n7)
edn_long <- edn_long %>% mutate(education_weight = individual*edn_val/total)

# Repeat steps for national average
edn_nat_long <- edn_nat %>% gather(education, individual, -year)
edn_nat_long <- edn_nat_long %>% left_join(edn_meta)
edn_nat_long <- edn_nat_long %>% left_join(edn_nat_long %>% group_by(year) %>% summarise(total = sums(individual)))
edn_nat_long <- edn_nat_long %>% mutate(education_weight = individual*edn_val/total)
edn_nat_long <- edn_nat_long %>% group_by(year) %>% summarise(district_edn_level = sums(education_weight)) 
edn_nat_long <- edn_nat_long %>% ungroup %>% summarise(district_edn_level = mean(district_edn_level)) %>% mutate(districts = NA)

# convert long format to wide format
edn_wide <- edn_long %>% ungroup %>% select(-c(individual:total)) %>% spread(education, education_weight) 
# add row-wise weight for district educational level for each survey
edn_wide_ref <- edn_wide %>% group_by(District, year) %>% transmute(district_edn_level = pre_primary + primary + secondary + post_secondary + polytechnic + prof_qualification_diploma + university)
# average education level across both survey by district
edn_wide_ref <- edn_wide_ref %>% ungroup %>% 
  mutate(districts = sprintf("%02d", District)) %>% # beautify district by adding leading 0 for district 1-9
  group_by(districts) %>% summarise(district_edn_level = mean(district_edn_level, na.rm=TRUE))

# replace district with missing values with national average
edn_wide_ref <- do.call(rbind, list(edn_wide_ref, edn_nat_long))
edn_wide_ref <- edn_wide_ref %>% 
  mutate(district_edn_level = ifelse(districts %in% c("07", "17", "24", "26", "28"), 
                                     edn_nat_long$district_edn_level,
                                     district_edn_level))

saveRDS(edn_long, "./int/onemap_education_long_format_int.rds")
saveRDS(edn_wide_ref, "./int/onemap_education_by_district.rds")


# Household Income --------------------------------------------------------
# file containing income_val for each income bracket, e.g. below_sgd_1000 = 500
hh_income_meta <- read.csv("./int/onemap_hh_income.csv")
# OneMap file on Education "OneMap_income_hh.rds" 
hh_income <- readRDS(sdf_filenames[6])
hh_income <- hh_income %>% filter(is.na(Result)) # remove blank rows
# adding postal district code based on the planning area
hh_income <- hh_income %>% left_join(ref_table)

# sum each income bracket by district (1 - 28) and survey year (2015, 2020)
hh_income_1 <- hh_income %>% group_by(District, year) %>% 
  select(below_sgd_1000:sgd_9000_to_9999)  %>% summarise_all(sums)
# sum total number of people in district
hh_income_2 <- hh_income %>% group_by(District, year) %>% summarise(total = sums(total))

# repeat at national level
hh_income_nat <- readRDS(sdf_filenames[6]) %>% filter(is.na(Result)) %>% 
  select(-planning_area, -Result) %>% group_by(year) %>% summarise_all(sums)

# Calculate household income for each district
# convert to long format  
hh_income_long <- hh_income_1 %>% gather(income, individual, -District, -year) %>% na.exclude
# merge by income bracket to get income value
hh_income_long <- hh_income_long %>% left_join(hh_income_meta)
# merge by district to get total number of people
hh_income_long <- hh_income_long %>% left_join(hh_income_2)
# get average income for each bracket by year and district - e.g. n1*income_val(n1)/total(n)
hh_income_long <- hh_income_long %>% mutate(hh_income_weight = individual*income_val/total)

# Repeat for national average household income
hh_income_nat_1 <- hh_income_nat %>% gather(income, individual, -year) %>% filter(income != "total")
hh_income_nat_2 <- hh_income_nat %>% gather(income, individual, -year) %>% filter(income == "total") %>% select(-income) %>% rename(total = individual)
hh_income_nat_long <- hh_income_nat_1 %>% left_join(hh_income_meta)
hh_income_nat_long <- hh_income_nat_long %>% left_join(hh_income_nat_2)
hh_income_nat_long <- hh_income_nat_long %>% mutate(hh_income_weight = individual*income_val/total)
hh_income_nat_long <- hh_income_nat_long %>% group_by(year) %>% summarise(district_hh_income_level = sums(hh_income_weight)) 
hh_income_nat_long <- hh_income_nat_long %>% ungroup %>% summarise(district_hh_income_level = mean(district_hh_income_level)) %>% mutate(districts = NA)

# convert long format to wide format
hh_income_wide <- hh_income_long %>% ungroup %>% select(-c(individual:total)) %>% spread(income, hh_income_weight) 
# row-wise sum to get household income each district and year
hh_income_wide_ref <- hh_income_long %>% group_by(District, year) %>% 
  summarise(district_hh_income_level = sums(hh_income_weight)) 
# average household income across both survey by district
hh_income_wide_ref <- hh_income_wide_ref %>% ungroup %>% 
  mutate(districts = sprintf("%02d", District)) %>% 
  group_by(districts) %>% summarise(district_hh_income_level = mean(district_hh_income_level, na.rm=TRUE))

# replace missing district values with national average
hh_income_wide_ref <- do.call(rbind, list(hh_income_wide_ref, hh_income_nat_long))
hh_income_wide_ref <- hh_income_wide_ref %>% 
  mutate(district_hh_income_level = ifelse(districts %in% c("07", "17", "24", "26", "28"), 
                                 hh_income_nat_long$district_hh_income_level,
                                 district_hh_income_level))

saveRDS(hh_income_long, "./int/onemap_hh_income_long_format_int.rds")
saveRDS(hh_income_wide_ref, "./int/onemap_hh_income_by_district.rds")

# working table with derived average social measures
sdf_tab <- data.frame(edn_wide_ref, district_hh_income_level = hh_income_wide_ref$district_hh_income_level)

# Employment --------------------------------------------------------------

# OneMap file on Employment "OneMap_employment.rds"
employment <- readRDS(sdf_filenames[5])
# sum each employment status by planning area; add district; get total n for each planning area
employment <- employment %>% group_by(planning_area, year) %>% select(-gender) %>% summarise_all(sums)
employment <- employment %>% left_join(ref_table)

employment <- employment %>% mutate(total = employed + unemployed + inactive)

# get national average for employed individual/ total n
employment_nat <- readRDS(sdf_filenames[5]) %>% group_by(year) %>% 
  select(-gender, -planning_area) %>% summarise_all(sums)
employment_nat <- employment_nat %>% ungroup %>% 
  mutate(district_employment_level = employed/ (employed + unemployed + inactive)) %>%
  summarise(district_employment_level = mean(district_employment_level)) %>% 
  mutate(districts = NA)

# get total employed and total by district and year
employment_tab <- employment %>% group_by(District, year) %>% summarise_at(vars(employed, total), funs(sums)) %>% na.exclude
# calculate aveage employment rate by district and year
employment_tab_ref <- employment_tab %>% mutate(district_employment_level = employed/total) %>% select(District, year, district_employment_level)
employment_tab_ref <- employment_tab_ref %>% ungroup %>% 
  mutate(districts = sprintf("%02d", District)) %>% 
  group_by(districts) %>% summarise(district_employment_level = mean(district_employment_level, na.rm=TRUE))
# replace missing district values with national average
employment_tab_ref <- do.call(rbind, list(employment_tab_ref, employment_nat))
employment_tab_ref <- employment_tab_ref %>% 
  mutate(district_employment_level = ifelse(districts %in% c("07", "17", "24", "26", "28"), 
                                            employment_nat$district_employment_level,
                                            district_employment_level))

saveRDS(employment_tab, "./int/onemap_employment_long_format_int.rds")
saveRDS(employment_tab_ref, "./int/onemap_employment_by_district.rds")

# working table with derived average social measures
sdf_tab <- data.frame(sdf_tab, district_employment_level = employment_tab_ref$district_employment_level)
saveRDS(sdf_tab, "./int/sdf_data.rds")



# Industry ----------------------------------------------------------------


industry_meta <- read.csv("./int/onemap_industry.csv")
industry <- readRDS(sdf_filenames[8])
industry <- industry %>% left_join(ref_table) %>% na.exclude


industry_1 <- industry %>% group_by(District, year) %>%
  select(-planning_area) %>% summarise_all(sums)

industry_long <- industry_1 %>% gather(industry, individual, -District, -year) %>% na.exclude
industry_2 <- industry_long %>% group_by(District, year) %>% summarise(total = sum(individual))
industry_long <- industry_long %>% left_join(industry_meta)
industry_long <- industry_long %>% group_by(District, year, collar) %>% summarise(individual = sum(individual))
industry_long <- industry_long %>% left_join(industry_2)
industry_wide <- industry_long %>% spread(collar, individual)
industry_wide <- industry_wide %>% mutate(district_blue_collar_level = blue/total)

industry_nat <- readRDS(sdf_filenames[8]) %>% select(-planning_area) %>% 
  group_by(year) %>% summarise_all(sums)
industry_nat_long <- industry_nat %>% gather(industry, individual, -year)
industry_nat_long <- industry_nat_long %>% left_join(industry_meta)  %>% group_by(year, collar) %>% summarise(individual = sum(individual))
industry_nat_wide <- industry_nat_long %>% spread(collar, individual) %>% mutate(total = blue + other + service + white)
industry_nat_wide <- industry_nat_wide %>% mutate(district_blue_collar_level = blue/total)
industry_nat_wide <- industry_nat_wide %>% ungroup %>% summarise(district_blue_collar_level = mean(district_blue_collar_level)) %>% mutate(districts = NA)

industry_wide_ref <- industry_wide %>% ungroup %>% 
  mutate(districts = sprintf("%02d", District)) %>% 
  group_by(districts) %>% summarise(district_blue_collar_level = mean(district_blue_collar_level, na.rm=TRUE))
industry_wide_ref <- do.call(rbind, list(industry_wide_ref, industry_nat_wide))
industry_wide_ref <- industry_wide_ref %>% 
  mutate(district_blue_collar_level = ifelse(districts %in% c("01", "06", "07", "09", "17", "24", "26", "28"), 
                                             industry_nat_wide$district_blue_collar_level,
                                           district_blue_collar_level))

saveRDS(industry_long, "./int/onemap_industry_long_format_int.rds")
saveRDS(industry_wide_ref, "./int/onemap_industry_by_district.rds")


sdf_tab <- data.frame(sdf_tab, district_blue_collar_level = industry_wide_ref$district_blue_collar_level)
saveRDS(sdf_tab, "./int/sdf_data.rds")



# Occupation --------------------------------------------------------------


occupation_meta <- read.csv("./int/onemap_occupation.csv")
occupation <- readRDS(sdf_filenames[12])
occupation <- occupation %>% left_join(ref_table) %>% na.exclude

occupation_1 <- occupation %>% group_by(District, year) %>%
  select(-planning_area) %>% summarise_all(sums)

occupation_long <- occupation_1 %>% gather(occupation, individual, -District, -year) %>% na.exclude
occupation_2 <- occupation_long %>% group_by(District, year) %>% summarise(total = sum(individual))
occupation_long <- occupation_long %>% left_join(occupation_meta)
occupation_long <- occupation_long %>% group_by(District, year, collar) %>% summarise(individual = sum(individual))
occupation_long <- occupation_long %>% left_join(occupation_2)
occupation_wide <- occupation_long %>% spread(collar, individual)
occupation_wide <- occupation_wide %>% mutate(district_blue_occp_collar_level = blue/total)

occupation_nat <- readRDS(sdf_filenames[12]) %>% select(-planning_area) %>% 
  group_by(year) %>% summarise_all(sums)
occupation_nat_long <- occupation_nat %>% gather(occupation, individual, -year)
occupation_nat_long <- occupation_nat_long %>% left_join(occupation_meta)  %>% group_by(year, collar) %>% summarise(individual = sum(individual))
occupation_nat_wide <- occupation_nat_long %>% spread(collar, individual) %>% mutate(total = blue + other + service + white)
occupation_nat_wide <- occupation_nat_wide %>% mutate(district_blue_occp_collar_level = blue/total)
occupation_nat_wide <- occupation_nat_wide %>% ungroup %>% summarise(district_blue_occp_collar_level = mean(district_blue_occp_collar_level)) %>% mutate(districts = NA)

occupation_wide_ref <- occupation_wide %>% ungroup %>% 
  mutate(districts = sprintf("%02d", District)) %>% 
  group_by(districts) %>% summarise(district_blue_occp_collar_level = mean(district_blue_occp_collar_level, na.rm=TRUE))
occupation_wide_ref <- do.call(rbind, list(occupation_wide_ref, occupation_nat_wide))
occupation_wide_ref <- occupation_wide_ref %>% 
  mutate(district_blue_occp_collar_level = ifelse(districts %in% c("01", "06", "07", "09", "17", "24", "26", "28"), 
                                             occupation_nat_wide$district_blue_occp_collar_level,
                                             district_blue_occp_collar_level))

saveRDS(occupation_long, "./int/onemap_occupation_long_format_int.rds")
saveRDS(occupation_wide_ref, "./int/onemap_occupation_by_district.rds")


sdf_tab <- data.frame(sdf_tab, district_blue_occp_collar_level = occupation_wide_ref$district_blue_occp_collar_level)
saveRDS(sdf_tab, "./int/sdf_data.rds")



# Household Size ----------------------------------------------------------

hh_size_meta <- read.csv("./int/onemap_hh_size.csv")
hh_size <- readRDS(sdf_filenames[15])
hh_size <- hh_size %>% left_join(ref_table) %>% na.exclude

hh_size_1 <- hh_size %>% group_by(District, year) %>%
  select(-planning_area) %>% summarise_all(sums)

hh_size_long <- hh_size_1 %>% gather(hh_size, individual, -District, -year) %>% na.exclude
hh_size_2 <- hh_size_long %>% group_by(District, year) %>% summarise(total = sum(individual))
hh_size_long <- hh_size_long %>% left_join(hh_size_meta)
hh_size_long <- hh_size_long %>% left_join(hh_size_2)
hh_size_long <- hh_size_long %>% mutate(hh_size_weight = individual*hh_size_val/total)

hh_size_nat <- readRDS(sdf_filenames[15]) %>% select(-planning_area) %>% 
  group_by(year) %>% summarise_all(sums)
hh_size_nat_long <- hh_size_nat %>% gather(hh_size, individual, -year)
hh_size_nat_long <- hh_size_nat_long %>% left_join(hh_size_meta) 
hh_size_nat_2 <- hh_size_nat_long %>% group_by(year) %>% summarise(total = sum(individual))
hh_size_nat_long <- hh_size_nat_long %>% left_join(hh_size_nat_2)
hh_size_nat_long <- hh_size_nat_long %>% mutate(hh_size_weight = individual*hh_size_val/total)
hh_size_nat_wide <- hh_size_nat_long %>% group_by(year) %>% summarise(district_hh_size = sums(hh_size_weight))
hh_size_nat_wide <- hh_size_nat_wide %>% ungroup %>% summarise(district_hh_size = mean(district_hh_size)) %>% mutate(districts=NA)

hh_size_wide <- hh_size_long %>% group_by(District, year) %>% summarise(district_hh_size = sums(hh_size_weight))
hh_size_wide_ref <- hh_size_wide %>% ungroup %>% 
  mutate(districts = sprintf("%02d", District)) %>% 
  group_by(districts) %>% summarise(district_hh_size = mean(district_hh_size, na.rm=TRUE))
hh_size_wide_ref <- do.call(rbind, list(hh_size_wide_ref, hh_size_nat_wide))
hh_size_wide_ref <- hh_size_wide_ref %>% 
  mutate(district_hh_size = ifelse(districts %in% c("07", "17", "24", "26", "28"), 
                                                  hh_size_nat_wide$district_hh_size,
                                                  district_hh_size))

saveRDS(hh_size_long, "./int/onemap_hh_size_long_format_int.rds")
saveRDS(hh_size_wide_ref, "./int/onemap_hh_size_by_district.rds")


sdf_tab <- data.frame(sdf_tab, district_hh_size = hh_size_wide_ref$district_hh_size)
saveRDS(sdf_tab, "./int/sdf_data.rds")


# Transport - Work --------------------------------------------------------
transport_meta <- read.csv("./int/onemap_transport_work.csv")
transport <- readRDS(sdf_filenames[19])
transport <- transport %>% left_join(ref_table) %>% na.exclude

transport_1 <- transport %>% group_by(District, year) %>%
  select(-planning_area) %>% summarise_all(sums)

transport_long <- transport_1 %>% gather(transport, individual, -District, -year) %>% na.exclude
transport_2 <- transport_long %>% group_by(District, year) %>% summarise(total = sum(individual))
transport_long <- transport_long %>% left_join(transport_meta)
transport_long <- transport_long %>% group_by(District, year, transport_type) %>% summarise(individual = sum(individual))
transport_long <- transport_long %>% left_join(transport_2)
transport_wide <- transport_long %>% spread(transport_type, individual)
transport_wide <- transport_wide %>% mutate(district_transport_public_level = public/total)


transport_nat <- readRDS(sdf_filenames[19]) %>% select(-planning_area) %>% 
  group_by(year) %>% summarise_all(sums)
transport_nat_long <- transport_nat %>% gather(transport, individual, -year)
transport_nat_long <- transport_nat_long %>% left_join(transport_meta) 
transport_nat_long <- transport_nat_long %>% group_by(year, transport_type) %>% summarise(individual = sum(individual))
transport_nat_2 <- transport_nat_long %>% group_by(year) %>% summarise(total = sum(individual))
transport_nat_long <- transport_nat_long %>% left_join(transport_nat_2)
transport_nat_wide <- transport_nat_long %>% spread(transport_type, individual)
transport_nat_wide <- transport_nat_wide %>% mutate(district_transport_public_level = public/total)
transport_nat_wide <- transport_nat_wide %>% ungroup %>% summarise(district_transport_public_level = mean(district_transport_public_level)) %>% mutate(districts = NA)


transport_wide_ref <-transport_wide %>% ungroup %>% 
  mutate(districts = sprintf("%02d", District)) %>% 
  group_by(districts) %>% summarise(district_transport_public_level = mean(district_transport_public_level, na.rm=TRUE))
transport_wide_ref <- do.call(rbind, list(transport_wide_ref, transport_nat_wide))
transport_wide_ref <- transport_wide_ref %>% 
  mutate(district_transport_public_level = ifelse(districts %in% c("07", "17", "24", "26", "28"), 
                                   transport_nat_wide$district_transport_public_level,
                                   district_transport_public_level))

saveRDS(transport_long, "./int/onemap_transport_long_format_int.rds")
saveRDS(transport_wide_ref, "./int/onemap_transport_by_district.rds")


sdf_tab <- data.frame(sdf_tab, district_transport_public_level = transport_wide_ref$district_transport_public_level)
saveRDS(sdf_tab, "./int/sdf_data.rds")



# Religion ----------------------------------------------------------------


religion <- readRDS(sdf_filenames[14])
religion <- religion %>% left_join(ref_table)

religion_1 <- religion %>% group_by(District, year) %>% 
  select(-planning_area) %>% summarise_all(sums) %>% na.exclude
religion_2 <- religion_1 %>% gather(var, val, -District, - year) %>% 
  group_by(District, year) %>% summarise(total = sum(val))

religion_nat <- readRDS(sdf_filenames[14]) %>% group_by(year) %>% select(-planning_area) %>% summarise_all(sums)
religion_nat_2 <- religion_nat %>% gather(var, val,- year) %>% 
  group_by(year) %>% summarise(total = sum(val))
religion_nat <- religion_nat %>% mutate(buddhism_taoism = buddhism + taoism) %>% left_join(religion_nat_2)
religion_nat_wide <- religion_nat %>% group_by(year) %>% summarise(district_buddhism_taoism_level = buddhism_taoism/total)
religion_nat_wide <- religion_nat_wide %>% ungroup %>% summarise(district_buddhism_taoism_level = mean(district_buddhism_taoism_level)) %>% mutate(districts=NA)

religion_tab <- religion_1 %>% left_join(religion_2) 
religion_tab <- religion_tab %>% mutate(buddhism_taoism = buddhism + taoism)
religion_tab_ref <- religion_tab %>% ungroup %>% 
  select(District, year, buddhism_taoism, total) %>% mutate(district_buddhism_taoism_level = buddhism_taoism/total)

religion_tab_ref <-religion_tab_ref %>% ungroup %>% 
  mutate(districts = sprintf("%02d", District)) %>% 
  group_by(districts) %>% summarise(district_buddhism_taoism_level = mean(district_buddhism_taoism_level, na.rm=TRUE))
religion_tab_ref <- do.call(rbind, list(religion_tab_ref, religion_nat_wide))
religion_tab_ref <- religion_tab_ref %>% 
  mutate(district_buddhism_taoism_level = ifelse(districts %in% c("07", "17", "24", "26", "28"), 
                                                 religion_nat_wide$district_buddhism_taoism_level,
                                                  district_buddhism_taoism_level))


saveRDS(religion_tab, "./int/onemap_religion_long_format_int.rds")
saveRDS(religion_tab_ref, "./int/onemap_religion_by_district.rds")

sdf_tab <- data.frame(sdf_tab, district_buddhism_taoism_level = religion_tab_ref$district_buddhism_taoism_level)
saveRDS(sdf_tab, "./int/sdf_data.rds")
 