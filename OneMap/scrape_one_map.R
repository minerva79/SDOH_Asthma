token <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOjkxNjEsInVzZXJfaWQiOjkxNjEsImVtYWlsIjoiYWRhbTI1MDk3OUBnbWFpbC5jb20iLCJmb3JldmVyIjpmYWxzZSwiaXNzIjoiaHR0cDpcL1wvb20yLmRmZS5vbmVtYXAuc2dcL2FwaVwvdjJcL3VzZXJcL3Nlc3Npb24iLCJpYXQiOjE2ODAzMzEzOTYsImV4cCI6MTY4MDc2MzM5NiwibmJmIjoxNjgwMzMxMzk2LCJqdGkiOiI0Zjg2MjVkY2JhOTVmODU3ZjczMjE3NGU0ODgyNGQ2MyJ9.gal_5Y9HxqGxDS0xOpnwN6tJIXNvO_vZY6TFNdc8yrI"

lib <- c("plyr", "tidyverse", "httr", "jsonlite")
invisible(lapply(lib, require, character=TRUE))
rm(lib)


# Get Planning Areas ------------------------------------------------------

get_planning_areas <- function(api_key) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getPlanningareaNames?"
  query <- paste0(base_url, "token=", api_key, "&otptFlds=PLN_AREA_N,REGION_N,LATITUDE,LONGITUDE")
  
  response <- GET(query)
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

plan_areas <- get_planning_areas(token)
plan_areas$area <- gsub(" ", "%20", plan_areas$pln_area_n)

saveRDS(plan_areas, "./int/OneMap_planning_areas.rds")
write_csv(plan_areas, "./int/OneMap_planning_areas.csv", na="")


# Get Economic Status -----------------------------------------------------

get_economic_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getEconomicStatus?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year, "&gender=", gender)
  
  response <- GET(query)
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

e101 <- ldply(plan_areas$area, function(x) get_economic_status(token, x, 2015, "male"))
e102 <- ldply(plan_areas$area, function(x) get_economic_status(token, x, 2015, "female"))
e103 <- ldply(plan_areas$area, function(x) get_economic_status(token, x, 2020, "male"))
e104 <- ldply(plan_areas$area, function(x) get_economic_status(token, x, 2020, "female"))

employment <- do.call(rbind, list(e101, e102, e103, e104))

saveRDS(employment, "./int/OneMap_employment.rds")
write_csv(employment, "./int/OneMap_employment.csv", na="")


# Get Education Status ----------------------------------------------------

get_education_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getEducationAttending?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)

  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

e105 <- ldply(plan_areas$area, function(x) get_education_status(token, x, 2015))
e107 <- ldply(plan_areas$area, function(x) get_education_status(token, x, 2020))

education <- do.call(rbind, list(e105, e107))

saveRDS(education, "./int/OneMap_education.rds")
write_csv(education, "./int/OneMap_education.csv", na="")


# Work Income (Household) -------------------------------------------------


get_income_hh_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getHouseholdMonthlyIncomeWork?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_income_hh_status(token, "BEDOK", 2010)

e109 <- ldply(plan_areas$area, function(x) get_income_hh_status(token, x, 2015))
e110 <- ldply(plan_areas$area, function(x) get_income_hh_status(token, x, 2020))

income_hh <- do.call(rbind, list(e109, e110))

saveRDS(income_hh, "./int/OneMap_income_hh.rds")
write_csv(income_hh, "./int/OneMap_income_hh.csv", na="")


# Household Size ----------------------------------------------------------

get_size_hh_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getHouseholdSize?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_size_hh_status(token, "BEDOK", 2010)

e111 <- ldply(plan_areas$area, function(x) get_size_hh_status(token, x, 2015))
e112 <- ldply(plan_areas$area, function(x) get_size_hh_status(token, x, 2020))

size_hh <- do.call(rbind, list(e111, e112))

saveRDS(size_hh, "./int/OneMap_size_hh.rds")
write_csv(size_hh, "./int/OneMap_size_hh.csv", na="")


# Household structure -----------------------------------------------------

get_strcuture_hh_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getHouseholdStructure?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_strcuture_hh_status(token, "BEDOK", 2010)

e113 <- ldply(plan_areas$area, function(x) get_strcuture_hh_status(token, x, 2015))
e114 <- ldply(plan_areas$area, function(x) get_strcuture_hh_status(token, x, 2020))

structure_hh <- do.call(rbind, list(e113, e114))

saveRDS(structure_hh, "./int/OneMap_structure_hh.rds")
write_csv(structure_hh, "./int/OneMap_structure_hh.csv", na="")


e101 <- ldply(2015:2021, function(x)get_economic_status(token, plan_areas$area[1], x, "male"))
e102 <- ldply(2015:2021, function(x)get_economic_status(token, plan_areas$area[1], x, "female"))


# Income from Work --------------------------------------------------------

get_income_work_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getIncomeFromWork?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_income_work_status(token, "BEDOK", 2010)

e115 <- ldply(plan_areas$area, function(x) get_income_work_status(token, x, 2015))
e116 <- ldply(plan_areas$area, function(x) get_income_work_status(token, x, 2020))

income_work <- ldply(list(e115, e116))

saveRDS(income_work, "./int/OneMap_income_work.rds")
write_csv(income_work, "./int/OneMap_income_work.csv", na="")


e101 <- ldply(2015:2021, function(x)get_economic_status(token, plan_areas$area[1], x, "male"))
e102 <- ldply(2015:2021, function(x)get_economic_status(token, plan_areas$area[1], x, "female"))


# Industry Population -----------------------------------------------------


get_industry_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getIndustry?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_industry_status(token, "BEDOK", 2010)

e117 <- ldply(plan_areas$area, function(x) get_industry_status(token, x, 2015))
e118 <- ldply(plan_areas$area, function(x) get_industry_status(token, x, 2020))

industry <- ldply(list(e117, e118))

saveRDS(industry, "./int/OneMap_industry.rds")
write_csv(industry, "./int/OneMap_industry.csv", na="")



# Language Literacy Data --------------------------------------------------

get_lang_lit_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getLanguageLiterate?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_lang_lit_status(token, "BEDOK", 2010)

e119 <- ldply(plan_areas$area, function(x) get_lang_lit_status(token, x, 2015))
e120 <- ldply(plan_areas$area, function(x) get_lang_lit_status(token, x, 2020))

lang_lit <- ldply(list(e119, e120))

saveRDS(lang_lit, "./int/OneMap_lang_lit.rds")
write_csv(lang_lit, "./int/OneMap_lang_lit.csv", na="")



# Marital Status Data -----------------------------------------------------


get_marital_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getMaritalStatus?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_marital_status(token, "BEDOK", 2015)

e121 <- ldply(plan_areas$area, function(x) get_marital_status(token, x, 2015))
e122 <- ldply(plan_areas$area, function(x) get_marital_status(token, x, 2020))

marital <- ldply(list(e121, e122))

saveRDS(marital, "./int/OneMap_marital.rds")
write_csv(marital, "./int/OneMap_marital.csv", na="")



# Mode of Transports to School Data ---------------------------------------

get_transport_sch_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getModeOfTransportSchool?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_transport_sch_status(token, "BEDOK", 2015)

e123 <- ldply(plan_areas$area, function(x) get_transport_sch_status(token, x, 2015))
e124 <- ldply(plan_areas$area, function(x) get_transport_sch_status(token, x, 2020))

transport_sch <- ldply(list(e123, e124))

saveRDS(transport_sch, "./int/OneMap_transport_sch.rds")
write_csv(transport_sch, "./int/OneMap_transport_sch.csv", na="")


# Mode of Transport to Work Data ------------------------------------------

get_transport_wrk_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getModeOfTransportWork?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_transport_wrk_status(token, "BEDOK", 2015)

e125 <- ldply(plan_areas$area, function(x) get_transport_wrk_status(token, x, 2015))
e126 <- ldply(plan_areas$area, function(x) get_transport_wrk_status(token, x, 2020))

transport_wrk <- ldply(list(e125, e126))

saveRDS(transport_wrk, "./int/OneMap_transport_wrk.rds")
write_csv(transport_wrk, "./int/OneMap_transport_wrk.csv", na="")


# Occupation Data ---------------------------------------------------------

get_occupation_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getOccupation?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_occupation_status(token, "BEDOK", 2015)

e127 <- ldply(plan_areas$area, function(x) get_occupation_status(token, x, 2015))
e128 <- ldply(plan_areas$area, function(x) get_occupation_status(token, x, 2020))

occupation <- ldply(list(e127, e128))

saveRDS(occupation, "./int/OneMap_occupation.rds")
write_csv(occupation, "./int/OneMap_occupation.csv", na="")


# Age Data ----------------------------------------------------------------

get_age_gp_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getPopulationAgeGroup?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_age_gp_status(token, "BEDOK", 2015)

e129 <- ldply(plan_areas$area, function(x) get_age_gp_status(token, x, 2015))
e130 <- ldply(plan_areas$area, function(x) get_age_gp_status(token, x, 2020))

age_gp <- ldply(list(e129, e130))

saveRDS(age_gp, "./int/OneMap_age_gp.rds")
write_csv(age_gp, "./int/OneMap_age_gp.csv", na="")


# Religion Data -----------------------------------------------------------

get_religion_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getReligion?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_religion_status(token, "BEDOK", 2015)

e131 <- ldply(plan_areas$area, function(x) get_religion_status(token, x, 2015))
e132 <- ldply(plan_areas$area, function(x) get_religion_status(token, x, 2020))

religion <- ldply(list(e131, e132))

saveRDS(religion, "./int/OneMap_religion.rds")
write_csv(religion, "./int/OneMap_religion.csv", na="")


# Spoken Language Data ----------------------------------------------------


get_lang_spk_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getSpokenAtHome?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_lang_spk_status(token, "BEDOK", 2015)

e133 <- ldply(plan_areas$area, function(x) get_lang_spk_status(token, x, 2015))
e134 <- ldply(plan_areas$area, function(x) get_lang_spk_status(token, x, 2020))

lang_spk <- ldply(list(e133, e134))

saveRDS(lang_spk, "./int/OneMap_lang_spk.rds")
write_csv(lang_spk, "./int/OneMap_lang_spk.csv", na="")


# Tenancy Data ------------------------------------------------------------

get_tenancy_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getTenancy?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_tenancy_status(token, "BEDOK", 2015)

e135 <- ldply(plan_areas$area, function(x) get_tenancy_status(token, x, 2015))
e136 <- ldply(plan_areas$area, function(x) get_tenancy_status(token, x, 2020))

tenancy <- ldply(list(e135, e136))

saveRDS(tenancy, "./int/OneMap_tenancy.rds")
write_csv(tenancy, "./int/OneMap_tenancy.csv", na="")


# Dwelling Type Household Data --------------------------------------------

get_dwelling_hh_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getTypeOfDwellingHousehold?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_dwelling_hh_status(token, "BEDOK", 2015)

e137 <- ldply(plan_areas$area, function(x) get_dwelling_hh_status(token, x, 2015))
e138 <- ldply(plan_areas$area, function(x) get_dwelling_hh_status(token, x, 2020))

dwelling_hh <- ldply(list(e137, e138))

saveRDS(dwelling_hh, "./int/OneMap_dwelling_hh.rds")
write_csv(dwelling_hh, "./int/OneMap_dwelling_hh.csv", na="")


# Dwelling Type Population Data -------------------------------------------

get_dwelling_pop_status <- function(api_key, planning_area, year, gender) {
  base_url <- "https://developers.onemap.sg/privateapi/popapi/getTypeOfDwellingPop?"
  query <- paste0(base_url, "token=", api_key, "&planningArea=", planning_area, "&year=", year)
  
  response <- GET(query)
  
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    df <- data.frame(data)
    return(df)
  } else {
    cat("Error:", response$status_code, "\n")
    return(NULL)
  }
}

get_dwelling_pop_status(token, "BEDOK", 2015)

e139 <- ldply(plan_areas$area, function(x) get_dwelling_pop_status(token, x, 2015))
e140 <- ldply(plan_areas$area, function(x) get_dwelling_pop_status(token, x, 2020))

dwelling_pop <- ldply(list(e139, e140))

saveRDS(dwelling_pop, "./int/OneMap_dwelling_pop.rds")
write_csv(dwelling_pop, "./int/OneMap_dwelling_pop.csv", na="")








