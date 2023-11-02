library(rvest)
library(dplyr)

# crawling all postal codes from https://www.getpostalcodes.com/singapore/ 

central_districts <- c("01", "02", "03", "04", "06", "07")
northeast_districts <- c("19", "20", "25", "26", "27", "28")
northwest_districts <- c("05", "21", "22", "23", "24")
southeast_districts <- c("13", "14", "15", "16", "17", "18")
southwest_districts <- c("08", "09", "10", "11", "12")

for(i in central_districts){
  url <- paste0("https://www.getpostalcodes.com/singapore/place-singapore-", i, "/")
  page <- read_html(url)
  
  # Find the table containing the postal codes
  postal_code_table <- page %>%
    html_nodes("table.table-bordered") %>%
    .[[1]]
  
  # Extract the Postal Code data
  postal_codes <- postal_code_table %>%
    html_nodes("td") %>%
    html_text() %>%
    str_extract_all("[0-9]{6}") %>%
    unlist()  
  
  assign(paste0("cn_d", i), postal_codes)
}
for(i in northeast_districts){
  url <- paste0("https://www.getpostalcodes.com/singapore/place-singapore-", i, "/")
  page <- read_html(url)
  
  # Find the table containing the postal codes
  postal_code_table <- page %>%
    html_nodes("table.table-bordered") %>%
    .[[1]]
  
  # Extract the Postal Code data
  postal_codes <- postal_code_table %>%
    html_nodes("td") %>%
    html_text() %>%
    str_extract_all("[0-9]{6}") %>%
    unlist()  
  
  assign(paste0("ne_d", i), postal_codes)
}
for(i in northwest_districts){
  url <- paste0("https://www.getpostalcodes.com/singapore/place-singapore-", i, "/")
  page <- read_html(url)
  
  # Find the table containing the postal codes
  postal_code_table <- page %>%
    html_nodes("table.table-bordered") %>%
    .[[1]]
  
  # Extract the Postal Code data
  postal_codes <- postal_code_table %>%
    html_nodes("td") %>%
    html_text() %>%
    str_extract_all("[0-9]{6}") %>%
    unlist()  
  
  assign(paste0("nw_d", i), postal_codes)
}
for(i in southeast_districts){
  url <- paste0("https://www.getpostalcodes.com/singapore/place-singapore-", i, "/")
  page <- read_html(url)
  
  # Find the table containing the postal codes
  postal_code_table <- page %>%
    html_nodes("table.table-bordered") %>%
    .[[1]]
  
  # Extract the Postal Code data
  postal_codes <- postal_code_table %>%
    html_nodes("td") %>%
    html_text() %>%
    str_extract_all("[0-9]{6}") %>%
    unlist()  
  
  assign(paste0("se_d", i), postal_codes)
}
for(i in southwest_districts){
  url <- paste0("https://www.getpostalcodes.com/singapore/place-singapore-", i, "/")
  page <- read_html(url)
  
  # Find the table containing the postal codes
  postal_code_table <- page %>%
    html_nodes("table.table-bordered") %>%
    .[[1]]
  
  # Extract the Postal Code data
  postal_codes <- postal_code_table %>%
    html_nodes("td") %>%
    html_text() %>%
    str_extract_all("[0-9]{6}") %>%
    unlist()  
  
  assign(paste0("sw_d", i), postal_codes)
}

central_postcodes <- lapply(paste0("cn_d", central_districts), get) %>% setNames(central_districts)
northeast_postcodes <- lapply(paste0("ne_d", northeast_districts), get) %>% setNames(northeast_districts)
northwest_postcodes <- lapply(paste0("nw_d", northwest_districts), get) %>% setNames(northwest_districts)
southeast_postcodes <- lapply(paste0("se_d", southeast_districts), get) %>% setNames(southeast_districts)
southwest_postcodes <- lapply(paste0("sw_d", southwest_districts), get) %>% setNames(southwest_districts)

rm(list = ls()[!grepl("_postcodes", ls())])

saveRDS(central_postcodes, "./rds/central_postcodes.rds")
saveRDS(northeast_postcodes, "./rds/northeast_postcodes.rds")
saveRDS(northwest_postcodes, "./rds/northwest_postcodes.rds")
saveRDS(southeast_postcodes, "./rds/southeast_postcodes.rds")
saveRDS(southwest_postcodes, "./rds/southwest_postcodes.rds")

central <- ldply(seq_len(length(central_postcodes)), 
       function(x){
         data.frame(region = "central", 
                    districts = names(central_postcodes)[x],
                    postal_code = central_postcodes[[x]])
         })

northeast <- ldply(seq_len(length(northeast_postcodes)), 
                 function(x){
                   data.frame(region = "northeast", 
                              districts = names(northeast_postcodes)[x],
                              postal_code = northeast_postcodes[[x]])
                 })

northwest <- ldply(seq_len(length(northwest_postcodes)), 
                 function(x){
                   data.frame(region = "northwest", 
                              districts = names(northwest_postcodes)[x],
                              postal_code = northwest_postcodes[[x]])
                 })

southeast <- ldply(seq_len(length(southeast_postcodes)), 
                 function(x){
                   data.frame(region = "southeast", 
                              districts = names(southeast_postcodes)[x],
                              postal_code = southeast_postcodes[[x]])
                 })

southwest <- ldply(seq_len(length(southwest_postcodes)), 
                 function(x){
                   data.frame(region = "southwest", 
                              districts = names(southwest_postcodes)[x],
                              postal_code = southwest_postcodes[[x]])
                 })


singapore <- do.call(rbind, list(central, northeast, northwest, southeast, southwest))
saveRDS(singapore, "./rds/singapore.rds")
