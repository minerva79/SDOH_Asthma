# Dependencies ------------------------------------------------------------

lib <- c('sf', 'plyr', 'tidyverse', 'lubridate', 'scales', 'RColorBrewer',
         'httr', 'jsonlite', 'geosphere', 'Boruta')
invisible(lapply(lib, require, character.only = TRUE))
rm(lib)

singapore <- readRDS("./rds/singapore.rds")

# Custom Function ---------------------------------------------------------

## 2.2 Searching for longitude and latitude from OneMap API ##
onemap_api <- function(address) {
  
  # function to return lat-long based on address search on OneMap #
  
  url <- modify_url(
    "https://developers.onemap.sg/commonapi/search",
    query = list(searchVal = address,
                 returnGeom = "Y", 
                 getAddrDetails = "Y"))
  
  returned <- GET(url)
  
  returned_txt <- fromJSON(content(returned, "text"),
                           simplifyVector = FALSE)
  
  data.frame(latitude = returned_txt$results[[1]]$LATITUDE,
             longitude = returned_txt$results[[1]]$LONGITUDE)
  
}



# Scraping Lat-Long from Postal Code --------------------------------------

onemap_api(singapore$postal_code[5])

output_list <- ldply(seq_len(nrow(singapore)), function(x){
  tryCatch({
    onemap_api(singapore$postal_code[x])
  }, error = function(e) {
    message(paste0("Skipping input ", x, " due to error: ", e$message))
    return(data.frame(latitude = NA, longitude = NA))
  })
})

singapore <- singapore %>% cbind(output_list)
saveRDS(singapore, "./rds/singapore_v2.rds")
