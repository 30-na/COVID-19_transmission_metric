
# Load Packages
library(dplyer)
library(data.table)
library(ggplot2)
library(httr)



# Get data from CovidActNow API "Historic data for all counties"
url <- 'https://api.covidactnow.org/v2/counties.timeseries.csv' 
api_key <- "16f7df99ed364b908d596b49cebdc8b5"

params <- list(
  apiKey = api_key
  )

# It is 1G data and may take around 15 minutes to be downloaded 
response <- httr::GET(url, query = params)
# If the status_code is 200, this means that the request was successful
# and we can proceed to parse the response data
if (status_code(response) == 200) {
  covid_data <- fread(
    httr::content(response, as = "text"),
    encoding = "UTF-8"
    )
}


save(covid_data,
     file = "RawData/CovidActNow.rda")
