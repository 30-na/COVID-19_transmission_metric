# load library
library(dplyr)
library(data.table)
library(usdata)

# load NCHS Data
load("ProcessedData/county.NCHS.RDA")

# load United States COVID-19 County Level of Community Transmission Historical Changes - ARCHIVED
counties.transmission.file = data.table::fread("RawData/United_States_COVID-19_County_Level_of_Community_Transmission_Historical_Changes_-_ARCHIVED.csv") 
counties.transmission =  counties.transmission.file %>%
  dplyr::mutate(
    date = as.Date(date, "%m/%d/%Y"),
    state = state2abbr(state_name),
    cases_per_100K_numeric = as.numeric(cases_per_100K_7_day_count_change)
    ) %>%
  dplyr::select(
    state,
    fips_code, 
    county_name,
    date,
    cases_per_100K_numeric,
    cases_per_100K_7_day_count_change,
    community_transmission_level
    )


# time interval
startDate = "2021-08-06"
endDate = "2022-10-18"



# merged datas
counties.transmission.newCase = counties.transmission %>%
  left_join(
    county.NCHS, by=c("state", "fips_code")
    ) %>%
  dplyr::mutate(
    confirm = as.integer(cases_per_100K_numeric/100000*pop_2020),
    # When the case counts used to calculate the total new case rate
    # metric ("cases_per_100K_7_day_count_change") is greater than zero
    # and less than 10, this metric is set to "suppressed" to protect 
    # individual privacy. If the case count is 0, the total new case rate metric
    # is still displayed.
    # 5 replaced for suppressed values
    confirm = ifelse(cases_per_100K_7_day_count_change == "suppressed", 5, confirm)
  )%>%
  dplyr::filter(
    date < endDate & date > startDate
    )%>%
  dplyr::select(
    state,
    fips_code,
    county_name,
    date,
    cases_per_100K_numeric,
    confirm,
    UR_code,
    UR_category,
    community_transmission_level
  )
  
  
save(counties.transmission.newCase,
     file="ProcessedData/counties.transmission.newCase.RDA")

save(counties.transmission.newCase,
     file="ColabData/counties.transmission.newCase.RDA")



