library(data.table)
library(dplyr)
library(usdata)


# Load CDC, covidestimates and NCHS Data
estimates_data = fread("RawData/estimates.csv")
load("ProcessedData/county.NCHS.RDA")
CDC_data = fread("RawData/United_States_COVID-19_County_Level_of_Community_Transmission_Historical_Changes_-_ARCHIVED.csv")





#clean estimate data
estimates = estimates_data %>%
  dplyr::select(
    date,
    fips,
    #Estimate of the effective reproductive number (Rt)
    "Rt" = r_t
  ) %>%
  mutate(
    date = as.Date(date, "%y/%m/%d")
  )




# Clean CDC data
CDC = CDC_data %>%
  dplyr::select(
    date,
    "fips" = fips_code,
    "county" = county_name,
    "state" = state_name,
    community_transmission_level
  ) %>%
  filter(community_transmission_level != "")%>%
  mutate(
    date = as.Date(date, "%m/%d/%Y"),
    community_transmission_level = factor(community_transmission_level,
                                          level=c("low", "moderate", "substantial", "high"))
  )



# clean NCHS Data
NCHS = county.NCHS %>%
  dplyr::select(
    "fips" = fips_code,
    state,
    UR_code,
    UR_category
  ) %>%
  dplyr::mutate(
    state = abbr2state(state)
  )


data = estimates %>%
  inner_join(CDC, by=c("date", "fips")) %>%
  left_join(NCHS, by=c("state", "fips")) %>%
  group_by(county, state) %>%
  mutate(
    Rt_NextWeek = lead(Rt, 1),
    Rt_1prevWeek = lag(Rt, 1),
    Rt_2prevWeek = lag(Rt, 2),
    Rt_3prevWeek = lag(Rt, 3),
    risk_level = case_when(community_transmission_level == "low"  ~ 1,
                           community_transmission_level == "moderate"  ~ 2,
                           community_transmission_level == "substantial"  ~ 3,
                           community_transmission_level == "high"  ~ 4),
    stateFips = paste0(state, fips)
  )


dates = sort(unique(data$date))

columnNames = c("date",
                "Rt",
                "community_transmission_level",
                "risk_level",
                "UR_code",
                "UR_category",
                "stateFips",
                "Rt_NextWeek",
                "Rt_1prevWeek",
                "Rt_2prevWeek",
                "Rt_3prevWeek",
                "target_UR",
                "target_county",
                "target_risk",
                "target_rt",
                "target_rt_NextWeek",
                "target_rt_1prevWeek",
                "target_rt_2prevWeek",
                "target_rt_3prevWeek"
                )




for(date_index in 1:length(dates)){
  
  # define and empty dataframe
  compared_counties = data.frame(matrix(nrow = 0,
                                        ncol=length(columnNames)))
  
  # all counties in "date_index" specific day
  data_day = dplyr::filter(data,
                           date == dates[date_index])
  
  
  for(i in 1:nrow(data_day)){
    # choose the counties one by one
    county = data_day[i,]
    target_UR = county$UR_code
    target_county = county$stateFips
    target_risk = county$risk_level
    target_rt = county$Rt
    target_rt_NextWeek = county$Rt_NextWeek
    target_rt_1prevWeek = county$Rt_1prevWeek
    target_rt_2prevWeek = county$Rt_2prevWeek
    target_rt_3prevWeek = county$Rt_3prevWeek
    
    # filter counties with different Risk level and the same UR code
    other_counties = data_day[i:nrow(data_day),] %>%
      dplyr::filter(
        # different CDC risk level
        risk_level != target_risk,
        # Same UR code
        UR_code == target_UR
      )%>%
      dplyr::mutate(
        target_UR = target_UR,
        target_county = target_county,
        target_risk = target_risk,
        target_rt = target_rt,
        target_rt_NextWeek = target_rt_NextWeek,
        target_rt_1prevWeek = target_rt_1prevWeek,
        target_rt_2prevWeek = target_rt_2prevWeek,
        target_rt_3prevWeek = target_rt_3prevWeek
        )
    
    
    compared_counties = rbind(compared_counties, other_counties)
    
  }
  
  # save the all possible pair counties for each day in a separate file
  save(compared_counties, 
       file=paste0("ProcessedData/outputsample_covidEstim/",
                   date_index,
                   ".RDA"))
}





