library(readxl)
library(dplyr)
library(tidycensus)
library(usdata)
census_api_key("7e83aa1d195fd7cd921e4ac747998c618f05460d")


# getting counties population from the American Community Survey (ACS) 2020
county_pop = get_acs(geography = "county",
                     variable = "B01001_001",
                     geometry = FALSE) %>%
  mutate(state = gsub(pattern = ".*County, |.*Municipio, |.*Parish, |.*Borough, |.*Area, ",
                      replacement = "",
                      x = NAME),
         state_abbr = state2abbr(state),
         state_abbr = ifelse(state == "Puerto Rico", "PR", state_abbr),
         fips_code = as.numeric(GEOID)) %>%
  select(fips_code,
         state = state_abbr,
         pop_2020 = estimate)




# read NCHS Urban-Rural Classification Scheme for Counties Data 2013
UR_class_file = read_excel("RawData/NCHSURCodes2013.xlsx") 

# clean data 
UR_class = UR_class_file %>%
  select(fips_code = 'FIPS code',
         state = "State Abr.",
         UR_code = "2013 code") %>%
  mutate(UR_category = case_when(UR_code == 1 ~ "Large central metro",
                                 UR_code == 2 ~ "Large fringe metro",
                                 UR_code == 3 ~ "Medium metro",
                                 UR_code == 4 ~ "Small metro",
                                 UR_code == 5 ~ "Micropolitan",
                                 UR_code == 6 ~ "Noncore"),
         UR_category = factor(UR_category,
                              levels = c("Large central metro",
                                         "Large fringe metro",
                                         "Medium metro",
                                         "Small metro",
                                         "Micropolitan",
                                         "Noncore")
                              )
         )




# read percent rural and urban in 2010 by counties from cencus.gov 
pctUR_file = read_excel("RawData/PctUrbanRural_County.xls")

pctUR = pctUR_file %>% 
  mutate(fips_code = as.numeric(paste(STATE, COUNTY, sep = "")),
         state = state2abbr(STATENAME)) %>%
  select(fips_code,
         state,
         POPPCT_URBAN,
         POPPCT_RURAL,
         POP_URBAN,
         POP_RURAL)






# merge county population and NCHS category and urbon percentage
county.NCHS = UR_class %>%
  left_join(pctUR, by = c("fips_code", "state")) %>%
  left_join(county_pop, by = c("fips_code", "state"))


save(county.NCHS,
     file = "ProcessedData/county.NCHS.RDA")





