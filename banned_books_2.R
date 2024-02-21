#------------------------------------#
# Banned Books Project
# Jan 2024
# Aura Walmer
#------------------------------------#

# Set Up
library(readxl)
library(purrr)
library(tidyverse)
library(dplyr)
library(stringr)
library(plyr)
library(data.table)
library(tidyr)
setwd("/Volumes/AuraByte3/Data Projects/Banned Books in U.S.")

# Data Import
data_tm <- read_excel("Main_Table 1_8_2024.xlsx")

# Initial Exploration:
#table(data_tm$Challenge_Type, useNA = "ifany")
#table(data_tm$Decision, useNA = "ifany")
table(data_tm$Library_Type, useNA = "ifany") # Public: 994; School: 11037; NA: 171
table(data_tm$Year, useNA = "ifany") # 2021: 1,784; 2022: 4,276; 2023: 5,427; 2223: 154 (need to clean); NA: 561

## Distill down categories?
# (1) Banned (2) Overturned (3) Unresolved (4) Unrelated Removal (5) Unknown
decision_mapping <- aggregate(data.frame(Freq = data_tm$Decision),
                              list(Decision = data_tm$Decision), length)
decision_mapping[nrow(decision_mapping)+1,] <- c("NA",1499) # number of actual missing

banned_i <- c(2:4,8,9)
overturned_i <-c(5,10:14)
unresolved_i <- c(7,15,16)
unrelated_i <- c(18:20)
unkown_i <- c(1,6,17,21)

decision_mapping$Decision_Simplified <- ""
decision_mapping[banned_i,"Decision_Simplified"] <- c(rep("Banned", length(banned_i)))
decision_mapping[overturned_i,"Decision_Simplified"] <- c(rep("Overturned", length(overturned_i)))
decision_mapping[unresolved_i,"Decision_Simplified"] <- c(rep("Unresolved", length(unresolved_i)))
decision_mapping[unrelated_i,"Decision_Simplified"] <- c(rep("Unrelated Removal", length(unrelated_i)))
decision_mapping[unkown_i,"Decision_Simplified"] <- c(rep("Unknown", length(unkown_i)))

decision_mapping <- decision_mapping[order(decision_mapping$Decision_Simplified),]

# Send decision map to TM:
write.csv(decision_mapping,"bookchallenge_decisionmap.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

# Cleaning:
# Fix year typos:
data_tm$Year_2 <- replace(data_tm$Year, data_tm$Year==2223, 2023)
# Add version of Overseeing Agency where 'ISD' --> 'Independent School District'
data_tm$Overseeing_Agency_ISD_full <- str_to_title(str_replace(data_tm$Overseeing_Agency, 'ISD', 'Independent School District'))
  
## County Information:
nrow(data_tm[is.na(data_tm$County),]) # 5,070
sum(is.na(data_tm$County)) # same functionality, sleeker
# Need county data to match (1) public school districts & (2) public libraries
sch_dist_county <- read.csv("U.S. County Data/sdlist-23_PublicSchoolDistrictData.csv", skip = 2)
pub_lib_county <- read.csv("U.S. County Data/PLS_FY21_AE_pud21i_PublicLibraryData.csv")
# Prepare vars for merge:
  # school libs
  sch_dist_county_merge <- data.frame("State"=sch_dist_county$State.Postal.Code,
                                      "County"=str_to_title(str_remove_all(sch_dist_county$County.Names, " County")),
                                      "County_estimation"=str_to_title(str_remove_all(sch_dist_county$County.Names, " County")),
                                      "County_full"=sch_dist_county$County.Names,
                                      "Overseeing_Agency"=sch_dist_county$School.District.Name, 
                                      "Overseeing_Agency_ISD_full"=sch_dist_county$School.District.Name
                                      )
  
  # public libs
  pub_lib_county_merge <- data.frame("State"=pub_lib_county$STABR,
                                   "County"=str_to_title(pub_lib_county$CNTY),
                                   "County_estimation"=str_to_title(pub_lib_county$CNTY),
                                   "City"=str_to_title(pub_lib_county$CITY),
                                   "Lib_name"=str_to_title(pub_lib_county$LIBNAME),
                                   "Overseeing_Agency"=str_to_title(pub_lib_county$LIBNAME)
                                   )
# my edited data...
data_tm_aw <- data_tm

#-------------------------------------------------------------------------------
# COUNTY INFO MERGE
# Merge in county name by overseeing agency name
# Overseeing Agency is either a the name of a school district or the name of public library
sum(is.na(data_tm_aw$County)) #5070

#------------------------------
# SCHOOL DISTRICT COUNTY MERGE
#------------------------------
# create ID: state + county + agency
sch_dist_county_merge$ID_state_county_agency <- paste(sch_dist_county_merge$State,
                                                      sch_dist_county_merge$County,
                                                      sch_dist_county_merge$Overseeing_Agency_ISD_full)
sch_dist_county_merge$ID_state_agency <- paste(sch_dist_county_merge$State,
                                               sch_dist_county_merge$Overseeing_Agency_ISD_full)
# Remove duplicated and note that this means picking one of the counties that
# the school district could belong to - thus ESTIMATION/GUESS 
sch_dist_county_merge_nodups <- sch_dist_county_merge[!duplicated(sch_dist_county_merge$ID_state_agency),]
# Create data frame for merge
school_district_est_county <- sch_dist_county_merge_nodups[c("State","County_estimation","Overseeing_Agency_ISD_full")]
# Data set with estimated counties:
data_tm_aw_estcounty <- join(data_tm_aw, school_district_est_county, by=c("State","Overseeing_Agency_ISD_full"))
# check
temp <- data_tm_aw_estcounty[is.na(data_tm_aw_estcounty$County),] #5070
sum(!is.na(temp$County_estimation)) #1935: number of county estimations filled in


#-----------------------------
# PUBLIC LIBRARY COUNTY MERGE
#-----------------------------
# Now for public library county info:
pub_lib_county_merge$ID_state_county_agency <- paste(pub_lib_county_merge$State,
                                                     pub_lib_county_merge$County,
                                                     pub_lib_county_merge$Overseeing_Agency)
pub_lib_county_merge$ID_state_agency <- paste(pub_lib_county_merge$State,
                                              pub_lib_county_merge$Overseeing_Agency)
# Remove dups, only 5
pub_lib_county_merge_nodups <- pub_lib_county_merge[!duplicated(pub_lib_county_merge$ID_state_agency),]
# Create data frame for merge
pub_lib_est_county <- pub_lib_county_merge_nodups[c("State","County_estimation","Overseeing_Agency")]
# Data set with estimated counties:
data_tm_aw_estcounty <- join(data_tm_aw_estcounty, pub_lib_est_county, by=c("State","Overseeing_Agency"))
colnames(data_tm_aw_estcounty)[ncol(data_tm_aw_estcounty)] <- 'County_estimation_lib'
# check
temp <- data_tm_aw_estcounty[is.na(data_tm_aw_estcounty$County),]
sum(!is.na(temp$County_estimation_lib)) # 199: number of county estimations filled in
#-------------------------------------------------------------------------------

# COUNTY MERGE VAR
# Make new county variable that combines original with estimates:
data_tm_aw_estcounty$County_merge = ifelse(is.na(data_tm_aw_estcounty$County), data_tm_aw_estcounty$County_estimation, data_tm_aw_estcounty$County)
data_tm_aw_estcounty$County_merge = ifelse(is.na(data_tm_aw_estcounty$County_merge), data_tm_aw_estcounty$County_estimation_lib, data_tm_aw_estcounty$County_merge)
sum(is.na(data_tm_aw_estcounty$County_merge)) #2936

# 2134 rows of estimated counties filled in.
#-------------------------------------------------------------------------------


## Prep a data frame for uploading to Flourish

data_tm_aw_estcounty <- join(data_tm_aw_estcounty, 
     decision_mapping[c("Decision","Decision_Simplified")], 
     by=c("Decision"))

flourish_counties <- read.csv("U.S. County Data/FlourishBaseMap_USCountyRegionData.csv")
flourish_counties$county_state <- paste(flourish_counties$NAME, flourish_counties$STUSPS)
flourish_counties[duplicated(flourish_counties$county_state)==TRUE,] # 6 to remove
flourish_counties <- flourish_counties[!duplicated(flourish_counties$county_state),]

data_tm_aw_estcounty$ID_county_state <- ifelse(!is.na(data_tm_aw_estcounty$County_merge),
                                               paste(data_tm_aw_estcounty$County_merge, data_tm_aw_estcounty$State),
                                               NA)
  
data_tm_aw_estcounty <- left_join(data_tm_aw_estcounty, flourish_counties, 
                             by = c("ID_county_state" = "county_state"))

write.csv(data_tm_aw_estcounty,"bookchallenge_county_estimations.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

## May need to group data set:
# sum of banned books per county, sum of overturned challenges per county, sum of unkown/unrelated changes

table(data_tm_aw_estcounty$Decision_Simplified, useNA = "ifany") # Public: 994; School: 11037; NA: 171

cnty_freq <- setDT(data_tm_aw_estcounty)
cnty_freq <- cnty_freq[, .N, by = .(State, STATE_NAME, STATEFP, County_merge, COUNTYFIPS,
                          NAMELSAD, ID_county_state, Year_2,
                          Decision_Simplified #, Overseeing_Agency_ISD_full
                          )]

#temp <- temp[order(c(State, County_merge, Decision_Simplified))]
cnty_freq <- cnty_freq[order(c(-N))]
#temp <- temp[order(c(Overseeing_Agency_ISD_full))]
colnames(cnty_freq)[ncol(cnty_freq)] <- 'Frequency'

cnty_freq_flourish <- left_join(flourish_counties, cnty_freq, 
                                by = c("COUNTYFIPS"))

write.csv(cnty_freq_flourish,"cnty_freq_flourish.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")
write.csv(cnty_freq,"cnty_freq.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")


#------------------------------
# COUNTY DATA BY LAT/LON POINT
# Data frames for Flourish
#------------------------------

latlon_cnty <- read.csv("U.S. County Data/us_county_latlng.csv")
#colnames(latlon_cnty)[c(1,4,10,11)] <- c("state","county","lat","lon")
#latlon_cnty$lat <- round(latlon_cnty$lat, digits=6)
#latlon_cnty$ID_county_state <- paste(latlon_cnty$county, latlon_cnty$state)
#latlon_cnty$COUNTYFIPS <- as.integer(latlon_cnty$V2)

cnty_freq_coord <- left_join(cnty_freq, latlon_cnty, by=c("COUNTYFIPS"="fips_code"))
cnty_freq_coord$label <- paste0(cnty_freq_coord$NAMELSAD, ", ", cnty_freq_coord$STATE_NAME)
cnty_freq_coord_dropNA <- cnty_freq_coord[!is.na(cnty_freq_coord$COUNTYFIPS)]

write.csv(cnty_freq_coord_dropNA,"cnty_freq_coord_dropNA.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

## County with Frequency of Ban, Overturned, Unknown/Unresolved/Unrelated Removal
# Flourish requires year/time in column - will need to pivot to wide.

flourish_cnty_outcome_year <- as.data.frame.matrix(cnty_freq_coord_dropNA)
flourish_cnty_outcome_year <- 
  flourish_cnty_outcome_year[c("label","NAMELSAD","State","Year_2","lat","lng",
     "Decision_Simplified","Frequency")]

flourish_cnty_outcome_year <-
flourish_cnty_outcome_year %>%
  pivot_wider(names_from = Year_2, values_from = Frequency)

flourish_cnty_outcome_year <- 
  flourish_cnty_outcome_year[c("label","NAMELSAD","State","Decision_Simplified",
                              "lat","lng","2021","2022","2023")]

write.csv(flourish_cnty_outcome_year,"flourish_cnty_outcome_year.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")


#--------------------------------
# STATE DATA, OUTCOME CATEGORIES
# Data frame for Flourish
#--------------------------------
state_outcomes <- data_tm_aw_estcounty[!is.na(data_tm_aw_estcounty$State),]
state_outcomes_noyear <- state_outcomes[, .N, by = .(State,
                                    Decision_Simplified)]
state_outcomes_wyear <- state_outcomes[, .N, by = .(State, Year_2, # with out without year...
                                              Decision_Simplified)]

state_outcomes_noyear <- state_outcomes_noyear[order(c(-N))]
state_outcomes_wyear <- state_outcomes_wyear[order(c(-N))]

state_outcomes_noyear <-
  state_outcomes_noyear %>%
  pivot_wider(names_from = Decision_Simplified, values_from = N)
state_outcomes_wyear <-
  state_outcomes_wyear %>%
  pivot_wider(names_from = Decision_Simplified, values_from = N)

# drop NA column, reorder
state_outcomes_noyear <- within(state_outcomes_noyear, rm("NA"))

state_outcomes_wyear <- state_outcomes_wyear %>% drop_na(Year_2)

# Consolidate extra columns:
state_outcomes_wyear$Other <- rowSums(state_outcomes_wyear[, c("NA","Unknown","Unrelated Removal")],na.rm=TRUE)
# Remove unwanted:
state_outcomes_wyear <- within(state_outcomes_wyear, rm("NA","Unknown","Unrelated Removal"))
state_outcomes_wyear$Total <- rowSums(state_outcomes_wyear[, c(3:6)],na.rm=TRUE)

# state names:
state_name_map <- read.csv("U.S. County Data/FlourishBase_USstates.csv")

# merge state names:
state_outcomes_noyear <- left_join(state_outcomes_noyear, state_name_map, by=c("State" = "STUSPS"))
state_outcomes_wyear <- left_join(state_outcomes_wyear, state_name_map, by=c("State" = "STUSPS"))


#state_outcomes <- state_outcomes[ , -which(names(state_outcomes) %in% c("STATE_NAME","STATEFP"))]
write.csv(state_outcomes_noyear,"state_outcomes_allyears_totals.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

write.csv(state_outcomes_wyear,"state_outcomes_peryear.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")


# for state sums:
#state_outcome_sums_by_year <- state_outcomes[c("State","Year_2","Banned","Overturned","Unresolved","FIPS","NAME")]
#state_outcome_sums_by_year <- state_outcome_sums_by_year %>% drop_na(Year_2)

# state totals for all years by category:
state_outcomes_wyear2 <- state_outcomes_wyear
year_totals_by_outcome <- setDT(state_outcomes_wyear2)
year_totals_by_outcome <- year_totals_by_outcome[ ,list( Banned=sum(Banned, na.rm=TRUE),
                               Overturned=sum(Overturned, na.rm=TRUE),
                               Unresolved=sum(Unresolved, na.rm=TRUE),
                               Other=sum(Other, na.rm=TRUE),
                               Total=sum(Total, na.rm=TRUE)
                              ), by=Year_2]

# test
temp <- state_outcomes_wyear[state_outcomes_wyear$Year_2==2023,]
sum(temp$Unresolved, na.rm = TRUE)
sum(temp$Banned, na.rm = TRUE)

write.csv(year_totals_by_outcome,"year_totals_by_outcome.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

year_totals_by_outcome_vert <- 
year_totals_by_outcome %>%
  pivot_longer(!c(Total, Year_2), names_to = "Category", values_to = "Sum")

write.csv(year_totals_by_outcome_vert,"year_totals_by_outcome_vert.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")


# vertical format:
state_outcomes_vert <- data_tm_aw_estcounty[!is.na(data_tm_aw_estcounty$State),]
state_outcomes_vert <- state_outcomes_vert[, .N, by = .(State,
                                                        Decision_Simplified)]
state_outcomes_vert <- state_outcomes_vert[!is.na(Decision_Simplified)]
state_outcomes_vert <- left_join(state_outcomes_vert, state_name_map, by=c("State" = "STUSPS"))
write.csv(state_outcomes_vert,"state_outcomes_vert.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

# custom format vertical for flourish (not cool, flourish)
# https://www.youtube.com/watch?v=vrc5tQgn20I
s_bans <- state_outcomes[c("State","FIPS","NAME","Banned")]
s_bans <- drop_na(s_bans)
s_overt <- state_outcomes[c("State","FIPS","NAME","Overturned")]
s_overt <- drop_na(s_overt)
s_unres <- state_outcomes[c("State","FIPS","NAME","Unresolved")]
s_unres <- drop_na(s_unres)
s_unrem <- state_outcomes[c("State","FIPS","NAME","Unrelated Removal")]
s_unrem <- drop_na(s_unrem)
s_unkno <- state_outcomes[c("State","FIPS","NAME","Unknown")]
s_unkno <- drop_na(s_unkno)


state_outcomes_stacked <- bind_rows(s_bans,s_overt,s_unres,s_unrem,s_unkno)
#write.csv(state_outcomes_stacked,"state_outcomes_stacked.csv", 
#          row.names=FALSE, fileEncoding="UTF-8", na="")


#--------------------------------
# TOTAL OVER TIME (month, year)
#--------------------------------

## Outcome Type by STATE AND MONTH+YEAR (racing bar chart)

## mapping to merge:
month_number_map <- data.frame("month_num"=c("01","02","03","04","05","06","07","08","09","10","11","12"),
                               "month"=c("January","February","March","April",
                                         "May","June","July","August","September",
                                         "October","November","December"))
month_number_map2 <- data.frame("month_num"=c("1","2","3","4","5","6","7","8","9","10","11","12"),
                                "month"=c("January","February","March","April",
                                          "May","June","July","August","September",
                                          "October","November","December"))

## building the df
challenge_freq_year <- setDT(data_tm_aw_estcounty)

challenge_freq_year <- left_join(challenge_freq_year, month_number_map, by=c("Month"="month_num"))
challenge_freq_year$Month[!is.na(challenge_freq_year$month)] <- challenge_freq_year$month[!is.na(challenge_freq_year$month)]
challenge_freq_year <- within(challenge_freq_year, rm("month"))

challenge_freq_year <- left_join(challenge_freq_year, month_number_map2, by=c("Month"="month_num"))
challenge_freq_year$Month[!is.na(challenge_freq_year$month)] <- challenge_freq_year$month[!is.na(challenge_freq_year$month)]
challenge_freq_year <- within(challenge_freq_year, rm("month"))

challenge_freq_year$month_year <- paste0(challenge_freq_year$Month, ", ", challenge_freq_year$Year_2)
challenge_freq_year <- challenge_freq_year[!is.na(challenge_freq_year$Year_2)]
challenge_freq_year <- challenge_freq_year[!is.na(challenge_freq_year$Month)]

challenge_freq_year <- challenge_freq_year[, .N, by = .(month_year, Decision_Simplified)]
#challenge_freq_year$month_year_date <- as.Date(challenge_freq_year$month_year, "%m, %Y")

challenge_freq_year <- drop_na(challenge_freq_year)
#challenge_freq_year <- challenge_freq_year[, .N, by = .(month_year, Decision_Simplified)]

challenge_freq_year <- challenge_freq_year %>%
  pivot_wider(names_from = month_year, values_from = N)
write.csv(challenge_freq_year,"challenge_freq_moyear.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

# Just outcome and year:
challenge_freq_year <- setDT(data_tm_aw_estcounty)
challenge_freq_year <- challenge_freq_year[, .N, by = .(Year_2, Decision_Simplified)]
challenge_freq_year <- drop_na(challenge_freq_year)
challenge_freq_year <- challenge_freq_year %>%
  pivot_wider(names_from = Year_2, values_from = N)
write.csv(challenge_freq_year,"challenge_freq_year.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")





