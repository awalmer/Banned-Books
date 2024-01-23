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
decision_mapping[unkown_i,"Decision_Simplified"] <- c(rep("Unkown", length(unkown_i)))

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
