# CatCloud data with engagement data and funnels
# student catcloud with new features
# last run 09-03-2024

#### load libraries ####
library("tidyverse")
library(readr)
library(data.table)
library(dplyr)
library(tibble)
library(tidyr)
library(stringi)
library(stringr)
library(ggplot2)
library(readxl)
library(lubridate)
library(readr)
library(boxr)
library(RCurl)
library(forcats)
#### create a Write csv function#### 
write_named_csv <- function(x) 
  write_csv(x, file = paste0(
    "~/Documents/Trellis/CatCloud_dashboard/clean_data/clean_fall2024/Student/",
    deparse(substitute(x)),".csv"))

#### read in excel file ####
# doing an exploration with last 14 days from 8/23 to 9/5
CC_all1 <- read_csv("initial_data/Fall2024/CC_all/CC_all_Aug23-Sept5-2024.csv", skip=6)

CC_all1 <- CC_all1 %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(14)) # remove a total columns

CC_all1 <- CC_all1 %>%
  select(-c(9)) # total number of users are not needed as they are always 1 

# elapsed time from last page request is in milliseconds
# rename and convert to seconds
# CC_all1 <- CC_all1 %>%
#   mutate(Time_elapsed_in_seconds = `Elapsed time from last page request` / 1000)  # Convert to seconds
# 
# # average session duration is in seconds
# CC_all1 <- CC_all1 %>% 
#   mutate(Avg_session_in_seconds = `Average session duration`) %>% 
#   select(-`Average session duration`)
# 
# # user engagement is also in seconds
# CC_all1 <- CC_all1 %>% 
#   mutate(User_engagement_in_seconds = `User engagement`) %>% 
#   select(-`User engagement`)

# Condensed code chunk 
CC_all1 <- CC_all1 %>%
  mutate(
    Time_elapsed_in_seconds = `Elapsed time from last page request` / 1000,
    Avg_session_in_seconds = `Average session duration`,
    User_engagement_in_seconds = `User engagement`
  ) %>%
  select(-`Average session duration`, -`User engagement`)

#### cases users ####
CC_cases1 <- read_csv("initial_data/Fall2024/cases/cases_Aug23-Sept5-2024.csv", skip = 7)
CC_cases1 <- CC_cases1 %>%
  filter(!is.na(Date)) %>%
  # filter(Totals != 'Event count') %>%
  select(`Effective user ID`, `Event count`, Sessions)

CC_cases <- rename(CC_cases1, Cases.Sessions = Sessions)

CC_cases$Cases.Sessions <- as.numeric(CC_cases$Cases.Sessions)

#### appointment users ####
CC_appt1 <- read_csv("initial_data/Fall2024/appt/appt-Aug23-Sept5-2024.csv", skip = 6)
CC_appt  <- CC_appt1 %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(-c(7:8)) 

# rename variables 
CC_appt <- rename(CC_appt, Appt.Sessions = Sessions) #, Namespace.ID = `Namespace ID`, Stream.name = `Stream name`, App.instance.ID = `App-instance ID`

CC_appt <- CC_appt %>%
  select(`Effective user ID`, Appt.Sessions) %>% 
  distinct() 

CC_appt$`Effective user ID` <- as.character(CC_appt$`Effective user ID`)

#### add edit users ####
CC_edit <- read_csv("initial_data/Fall2024/add_edit/add_edit_Jan-Sept2-2024.csv", skip = 6)

CC_edit <- CC_edit %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(-c(7:8))  

CC_edit <- rename(CC_edit, Edit.Sessions = Sessions)
CC_edit <- CC_edit %>% 
  select(`Effective user ID`, Edit.Sessions) %>% 
  distinct()

#### goals users ####
CC_goals1 <- read_csv("initial_data/Fall2024/goals/goals_Aug23-Sept5-2024.csv", skip = 6)

CC_goals <- CC_goals1 %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(7:8))

CC_goals <- rename(CC_goals, Goal.Sessions = Sessions)
CC_goals <- CC_goals %>%
  select(`Effective user ID`, Goal.Sessions) %>% 
  distinct()

#### events users ####
CC_events1 <- read_csv("initial_data/Fall2024/events/events-Aug23-Sept5-2024.csv", skip = 6)

CC_events <- CC_events1 %>%
  filter(!is.na(`Stream name`)) %>%
  select(`Effective user ID`, `Total users`) %>% 
  rename(Events.Sessions = `Total users`)

#### funnel explorations ####
# Get the list of files in the folder
f <- list.files("./initial_data/Fall2024/funnel_exploration/", full.names = TRUE)

# Remove the directory path and file extension to use the file names as dataframe names
file_names <- tools::file_path_sans_ext(basename(f))

# Read each CSV and assign to a dataframe using the original file name
for (i in seq_along(f)) {
  assign(file_names[i], read_csv(f[i], skip = 7))
}

# home start 
funnel_start_home <- funnel_start_home %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(6))

# student services start
student_services_start <- `student-services-start` %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(6))

# home to student services 
`student_serv_from_home` <- `student-serv-from-home` %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(6))

# home to classes 
`classes_from_home` <- `classes-from-home` %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(6))

# home to advising
appt_from_home <- `appt-from-home` %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(6))

# student services to classes 
`classes_from_student_services` <- `classes-from-student-serv` %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(6))




#### read in SF csv ####
all_users_SF <- read_csv("initial_data/Fall2024/from_SF/Sept6_2024_community_users_profile.csv")

# change last login to a readable date format
all_users_SF$last_login2 <- as.Date(mdy_hm(all_users_SF$`Last Login`))
# 
# all_users_SF <- all_users_SF %>% 
#   filter(last_login2 > "2023-08-08") 
#### creating both types of SF ID ####
#convert  SF 15 into 18
binary <- c("00000","00001","00010","00011","00100","00101","00110","00111",
            "01000","01001","01010","01011","01100","01101","01110","01111",
            "10000","10001","10010","10011","10100","10101","10110","10111",
            "11000","11001","11010","11011","11100","11101","11110","11111")
binary.reverse <- lapply(binary, function(x){paste0(rev(strsplit(x, split = "")[[1]]), collapse = "")})
binary2letter <- c(LETTERS, 0:5)
names(binary2letter) <- unlist(binary.reverse)
rm(binary, binary.reverse)

SFID_Convert <- function(sfid) {
  sfid <- as.character(sfid) # in case the input column are factors
  
  str_num <- gsub("[A-Z]", "1", gsub("[a-z0-9]", "0", sfid))
  
  s1 <- substring(str_num, 1, 5)
  s2 <- substring(str_num, 6, 10)
  s3 <- substring(str_num, 11, 15)
  
  sfid.addon <- paste0(sfid,
                       binary2letter[s1], 
                       binary2letter[s2], 
                       binary2letter[s3])
  
  sfid[nchar(sfid)==15] <- sfid.addon[nchar(sfid)==15]
  
  return(sfid)
}


# SFID_Convert(sfid)
all_users_SF <- all_users_SF %>% 
  mutate(`SF_18_ID` = SFID_Convert(`User ID`)) %>% 
  rename(SF_15_ID = 'User ID')

#### merge all current students based on emails to CatSF ####
student_enrollment_from_sf <- read_csv("initial_data/Fall2024/from_SF/student_enrollment_as_of_Sept6_2024.csv")

# delete if email is blank
student_enrollment_from_sf <- student_enrollment_from_sf[!(is.na(student_enrollment_from_sf$Email) | student_enrollment_from_sf$Email==""), ]

# # merge student enrollment into all_users_SF
# all_users_SF <- all_users_SF %>% 
#   left_join(student_enrollment_from_sf)
#### subset sf_users by profile
unique(all_users_SF$Profile)

CC_all_test <- CC_all1 %>%
  mutate(
    SF_18_ID = ifelse(nchar(`Effective user ID`) == 18, `Effective user ID`, NA),
    SF_15_ID= ifelse(nchar(`Effective user ID`) == 15, `Effective user ID`, NA)
  )

CC_all_15_id <- CC_all_test %>%
  select(SF_15_ID, Date, `Event count`, Sessions, Views,
         User_engagement_in_seconds, Time_elapsed_in_seconds, Avg_session_in_seconds
         # `Elapsed time from last page request`, `Average session duration`
  ) %>% 
  filter(!is.na(SF_15_ID)) 

CC_all_18_id <- CC_all_test %>%
  select(SF_18_ID, Date, `Event count`, Sessions, Views,
         User_engagement_in_seconds, Time_elapsed_in_seconds, Avg_session_in_seconds) %>% 
  filter(!is.na(SF_18_ID)) 

CC_all_15_id <- left_join(CC_all_15_id, all_users_SF) 
CC_all_18_id <- left_join(CC_all_18_id, all_users_SF) 
CC_all_new <- rbind(CC_all_15_id, CC_all_18_id)

CC_appt <- CC_appt %>% rename(SF_18_ID = `Effective user ID`)
CC_case <- CC_cases %>% rename(SF_18_ID = `Effective user ID`)
CC_goal <- CC_goals %>% rename(SF_15_ID = `Effective user ID`)
CC_edit <- CC_edit %>% rename(SF_15_ID = `Effective user ID`)
CC_event <- CC_events %>% rename(SF_18_ID = `Effective user ID`)
# CC_news <- CC_news %>% rename(SF_18_ID = `Effective user ID`)

# join CC to all SF users to get id information
CC_all_new <- left_join(CC_all_new, all_users_SF)
CC_appt <- left_join(CC_appt, all_users_SF)
CC_case <- left_join(CC_case, all_users_SF)
CC_goal <- left_join(CC_goal, all_users_SF)
CC_edit <- left_join(CC_edit, all_users_SF)
# CC_news <- left_join(CC_news, all_users_SF_PO)
CC_event <- left_join(CC_event, all_users_SF)


#### merge the 5 data frame with SF data ####
Cat_SF <- CC_all_new %>% left_join(CC_appt) %>% 
  left_join(CC_case) %>% left_join(CC_goal) %>% 
  left_join(CC_edit) %>% left_join(CC_event) 


Cat_SF <- Cat_SF %>% 
  select(SF_15_ID, SF_18_ID, Sessions, Date, 
         Appt.Sessions, Edit.Sessions,Goal.Sessions,
         Cases.Sessions, Events.Sessions,     
         User_engagement_in_seconds, Time_elapsed_in_seconds, Avg_session_in_seconds,
         Email, `First Name`, `Last Name`, `Last Login`, Profile, 
         `EDS Primary Affiliation`, `EDS Affiliations`) %>% 
  distinct()
#### Change Last Login to date format ####
Cat_SF$last_login2 <- as.Date(as.character(Cat_SF$Date), format='%Y%m%d')

# create a counter if there is a appt session
Cat_SF <- Cat_SF %>%
  dplyr::mutate(Appt = ifelse(Appt.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Case = ifelse(Cases.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Event = ifelse(Events.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Goal = ifelse(Goal.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Edit = ifelse(Edit.Sessions > 0, 1, 0)) 

#### create a semester column for student login ####
# Function to assign semester based on the given date ranges
get_semester <- function(date_range) {
  # Define the date ranges for Spring and Fall 2024
  spring_start <- as.Date("2024-01-01")
  spring_end <- as.Date("2024-05-12")
  summer_start <- as.Date("2024-05-13")
  summer_end <- as.Date("2024-08-20")
  fall_start <- as.Date("2024-08-21")
  fall_end <- as.Date("2024-12-31")
  
  # Assign semester based on the date ranges
  if (date_range >= spring_start & date_range <= spring_end) {
    return("Spring 2024")
  } else if (date_range >= summer_start & date_range <= summer_end) {
      return("Summer 2024")
  } else if (date_range >= fall_start & date_range <= fall_end) {
    return("Fall 2024")
  } else {
    return(NA)  # If the date is outside the specified ranges
  }
}

# Apply the function to create the semester column
Cat_SF$semester <- sapply(Cat_SF$last_login2, get_semester)

#### join catcloud to student enrollment data in SF ####
Cat_SF_enroll <- left_join(Cat_SF, student_enrollment_from_sf) %>% 
  distinct()

# recode class standing
Cat_SF_enroll$Class_Standing_recode <- recode(Cat_SF_enroll$`Class Standing`,"Freshman" = "Freshman", 
                                              "Sophomore" = "Sophomore", 
                                              "Junior" = "Junior", "Senior" = "Senior", 
                                              "Graduate" = "Graduate", "Masters" = "Graduate",
                                              "Prof 1" = "Graduate", "Prof 2" = "Graduate",
                                              "Prof 3" = "Graduate", "Prof 4" = "Graduate", 
                                              "Doctoral" = "Graduate", "NA" = "Other", " " = "Other")
unique(Cat_SF_enroll$Class_Standing_recode)

Cat_SF_enroll$Class_Standing_recode <- as.character(Cat_SF_enroll$Class_Standing_recode)
Cat_SF_enroll$Class_Standing_recode[is.na(Cat_SF_enroll$Class_Standing_recode)] <- "Other"
unique(Cat_SF_enroll$Class_Standing_recode)

Cat_SF_enroll$Class_Standing_recode <- factor(Cat_SF_enroll$Class_Standing_recode, 
                                              levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate", "Other"))

# Headcounts data from UAccess
#### Student headcounts from UAccess #### 
Headcount_Fall2024 <- read_csv("initial_data/Fall2024/Headcount_Fall_Winter_2024.csv")

#### add semester columns for each headcount spreadsheet ####
Headcount_Fall2024$semester <- "Fall 2024"

#### refactor headcount academic level data ####
unique(Headcount_Fall2024$`Academic Level`)

Headcount_Fall2024$Class_standing_recode <- recode(Headcount_Fall2024$`Academic Level`,
                                                     "Freshman" = "Freshman", "Sophomore" = "Sophomore", 
                                                     "Junior" = "Junior", "Senior" = "Senior", 
                                                     "Graduate" = "Graduate", "Masters" = "Graduate",
                                                     "Professional Year 1" = "Graduate", "Professional Year 2" = "Graduate",
                                                     "Professional Year 3" = "Graduate", "Professional Year 4" = "Graduate", 
                                                     "Doctoral" = "Graduate", " " = "Other")

Headcount_Fall2024$Class_standing_recode <- factor(Headcount_Fall2024$Class_standing_recode, 
                                                     levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate", NA))

unique(Headcount_Fall2024$Class_standing_recode)

# rename academic programs
Headcount_Fall2024 <- Headcount_Fall2024 %>%
  mutate(Primary_College_recode = factor(`Academic Program`) %>%
           fct_recode(
             "Rogers College of Law" = "James E. Rogers College of Law",
             "Rogers College of Law" = "Law Doctoral",
             "Rogers College of Law" = "Law Masters",
             "Rogers College of Law" = "Law Non-Degree Seeking",
             "College of Veterinary Medicine" = "Vet. Medicine",
             # "CALES" = "Ag, Life & Envi Sci., Col of",
             # "Zuckerman Public Health" = "Zuckerman Coll Public Health",
             "Graduate Coursework" = "Graduate Certificate",
             "Graduate Coursework" = "Graduate Degree Seeking",
             "Graduate Coursework" = "Graduate Non-Degree Seeking",
             "Undergraduate Coursework" = "Undergrad Non-Degree Seeking",
             "Undergraduate Coursework" = "Undergraduate Certificate"))

#### recode Primary.College ####
Cat_SF_enroll$`Primary College`[is.na(Cat_SF_enroll$`Primary College`)] <- "Other"

unique(Cat_SF_enroll$`Primary College`)

Cat_SF_enroll <- Cat_SF_enroll %>%
  mutate(Primary.College_recode = factor(`Primary College`) %>%
           fct_recode(
             "Rogers College of Law" = "James E. Rogers College of Law",
             "Rogers College of Law" = "Law Doctoral",
             "Rogers College of Law" = "Law Masters",
             "Rogers College of Law" = "Law Non-Degree Seeking",
             "Graduate Coursework" = "Graduate Certificate",
             "Graduate Coursework" = "Graduate Non-Degree Seeking",
             "Graduate Coursework" = "Graduate Degree Seeking",
             "Undergraduate Coursework" = "Undergrad Non-Degree Seeking",
             "Undergraduate Coursework" = "Undergraduate Certificate"))

unique(Cat_SF_enroll$Primary.College_recode)
summary(Cat_SF_enroll$Primary.College_recode)

