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
#### merge all CC users ####
CC_all1 <- read_csv("initial_data/Fall2024/CC_all/CC_all_Jan-Feb29-2024.csv", skip=6)
CC_all2a <- read_csv("initial_data/Fall2024/CC_all/CC_all_Mar-2024.csv", skip=6)
CC_all2b <- read_csv("initial_data/Fall2024/CC_all/CC_all_Apr-2024.csv", skip=6)
CC_all3 <- read_csv("initial_data/Fall2024/CC_all/CC_all_May-2024.csv", skip=6)
CC_all4 <- read_csv("initial_data/Fall2024/CC_all/CC_all_Jun-Jul-2024.csv", skip=6)
CC_all5 <- read_csv("initial_data/Fall2024/CC_all/CC_all_Aug25-2024.csv", skip=6)
CC_all6a <- read_csv("initial_data/Fall2024/CC_all/CC_all_Aug26-Aug30-2024.csv", skip=6) # new format
CC_all6b <- read_csv("initial_data/Fall2024/CC_all/CC_all_Aug31-Sept1-2024.csv", skip=6) # new format
CC_all6c <- read_csv("initial_data/Fall2024/CC_all/CC_all_Sept2-2024.csv", skip=6) # new format

CC_all1 <- CC_all1 %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(9:10)) 

CC_all2a <- CC_all2a %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(9:10))

CC_all2b <- CC_all2b %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(9:10))

CC_all3 <- CC_all3 %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(9:10)) 

CC_all4 <- CC_all4 %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(9:10)) 

CC_all5 <- CC_all5 %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(9:10)) 

CC_all6a <- CC_all6a  %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(Segment, `Effective user ID`, Date, `Stream name`, `Namespace ID`, `Event count`, Sessions, Views) 

CC_all6b <- CC_all6b  %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(Segment, `Effective user ID`, Date, `Stream name`, `Namespace ID`, `Event count`, Sessions, Views) 

CC_all6c <- CC_all6c  %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(Segment, `Effective user ID`, Date, `Stream name`, `Namespace ID`, `Event count`, Sessions, Views) 


CC_all <- rbind(CC_all1, CC_all2a, CC_all2b, CC_all3, CC_all4, CC_all5, CC_all6a, CC_all6b, CC_all6c)


#### cases users ####
CC_cases1 <- read_csv("initial_data/Fall2024/cases/cases_Jan_Sept2_2024.csv", skip = 7)

CC_cases1 <- CC_cases1 %>%
  filter(!is.na(Date)) %>%
  # filter(Totals != 'Event count') %>%
  select(`Effective user ID`, `Event count`, Sessions)

CC_cases <- rename(CC_cases1, Cases.Sessions = Sessions)

CC_cases$Cases.Sessions <- as.numeric(CC_cases$Cases.Sessions)

#### appointment users ####
CC_appt1 <- read_csv("initial_data/Fall2024/appt/appt-Jan-Mar15-2024.csv", skip = 6)
CC_appt2 <- read_csv("initial_data/Fall2024/appt/appt-users-newID-Mar16-May12-2024.csv", skip = 6)
CC_appt3a <- read_csv("initial_data/Fall2024/appt/appt-May13-Aug25-2024.csv", skip = 6)
CC_appt3b <- read_csv("initial_data/Fall2024/appt/appt-Aug26-Sept1-2024.csv", skip = 6)
CC_appt3c <- read_csv("initial_data/Fall2024/appt/appt-Sept2-2024.csv", skip = 6)

CC_appt1  <- CC_appt1 %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(-c(7:8))   
CC_appt2  <- CC_appt2 %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(-c(7:8))   
CC_appt3a  <- CC_appt3a %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(-c(7:8))   
CC_appt3b  <- CC_appt3b %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(-c(7:8))   
CC_appt3c  <- CC_appt3c %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(-c(7:8))
         
CC_appt <- rbind(CC_appt1, CC_appt2, CC_appt3a, CC_appt3b, CC_appt3c)

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
CC_goals1 <- read_csv("initial_data/Fall2024/goals/goals-Jan-Sept2-2024.csv", skip = 6)

CC_goals <- CC_goals1 %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(7:8))

CC_goals <- rename(CC_goals, Goal.Sessions = Sessions)
CC_goals <- CC_goals %>%
  select(`Effective user ID`, Goal.Sessions) %>% 
  distinct()

#### events users ####
CC_events1 <- read_csv("initial_data/Fall2024/events/events-Jan-Sept2_2024.csv", skip = 6)

CC_events <- CC_events1 %>%
  filter(!is.na(`Stream name`)) %>%
  select(`Effective user ID`, `Total users`) %>% 
  rename(Events.Sessions = `Total users`)

#### read in SF csv ####
all_users_SF <- read_csv("initial_data/Fall2024/from_SF/Sept2_2024_community_users_profile.csv")

# change last login to a readable date format
all_users_SF$last_login2 <- as.Date(mdy_hm(all_users_SF$`Last Login`))

all_users_SF <- all_users_SF %>% 
  filter(last_login2 > "2023-08-08") 

# SF users with Parent Org
SF_parent_org <- read_csv("initial_data/Fall2024/from_SF/SF_Parent_organization_Sept2_2024.csv")

# Perform full join
all_users_SF_PO <- all_users_SF %>%
  left_join(SF_parent_org)


all_users_SF_PO <- all_users_SF_PO %>%
  mutate(NetID_1 = coalesce(NetID...11, NetID...12)) %>% 
  select(-NetID...11, -NetID...12)
names(all_users_SF_PO)

# coalesce old and new information
all_users_SF_PO <- all_users_SF_PO %>%
  mutate(NetID = coalesce(NetID, NetID_1))

all_users_SF_PO  <- all_users_SF_PO  %>% 
  select(-NetID_1) %>% 
  distinct()

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
all_users_SF_PO <- all_users_SF_PO %>% 
  mutate(`SF_18_ID` = SFID_Convert(`User ID`)) %>% 
  rename(SF_15_ID = 'User ID')

#### subset sf_users by profile
unique(all_users_SF_PO$Profile)

CC_all_test <- CC_all %>%
  mutate(
    SF_18_ID = ifelse(nchar(`Effective user ID`) == 18, `Effective user ID`, NA),
    SF_15_ID= ifelse(nchar(`Effective user ID`) == 15, `Effective user ID`, NA)
  )

CC_all_15_id <- CC_all_test %>%
  select(SF_15_ID, Date, `Event count`, Sessions, Views
         # `User engagement`, `Engagement rate`,
         # `Elapsed time from last page request`, `Average session duration`
  ) %>% 
  filter(!is.na(SF_15_ID)) 

CC_all_18_id <- CC_all_test %>%
  select(SF_18_ID, Date, `Event count`, Sessions, Views) %>% 
  # `User engagement`, `Engagement rate`,
  # `Elapsed time from last page request`, `Average session duration`) %>% 
  filter(!is.na(SF_18_ID)) 

CC_all_15_id <- left_join(CC_all_15_id, all_users_SF_PO) 
CC_all_18_id <- left_join(CC_all_18_id, all_users_SF_PO) 
CC_all_new <- rbind(CC_all_15_id, CC_all_18_id)

CC_appt <- CC_appt %>% rename(SF_18_ID = `Effective user ID`)
CC_case <- CC_cases %>% rename(SF_18_ID = `Effective user ID`)
CC_goal <- CC_goals %>% rename(SF_15_ID = `Effective user ID`)
CC_edit <- CC_edit %>% rename(SF_15_ID = `Effective user ID`)
CC_event <- CC_events %>% rename(SF_18_ID = `Effective user ID`)
# CC_news <- CC_news %>% rename(SF_18_ID = `Effective user ID`)

# join CC to all SF users to get id information
CC_all_new <- left_join(CC_all_new, all_users_SF_PO)
CC_appt <- left_join(CC_appt, all_users_SF_PO)
CC_case <- left_join(CC_case, all_users_SF_PO)
CC_goal <- left_join(CC_goal, all_users_SF_PO)
CC_edit <- left_join(CC_edit, all_users_SF_PO)
# CC_news <- left_join(CC_news, all_users_SF_PO)
CC_event <- left_join(CC_event, all_users_SF_PO)


#### merge the 5 data frame with SF data ####
Cat_SF <- CC_all_new %>% left_join(CC_appt) %>% 
  left_join(CC_case) %>% left_join(CC_goal) %>% 
  left_join(CC_edit) %>% left_join(CC_event) 


Cat_SF <- Cat_SF %>% 
  select(SF_15_ID, SF_18_ID, Sessions, Date, 
         Appt.Sessions, Edit.Sessions,Goal.Sessions,
         Cases.Sessions, Events.Sessions,
         `Parent Organization`, `Organization: Account Name`,
         Email, `First Name`, `Last Name`, `Last Login`, Profile, 
         `EDS Primary Affiliation`, `EDS Affiliations`) %>% 
  distinct()
#### Change Last Login to date format ####
Cat_SF$last_login2 <- as.Date(as.character(Cat_SF$Date), format='%Y%m%d')

# create a counter if there is a appt session
Cat_SF_enroll <- Cat_SF %>%
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
  spring_end <- as.Date("2024-08-20")
  fall_start <- as.Date("2024-08-21")
  fall_end <- as.Date("2024-12-31")
  
  # Assign semester based on the date ranges
  if (date_range >= spring_start & date_range <= spring_end) {
    return("Spring 2024")
  } else if (date_range >= fall_start & date_range <= fall_end) {
    return("Fall 2024")
  } else {
    return(NA)  # If the date is outside the specified ranges
  }
}

# Apply the function to create the semester column
Cat_SF_enroll$semester <- sapply(Cat_SF_enroll$last_login2, get_semester)

#### merge all current students based on emails to CatSF ####
student_enrollment_from_sf <- read_csv("initial_data/Fall2024/from_SF/student_enrollment_as_of_Sept2_2024.csv")

# delete if email is blank
student_enrollment_from_sf <- student_enrollment_from_sf[!(is.na(student_enrollment_from_sf$Email) | student_enrollment_from_sf$Email==""), ]

#### join catcloud to student enrollment data in SF ####
Cat_SF_enroll <- left_join(Cat_SF_enroll, student_enrollment_from_sf) %>% 
  distinct()


#### add career as factor ####
Cat_SF_enroll$Career <- factor(Cat_SF_enroll$Career, 
                               levels=c("Undergraduate", "Graduate", "Law", "Medicine", 
                                        "Pharmacy", "Veterinary Medicine", "NA"))

unique(Cat_SF_enroll$`Class Standing`)


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

#### Student headcounts from UAccess #### 
# Headcount_Fall2023 <- read_csv("initial_data/headcounts_data/Headcount_fall_winter2023.csv")
Headcount_Spring2024 <- read_csv("initial_data/Spring2024/Headcount_Spring_Summer_2024.csv")
Headcount_Fall2024 <- read_csv("initial_data/Fall2024/Headcount_Fall_Winter_2024.csv")

#### add semester columns for each headcount spreadsheet ####
# Headcount_Fall2023$semester <- "Fall 2023"
Headcount_Fall2024$semester <- "Fall 2024"
Headcount_Spring2024$semester <- "Spring 2024"

# merge data for headcounts 
Headcount_students <- rbind(Headcount_Fall2023, Headcount_Spring2024, Headcount_Fall2024)

#### refactor headcount academic level data ####
unique(`Headcount_students`$`Academic Level`)

`Headcount_students`$Class_standing_recode <- recode(`Headcount_students`$`Academic Level`,
                                                     "Freshman" = "Freshman", "Sophomore" = "Sophomore", 
                                                     "Junior" = "Junior", "Senior" = "Senior", 
                                                     "Graduate" = "Graduate", "Masters" = "Graduate",
                                                     "Professional Year 1" = "Graduate", "Professional Year 2" = "Graduate",
                                                     "Professional Year 3" = "Graduate", "Professional Year 4" = "Graduate", 
                                                     "Doctoral" = "Graduate", " " = "Other")

`Headcount_students`$Class_standing_recode <- factor(`Headcount_students`$Class_standing_recode, 
                                                     levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate", NA))

unique(`Headcount_students`$Class_standing_recode)

table(Headcount_students$Class_standing_recode)


#### rename academic programs ####
unique(Headcount_students$`Academic Program`)

Headcount_students <- Headcount_students %>%
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

#### Academic Program Table ####
Academic_program_table <- Headcount_students %>% 
  count(Primary_College_recode, name = "All_students_program") %>% 
  left_join(Cat_SF_enroll %>%   
              # filter(last_login2 > "2023-08-08 16:25:00 GMT") %>%
              select(SF_18_ID, Primary.College_recode) %>% 
              distinct() %>% 
              rename(Primary_College_recode = Primary.College_recode)    %>% 
              count(Primary_College_recode, name = "N_users_program")) %>% 
  mutate(proportion = N_users_program/All_students_program)

Academic_program_table_fall2024 <- Headcount_students %>%
  filter(semester == "Fall 2024") %>% # Filter for Fall 2024
  count(Primary_College_recode, name = "All_students_program") %>%
  left_join(
    Cat_SF_enroll %>%
      filter(semester == "Fall 2024") %>% # Filter for Fall 2024 in Cat_SF_enroll
      select(SF_18_ID, Primary.College_recode) %>%
      distinct() %>%
      rename(Primary_College_recode = Primary.College_recode) %>%
      count(Primary_College_recode, name = "N_users_program")
  ) %>%
  mutate(proportion = N_users_program / All_students_program)

Academic_program_table_spring2024 <- Headcount_students %>%
  filter(semester == "Spring 2024") %>% # Filter for Spring 2024
  count(Primary_College_recode, name = "All_students_program") %>%
  left_join(
    Cat_SF_enroll %>%
      filter(semester == "Spring 2024") %>% # Filter for Spring 2024 in Cat_SF_enroll
      select(SF_18_ID, Primary.College_recode) %>%
      distinct() %>%
      rename(Primary_College_recode = Primary.College_recode) %>%
      count(Primary_College_recode, name = "N_users_program")
  ) %>%
  mutate(proportion = N_users_program / All_students_program)

# write_named_csv(Academic_program_table)

Academic_program_table <- Headcount_students %>% 
  count(Primary_College_recode, name = "All_students_program") %>% 
  left_join(Cat_SF_enroll %>%   
              # filter(last_login2 > "2023-08-08 16:25:00 GMT") %>%
              select(SF_18_ID, Primary.College_recode) %>% 
              distinct() %>% 
              rename(Primary_College_recode = Primary.College_recode)    %>% 
              count(Primary_College_recode, name = "N_users_program")) %>% 
  mutate(proportion = N_users_program/All_students_program)

write_named_csv(Academic_program_table)

#### recode Campus ####
unique(Cat_SF_enroll$Campus)
Cat_SF_enroll$Campus[is.na(Cat_SF_enroll$Campus)] <- "Other"
# 
Cat_SF_enroll$Campus.recode1 <- recode(Cat_SF_enroll$Campus,
                                       "Arizona International" = "AZ International & Direct",
                                       "Arizona International Direct" = "AZ International & Direct",
                                       "Arizona Online" = "Arizona Online",
                                       "Community Campus" = "Community Campus",
                                       "Distance" = "Distance",
                                       "Phoenix"="Phoenix",
                                       "Sierra Vista" = "Sierra Vista",
                                       "University of Arizona - Main" = "University of Arizona - Main",
                                       "NA" = "Other", " " = "Other")

Cat_SF_enroll$Campus.recode <- recode(Cat_SF_enroll$Campus,
                                      "Arizona International" = "Other",
                                      "Arizona International Direct" = "Other",
                                      "Arizona Online" = "Arizona Online",
                                      "Community Campus" = "Other",
                                      "Distance" = "Dist, PHX, SV",
                                      "Phoenix"="Dist, PHX, SV",
                                      "Sierra Vista" = "Dist, PHX, SV",
                                      "University of Arizona - Main" = "University of Arizona - Main",
                                      "NA" = "Other", " " = "Other")
Cat_SF_enroll$last_login2 <- as.Date(as.character(Cat_SF_enroll$Date), format='%Y%m%d')
########### fix the program campus ####
unique(Headcount_students$Campus)

Headcount_students$Campus_recode1 <- recode(Headcount_students$Campus,
                                                    "[GLBL] Arizona International" = "AZ International & Direct",
                                                    "[GLBD] Arizona International Direct" = "AZ International & Direct",
                                                    "[ONLN] Arizona Online" = "Arizona Online", 
                                                    "[CMNTY] Community Campus" = "Community Campus", 
                                                    "[DIST] Distance" = "Distance", 
                                                    "[PHX] Phoenix"="Phoenix",
                                                    "[SOUTH] Sierra Vista" = "Sierra Vista", 
                                                    # "Southern Arizona" = "Southern Arizona", 
                                                    "[MAIN] University of Arizona - Main" = "University of Arizona - Main")
Headcount_students$Campus_recode <- recode(Headcount_students$Campus,
                                                   "[GLBL] Arizona International" = "Other",
                                                   "[GLBD] Arizona International Direct" = "Other",
                                                   "[ONLN] Arizona Online" = "Arizona Online", 
                                                   "[CMNTY] Community Campus" = "Other", 
                                                   "[DIST] Distance" = "Dist, PHX, SV", 
                                                   "[PHX] Phoenix"="Dist, PHX, SV",
                                                   "[SOUTH] Sierra Vista" = "Dist, PHX, SV", 
                                                   "[MAIN] University of Arizona - Main" = "University of Arizona - Main", 
                                                   "NA" = "Other")
# defining cat date filter
Cat_date_filter <- Cat_SF_enroll %>%
  filter(last_login2 > "2024-01-01 06:25:00 GMT")

Cat_date_filter <- Cat_date_filter %>%
  mutate(Goals = ifelse(!is.na(Goal), "G", ""),
         Appts = ifelse(!is.na(Appt), "A", ""),
         Cases = ifelse(!is.na(Case), "C", ""),
         Edits = ifelse(!is.na(Edit), "E", ""),
         Events = ifelse(!is.na(Event), "eV", ""),
         # News = ifelse(!is.na(News), "N", ""),
         Total = str_c(Goals, Cases, Appts, Edits, Events)) # omit | (!is.na(Goal1)


Cat_class_users_count <-  Cat_date_filter  %>%  
  select(SF_18_ID, Email, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  count(Class_Standing_recode, name = "total_class_users")

# Cat_date_filter$Campus.recode <- recode(Cat_date_filter$Campus,
#                                         "Arizona International" = "Other",
#                                         "Arizona International Direct" = "Other",
#                                         "Arizona Online" = "Arizona Online", 
#                                         "Distance" = "Other", 
#                                         "Phoenix"="Other",
#                                         "Sierra Vista" = "Other", 
#                                         "University of Arizona - Main" = "University of Arizona - Main", 
#                                         "NA" = "Other")
# Cat_date_filter$Campus.recode1 <- recode(Cat_date_filter$Campus,
#                                          "Arizona International" = "AZ International & Direct",
#                                          "Arizona International Direct" = "AZ International & Direct",
#                                          "Arizona Online" = "Arizona Online", 
#                                          # "[CMNTY] Community Campus" = "Community Campus", 
#                                          "Distance" = "Distance", 
#                                          "Phoenix"="Phoenix",
#                                          "Sierra Vista" = "Sierra Vista", 
#                                          "University of Arizona - Main" = "University of Arizona - Main", 
#                                          "NA" = "Other")
#### campus tables ####
Campus_table1 <- Headcount_students %>%
  count(Campus_recode, name = "All_students_campus")%>% 
  left_join(Cat_date_filter %>%
              select(SF_18_ID, Campus.recode) %>%
              distinct() %>%
              rename(Campus_recode = Campus.recode)    %>%
              count(Campus_recode, name = "N_users_campus")) %>%
  mutate(proportion = N_users_campus/All_students_campus)

write_named_csv(Campus_table1)


Campus_table <- Headcount_students %>%
  count(Campus_recode1, name = "All_students_campus")%>% 
  left_join(Cat_date_filter %>%
              select(SF_18_ID, Campus.recode1) %>%
              distinct() %>%
              rename(Campus_recode1 = Campus.recode1)    %>%
              count(Campus_recode1, name = "N_users_campus")) %>%
  mutate(proportion1 = N_users_campus/All_students_campus)

write_named_csv(Campus_table)

#### detailed class tables ####
Cat_SF_users_count <- Cat_date_filter  %>% 
  select(SF_18_ID, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  rename(Class_standing_recode = Class_Standing_recode) %>% 
  count(Class_standing_recode, name = "N_users")

detailed_class_standing_table <- Headcount_students %>%
  count(Class_standing_recode,name = "All_students") %>% 
  left_join(Cat_SF_users_count) %>%
  mutate(proportion = N_users/All_students)

write_named_csv(detailed_class_standing_table)

# write_named_csv(Cat_date_filter)

#### rename effective user id ####
Cat_date_filter_student <- Cat_date_filter %>% 
  rename(`App-instance ID` = SF_18_ID)

#delete if email is blank
Cat_date_filter2_student <- Cat_date_filter_student[!(is.na(Cat_date_filter_student$Email) |Cat_date_filter_student$Email==""), ]


#### write Cat date filter to file ####
# Cat_date_filter_student <- as.data.frame(Cat_date_filter)
write_named_csv(Cat_date_filter_student)

write_named_csv(Headcount_students)
# who are the others
other_students<- Cat_date_filter_student %>% 
  filter(Class_Standing_recode=="Other")

