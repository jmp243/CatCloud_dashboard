# Jung Mee Park
# 11-06-2023
# retrieving catcloud data using a new skuid tag
# last run 07-01-2024

#### load libraries ####
# library("googleAnalyticsR")
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
    "~/Documents/Trellis/CatCloud_dashboard/clean_data/clean_spring2024/",
    deparse(substitute(x)),".csv"))

#### read in excel file ####
# merge all CC users
CC_all1 <- read_csv("initial_data/Spring2024/CC_all_Jan8-Mar15-2024.csv", skip=6)
CC_all2 <- read_csv("initial_data/Spring2024/CC_all_Mar16-May12-2024.csv", skip=6)
CC_all3 <- read_csv("initial_data/Spring2024/CC_all_May13-Jun30-2024.csv", skip=6)

CC_all1 <- CC_all1 %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(9:10)) 

CC_all2 <- CC_all2 %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(9:10))

CC_all3 <- CC_all3 %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(9:10)) 

CC_all <- rbind(CC_all1, CC_all2, CC_all3)

# appointment users
CC_appt1 <- read_csv("initial_data/Spring2024/appt-users-newID-Jan8-Mar15-2024.csv", skip = 6)
CC_appt2 <- read_csv("initial_data/Spring2024/appt-users-newID-Mar16-May12-2024.csv", skip = 6)
CC_appt3 <- read_csv("initial_data/Spring2024/appt-users-newID-May13-Jun30-2024.csv", skip = 6)

CC_appt1  <- CC_appt1 %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(-c(7:8))   
CC_appt2  <- CC_appt2 %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(-c(7:8))   
CC_appt3  <- CC_appt3 %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(-c(7:8))   

CC_appt <- rbind(CC_appt1, CC_appt2, CC_appt3)
# 
CC_appt <- rename(CC_appt, Appt.Sessions = Sessions) #, Namespace.ID = `Namespace ID`, Stream.name = `Stream name`, App.instance.ID = `App-instance ID`

CC_appt <- CC_appt %>%
  select(`Effective user ID`, Appt.Sessions) %>% 
  distinct() 
# 
CC_appt$`Effective user ID` <- as.character(CC_appt$`Effective user ID`)

# add and edit users
CC_edit <- read_csv("initial_data/Spring2024/add-edit-users-newID-Jan8-Mar30-2024.csv", skip = 6)
CC_edit2 <- read_csv("initial_data/Spring2024/add-edit-users-newID-Apr1-Jun30-2024.csv", skip = 6)

CC_edit <- rbind(CC_edit, CC_edit2)

CC_edit <- CC_edit %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(-c(7:8))  

CC_edit <- rename(CC_edit, Edit.Sessions = Sessions)
CC_edit <- CC_edit %>% 
  select(`Effective user ID`, Edit.Sessions) %>% 
  distinct()

# cases users
CC_cases1 <- read_csv("initial_data/Spring2024/cases-Jan8-Mar31-2024.csv", skip = 7)
CC_cases2 <- read_csv("initial_data/Spring2024/cases-Apr1-Jun30-2024.csv", skip = 7)
CC_cases1 <- rbind(CC_cases1, CC_cases2)

CC_cases <- CC_cases1 %>%
  filter(!is.na(Date)) %>%
  # filter(Totals != 'Event count') %>%
  select(`Effective user ID`, `Event count`, Sessions)

# CC_cases <- rename(CC_cases, `App-instance ID` = `Event name`)
CC_cases <- rename(CC_cases, Cases.Sessions = Sessions)
CC_cases <- CC_cases %>%
  select(`Effective user ID`, Cases.Sessions) %>% 
  distinct()

CC_cases$Cases.Sessions <- as.numeric(CC_cases$Cases.Sessions)

# CC_cases <- CC_cases %>%
#   select(`Effective user ID`, Cases.Sessions) %>% 
#   distinct()
# 
# CC_cases$Cases.Sessions <- as.numeric(CC_cases$Cases.Sessions)

# events users
CC_events1 <- read_csv("initial_data/Spring2024/events-Jan8-Mar31-2024.csv", skip = 6)
CC_events2 <- read_csv("initial_data/Spring2024/events-Apr1-Jun30-2024.csv", skip = 6)
CC_events1 <- rbind(CC_events1, CC_events2)

CC_events <- CC_events1 %>%
  filter(!is.na(`Stream name`)) %>%
  # filter(Totals != 'Event count') %>%
  select(`Effective user ID`, `Total users`) %>% 
  rename(Events.Sessions = `Total users`)

# CC_cases <- rename(CC_cases, `App-instance ID` = `Event name`)
# CC_cases <- rename(CC_cases, Cases.Sessions = Sessions)

# goals users
CC_goals1 <- read_csv("initial_data/Spring2024/goal-users-newID-Jan8-Mar31-2024.csv", skip = 6)
CC_goals2 <- read_csv("initial_data/Spring2024/goal-users-newID-Apr1-Jun30-2024.csv", skip = 6)
CC_goals1 <- rbind(CC_goals1, CC_goals2)

CC_goals <- CC_goals1 %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(7:8))

CC_goals <- rename(CC_goals, Goal.Sessions = Sessions)
CC_goals <- CC_goals %>%
  select(`Effective user ID`, Goal.Sessions) %>% 
  distinct()



#### read in SF csv ####
all_users_SF <- read_csv("initial_data/Spring2024/SF_UserID.csv")

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
all_users_SF <- all_users_SF %>% mutate(`SF_18_ID` = SFID_Convert(`User ID`)) %>% 
  rename(SF_15_ID = 'User ID')

#### subset sf_users by profile
unique(all_users_SF$Profile)
# go to salesforce for this data

# try running the analysis without this
all_users_SF <- all_users_SF %>%
  filter(Profile == "Employee Community User" | Profile == "All Student Community User" | Profile == "Trellis User" )

#### merge sfcontact data with google
# rename Effective user id variables to corresponding SFID
# classApp is shortening the SF ID

CC_all <- CC_all %>% rename(SF_18_ID = `Effective user ID`)
CC_appt <- CC_appt %>% rename(SF_18_ID = `Effective user ID`)
CC_case <- CC_cases %>% rename(SF_18_ID = `Effective user ID`)
CC_goal <- CC_goals %>% rename(SF_15_ID = `Effective user ID`)
CC_edit <- CC_edit %>% rename(SF_15_ID = `Effective user ID`)
CC_event <- CC_events %>% rename(SF_18_ID = `Effective user ID`)

CC_all <- left_join(CC_all, all_users_SF) 
CC_appt <- left_join(CC_appt, all_users_SF)
CC_case <- left_join(CC_case, all_users_SF)
CC_goal <- left_join(CC_goal, all_users_SF)
CC_edit <- left_join(CC_edit, all_users_SF)
CC_event <- left_join(CC_event, all_users_SF)
#### merge the 5 data frame with SF data ####
Cat_SF <- CC_all %>% left_join(CC_appt) %>% left_join(CC_case) %>% left_join(CC_goal) %>% left_join(CC_edit) %>% left_join(CC_event)

Cat_SF <- Cat_SF %>% 
  select(-`Namespace ID`, -Segment, -`Stream name`) %>% 
  distinct()

# Cat_SF <- left_join(CC_all, all_users_SF, by = c(`Effective user ID` = "User ID")) 

Cat_SF <- Cat_SF %>% 
  select(SF_15_ID, SF_18_ID, Sessions, Date,
         Appt.Sessions, Edit.Sessions,Goal.Sessions,
         Cases.Sessions, Events.Sessions,
         Email, `First Name`, `Last Name`, `Last Login`) %>% 
  distinct()

#### merge all current students based on emails to CatSF ####
# student_enrollment_from_sf_oct <- read_csv("initial_data/Fall2023/all_student_enrollment_Oct2023.csv")
student_enrollment_from_sf <- read_csv("initial_data/Spring2024/SF_student_enrollment_Jul1-2024.csv")
 
# delete if email is blank
# student_enrollment_from_sf_oct <- student_enrollment_from_sf_oct[!(is.na(student_enrollment_from_sf_oct$Email) | student_enrollment_from_sf_oct$Email==""), ]
student_enrollment_from_sf <- student_enrollment_from_sf[!(is.na(student_enrollment_from_sf$Email) | student_enrollment_from_sf$Email==""), ]

Cat_SF_enroll <- left_join(Cat_SF, student_enrollment_from_sf) %>% 
  distinct()# no need to add , by = c("Email"="Email")


# # make unique one name per row
# Cat_SF_enroll_3 <- Cat_SF_enroll_2 %>% 
#   distinct(`Effective user ID`, .keep_all=TRUE)

# does not work with by = c("App-instance ID" = "Contact ID"); it should be userid

Cat_SF_enroll %>% 
  select(SF_18_ID, SF_15_ID, `Primary College`) %>%
  distinct() %>%
  group_by(`Primary College`) %>% 
  count() 

# Cat_SF_enroll <- left_join(Cat_SF_enroll, classApp_db)
unique(Cat_SF_enroll$`Primary College`)
unique(Cat_SF_enroll$Career)

Cat_SF_enroll$Career <- factor(Cat_SF_enroll$Career, 
                               levels=c("Undergraduate", "Graduate", "Law", "Medical School", 
                                        "Pharmacy", "Veterinary Medicine", "NA"))

unique(Cat_SF_enroll$`Class Standing`)


Cat_SF_enroll$Class_Standing_recode <- recode(Cat_SF_enroll$`Class Standing`,"Freshman" = "Freshman", "Sophomore" = "Sophomore", 
                                              "Junior" = "Junior", "Senior" = "Senior", 
                                              "Graduate" = "Graduate", "Masters" = "Graduate",
                                              "Prof 1" = "Graduate", "Prof 2" = "Graduate",
                                              "Prof 3" = "Graduate", "Prof 4" = "Graduate", 
                                              "Doctoral" = "Graduate", "NA" = "Other")


Cat_SF_enroll$Class_Standing_recode <- factor(Cat_SF_enroll$Class_Standing_recode, 
                                              levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate", "Other"))

unique(Cat_SF_enroll$Class_Standing_recode)

#### headcounts with student info #### 
### from uaccess

Headcount_students <- read_csv("initial_data/Spring2024/Spring and Summer 2024 Headcount Details.csv")

# View(Headcount_Details)
unique(`Headcount_students`$`Academic Level`)

`Headcount_students`$Academic.Level_recode <- recode(`Headcount_students`$`Academic Level`,
                                                     "Freshman" = "Freshman", "Sophomore" = "Sophomore", 
                                                     "Junior" = "Junior", "Senior" = "Senior", 
                                                     "Graduate" = "Graduate", "Masters" = "Graduate",
                                                     "Professional Year 1" = "Graduate", "Professional Year 2" = "Graduate",
                                                     "Professional Year 3" = "Graduate", "Professional Year 4" = "Graduate", 
                                                     "Doctoral" = "Graduate", "NA" = "Other")

`Headcount_students`$Academic.Level_recode <- factor(`Headcount_students`$Academic.Level_recode, 
                                                     levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate", "Other"))

unique(`Headcount_students`$Academic.Level_recode)

table(Headcount_students$Academic.Level_recode)

# df$new_variable <- recode(df$old_variable,  "Old Name" = "New Name", "Old Name2" = "New Name")
unique(Headcount_students$`Academic Program`)

Headcount_students <- Headcount_students %>%
  mutate(Academic.Program_recode = factor(`Academic Program`) %>%
           fct_recode(
             "Rogers College of Law" = "James E. Rogers College of Law",
             "Rogers College of Law" = "Law Doctoral",
             "Rogers College of Law" = "Law Masters",
             "Rogers College of Law" = "Law Non-Degree Seeking",
             "College of Veterinary Medicine" = "Vet. Medicine",
             "Graduate Coursework" = "Graduate Certificate",
             "Graduate Coursework" = "Graduate Degree Seeking",
             "Graduate Coursework" = "Graduate Non-Degree Seeking",
             "Undergraduate Coursework" = "Undergrad Non-Degree Seeking",
             "Undergraduate Coursework" = "Undergraduate Certificate"))



#### Change Last Login to date format ####
#### subset the dates to post 8/15 ####
Cat_SF_enroll$last_login2 <- as.Date(as.character(Cat_SF_enroll$Date), format='%Y%m%d')

# create a counter if there is a appt session
Cat_SF_enroll <- Cat_SF_enroll %>%
  dplyr::mutate(Appt = ifelse(Appt.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Edit = ifelse(Edit.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Case = ifelse(Cases.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Event = ifelse(Events.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Goal = ifelse(Goal.Sessions > 0, 1, 0))

#### find number of categories in given groups ####
# # without student info

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

# Cat_SF_enroll_test <- fct_explicit_na(Cat_SF_enroll$Primary.College_recode, "Other")
# 
unique(Cat_SF_enroll$Primary.College_recode)
summary(Cat_SF_enroll$Primary.College_recode)

#### subset the dates to post 8/15 ####
# Cat_SF_enroll$last_login2 <- strptime(Cat_SF_enroll$Last.Login,"%m/%d/%Y %H:%M",tz="GMT")

#### Academic Program Table ####
Academic_program_table <- Headcount_students %>% 
  count(Academic.Program_recode, name = "All_students_program") %>% 
  left_join(Cat_SF_enroll %>%   
              filter(last_login2 > "2023-08-08 16:25:00 GMT") %>%
              select(SF_18_ID, Primary.College_recode) %>% 
              distinct() %>% 
              rename(Academic.Program_recode = Primary.College_recode)    %>% 
              count(Academic.Program_recode, name = "N_users_program")) %>% 
  mutate(proportion = N_users_program/All_students_program)


# Cat_SF_enroll %>% 
#   select(SF_18_ID, Primary.College_recode) %>% 
#   distinct() %>% 
#   count(Primary.College_recode)
#   
  

Academic_program_table_email <- Headcount_students %>% 
  count(Academic.Program_recode, name = "All_students_program") %>% 
  left_join(Cat_SF_enroll %>%   
              filter(last_login2 > "2023-08-08 16:25:00 GMT") %>% 
              select(Email, Primary.College_recode) %>% 
              distinct() %>% 
              rename(Academic.Program_recode = Primary.College_recode)    %>% 
              count(Academic.Program_recode, name = "N_users_program")) %>% 
  mutate(proportion = N_users_program/All_students_program)

write_named_csv(Academic_program_table)

# unique(Headcount_Details.csv$Academic.Level)
unique(Headcount_students$Academic.Level_recode)

Headcount_students %>% 
  group_by(Academic.Level_recode) %>% 
  count() %>% 
  ungroup()

Cat_SF_enroll %>% 
  select(SF_18_ID, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  count() %>% 
  ungroup()

#### recode Campus ####
unique(Cat_SF_enroll$Campus)

Cat_SF_enroll$Campus.recode1 <- recode(Cat_SF_enroll$Campus,
                                       "Arizona International" = "AZ International & Direct",
                                       "Arizona International Direct" = "AZ International & Direct",
                                       "Arizona Online" = "Arizona Online", 
                                       # "Community Campus" = "Community Campus", 
                                       "Distance" = "Distance", 
                                       "Phoenix"="Phoenix",
                                       "Sierra Vista" = "Sierra Vista", 
                                       "University of Arizona - Main" = "University of Arizona - Main",
                                       "NA" = "Other")

Cat_SF_enroll$Campus.recode <- recode(Cat_SF_enroll$Campus,
                                      "Arizona International" = "Other",
                                      "Arizona International Direct" = "Other",
                                      "Arizona Online" = "Arizona Online", 
                                      # "[CMNTY] Community Campus" = "Other", 
                                      "Distance" = "Other", 
                                      "Phoenix"="Other",
                                      "Sierra Vista" = "Other", 
                                      "University of Arizona - Main" = "University of Arizona - Main", 
                                      "NA" = "Other")

ALL_DATES <- Cat_SF_enroll %>%   
  filter(last_login2 > "2024-01-01 06:25:00 GMT") %>% 
  select(SF_18_ID, last_login2) %>% 
  distinct() %>% 
  group_by(last_login2) %>% 
  count()


#### new dataset with filtered dates ####
Cat_date_filter <- Cat_SF_enroll %>% 
  filter(last_login2 > "2024-01-01 06:25:00 GMT") 

Cat_date_filter  %>%
  select(SF_18_ID, Email, Primary.College_recode) %>%
  distinct() %>%
  group_by(Primary.College_recode) %>%
  count()


Cat_date_filter <- Cat_date_filter %>%
  mutate(Goals = ifelse(!is.na(Goal), "G", ""), 
         Appts = ifelse(!is.na(Appt), "A", ""),
         Cases = ifelse(!is.na(Case), "C", ""),
         Edits = ifelse(!is.na(Edit), "E", ""),
         Events = ifelse(!is.na(Event), "eV", ""),
         Total = str_c(Goals, Cases, Appts, Edits, Events)) # omit | (!is.na(Goal1) 
### appt sessions ###
Cat_class_users_count <-  Cat_date_filter  %>%  
  select(SF_18_ID, Email, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  count(Class_Standing_recode, name = "total_class_users")

# Total = str_c(Goals, Appts, Cases, Edits) # omit | (!is.na(Goal1) from line 348

Cat_date_filter %>% 
  select(SF_18_ID, Sessions, Total) %>% 
  distinct() %>% 
  group_by(Total) %>%
  count(Total, name = "App_users")


Cat_date_filter %>% 
  select(SF_18_ID, Email) %>% 
  distinct() %>% 
  count()
# count distinct emails
# Cat_date_filter %>%   
#   select(`Effective user ID`,  Campus, Email) %>% 
#   distinct() %>% 
#   count() # 33781 as of Jan 23

# unique(Headcount_students$`Academic Program`)

########### fix the program campus ####

Cat_date_filter %>%   
  select(SF_18_ID, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  count()

unique(Headcount_students$Campus)

Headcount_students$Program.Campus_recode1 <- recode(Headcount_students$Campus,
                                                    "[GLBL] Arizona International" = "AZ International & Direct",
                                                    "[GLBD] Arizona International Direct" = "AZ International & Direct",
                                                    "[ONLN] Arizona Online" = "Arizona Online", 
                                                    "[CMNTY] Community Campus" = "Community Campus", 
                                                    "[DIST] Distance" = "Distance", 
                                                    "[PHX] Phoenix"="Phoenix",
                                                    "[SOUTH] Sierra Vista" = "Sierra Vista", 
                                                    # "Southern Arizona" = "Southern Arizona", 
                                                    "[MAIN] University of Arizona - Main" = "University of Arizona - Main")
Headcount_students$Program.Campus_recode <- recode(Headcount_students$Campus,
                                                   "[GLBL] Arizona International" = "Other",
                                                   "[GLBD] Arizona International Direct" = "Other",
                                                   "[ONLN] Arizona Online" = "Arizona Online", 
                                                   "[CMNTY] Community Campus" = "Other", 
                                                   "[DIST] Distance" = "Other", 
                                                   "[PHX] Phoenix"="Other",
                                                   "[SOUTH] Sierra Vista" = "Other", 
                                                   "[MAIN] University of Arizona - Main" = "University of Arizona - Main", 
                                                   "NA" = "Other")

unique(Cat_date_filter$Campus.recode1)
Cat_date_filter$Campus.recode <- recode(Cat_date_filter$Campus,
                                        "Arizona International" = "Other",
                                        "Arizona International Direct" = "Other",
                                        "Arizona Online" = "Arizona Online", 
                                        "Distance" = "Other", 
                                        "Phoenix"="Other",
                                        "Sierra Vista" = "Other", 
                                        "University of Arizona - Main" = "University of Arizona - Main", 
                                        "NA" = "Other")
Cat_date_filter$Campus.recode1 <- recode(Cat_date_filter$Campus,
                                         "Arizona International" = "AZ International & Direct",
                                         "Arizona International Direct" = "AZ International & Direct",
                                         "Arizona Online" = "Arizona Online", 
                                         # "[CMNTY] Community Campus" = "Community Campus", 
                                         "Distance" = "Distance", 
                                         "Phoenix"="Phoenix",
                                         "Sierra Vista" = "Sierra Vista", 
                                         "University of Arizona - Main" = "University of Arizona - Main", 
                                         "NA" = "Other")
Cat_date_filter <- Cat_date_filter %>% 
  select(-`First Name`, -`Last Name`, -`Full Name`)

Cat_date_filter %>% 
  select(SF_18_ID) %>%
  distinct() %>% 
  count()

Cat_date_filter %>%   
  select(SF_18_ID, Class_Standing_recode) %>% 
  distinct() %>% 
  # group_by(Class_standing_recode) %>% 
  count()


#### campus tables ####
Campus_table <- Headcount_students %>%
  count(Program.Campus_recode, name = "All_students_campus")%>% 
  left_join(Cat_date_filter %>%
              select(SF_18_ID, Campus.recode) %>%
              distinct() %>%
              rename(Program.Campus_recode = Campus.recode)    %>%
              count(Program.Campus_recode, name = "N_users_campus")) %>%
  mutate(proportion = N_users_campus/All_students_campus)

# Program.Campus_recode is from the headcount, Campus_recode is from the Cat_date_filter
Campus_table2 <- Headcount_students %>% 
  count(Program.Campus_recode1, name = "All_students_campus")%>% 
  left_join(Cat_date_filter %>%
              select(SF_18_ID,Campus.recode1) %>% 
              distinct() %>% 
              rename(Program.Campus_recode1 = Campus.recode1)    %>% 
              count(Program.Campus_recode1, name = "N_users_campus")) %>% 
  mutate(proportion = N_users_campus/All_students_campus)
write_named_csv(Campus_table)
write_named_csv(Campus_table2)

#### detailed class tables ####
Cat_SF_users_count <- Cat_date_filter  %>% 
  select(SF_18_ID, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  rename(Academic.Level_recode = Class_Standing_recode) %>% 
  count(Academic.Level_recode, name = "N_users")

detailed_class_standing_table <- Headcount_students %>%
  count(Academic.Level_recode,name = "All_students") %>% 
  left_join(Cat_SF_users_count) %>%
  mutate(proportion = N_users/All_students)

write_named_csv(detailed_class_standing_table)

#### rename effective user id ####
Cat_date_filter <- Cat_date_filter %>% 
  rename(`App-instance ID` = SF_18_ID)

# delete if email is blank
Cat_date_filter2 <- Cat_date_filter[!(is.na(Cat_date_filter$Email) |Cat_date_filter$Email==""), ]

#### add employee ID ####
# Employees <- Employees %>% 
#   select(NetID, `EDS Affiliations`, `EDS Primary Affiliation`, `Parent Organization`)

#### add employee ID ####
Employees <- Employees %>% 
  select(NetID, `EDS Primary Affiliation`, `Parent Organization`) %>% 
  filter(!is.na(NetID)) # important to do a nonNA filter otherwise the merge will max memory limit


Cat_date_filter_long <- Cat_date_filter %>% left_join(Employees, relationship = "many-to-many") 

table(Cat_date_filter_long$`EDS Primary Affiliation`)

write_named_csv(Cat_date_filter_long)


# ####### data exploration to fix different counts
# vet_students <- Cat_date_filter %>% 
#   filter(Primary.College_recode == "College of Veterinary Medicine")
# 
# law_students <- Cat_date_filter %>% 
#   filter(Primary.College_recode == "Rogers College of Law")
# 
# other_logins <- Cat_date_filter %>% 
#   select(-last_login2, -Date, -`Last Login`) %>% 
#   filter(Primary.College_recode == "Other") %>% 
#   distinct()
#  
