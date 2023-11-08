# Jung Mee Park
# 11-06-2023
# retrieving catcloud data using a new skuid tag

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

#### read in excel file ####

# merge all CC users
CC_all <- read_csv("initial_data/Fall2023/CC_all_Oct21-Oct31-2023.csv", skip=6)
CC_all <- CC_all %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(9:10))

# appointment users
CC_appt1 <- read_csv("initial_data/Fall2023/appt-users-newID.csv", skip = 6)
CC_appt1  <- CC_appt1 %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(-c(7:8))  
# 
CC_appt1 <- rename(CC_appt1, Appt.Sessions = Sessions) #, Namespace.ID = `Namespace ID`, Stream.name = `Stream name`, App.instance.ID = `App-instance ID`
CC_appt1 <- CC_appt1 %>%
  select(`Effective user ID`, Appt.Sessions)
# 
CC_appt1$`Effective user ID` <- as.character(CC_appt1$`Effective user ID`)

# add and edit users
CC_edit <- read_csv("initial_data/Fall2023/add-edit-users-newID.csv", skip = 6)
CC_edit <- CC_edit %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(-c(7:8))  

CC_edit <- rename(CC_edit, Edit.Sessions = Sessions)
CC_edit <- CC_edit %>% 
  select(`Effective user ID`, Edit.Sessions)

# # cases users
# CC_cases <- read_csv("initial_data/Fall2023/cases_users_Aug24_2023.csv", skip = 6)
# CC_cases <- CC_cases %>% 
#   filter(!is.na(`Event name`)) %>% 
#   filter(Totals != 'Event count') %>% 
#   select(-c(1,8)) 
# 
# CC_cases <- rename(CC_cases, `App-instance ID` = `Event name`)
# CC_cases <- rename(CC_cases, Cases.Sessions = `Totals`)
# CC_cases <- CC_cases %>% 
#   select(`App-instance ID`, Cases.Sessions)
# 
# CC_cases$Cases.Sessions <- as.numeric(CC_cases$Cases.Sessions)

# goals users
CC_goals <- read_csv("initial_data/Fall2023/goal-users-newID.csv", skip = 6)
CC_goals <- CC_goals %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(7:8))

CC_goals <- rename(CC_goals, Goal.Sessions = Sessions)
CC_goals <- CC_goals %>%
  select(`Effective user ID`, Goal.Sessions)


# add edits, goals, and appts to all CC users
CC_all <- left_join(CC_all, CC_appt1) %>% 
  left_join(CC_edit) %>% 
  left_join(CC_goals) 
# left_join(CC_cases)
# Cat_UserID <- CC_all
# Cat_UserID <- CC_all %>% 
#   filter(`Namespace ID` == "USER_ID") # keep only the cases we can track

CC_all <- CC_all %>% 
  select(-`Namespace ID`, -Segment, -`Stream name`)
# read in SF csv
all_users_SF <- read_csv("initial_data/Fall2023/SF_UserID.csv")
#### subset sf_users by profile

unique(all_users_SF$Profile)
# go to salesforce for this data

sf_users_IDs_emails_Fall2023 <- all_users_SF %>%
  filter(Profile == "Employee Community User" | Profile == "All Student Community User" | Profile == "Trellis User" )

#### merge sfcontact data with google
Cat_SF <- left_join(CC_all, all_users_SF, by = c(`Effective user ID` = "User ID")) 


Cat_SF <- Cat_SF %>% 
  select(`Effective user ID`, Sessions, Date,
         Appt.Sessions, Edit.Sessions,Goal.Sessions,
         # Cases.Sessions,
         Email, `First Name`, `Last Name`, `Last Login`) %>% 
  distinct()

#### merge all current students based on emails to CatSF ####
#### merge in new sf student data ####
# student_enrollment_from_sf_oct <- read_csv("initial_data/Fall2023/all_student_enrollment_Oct2023.csv")
student_enrollment_from_sf <- read_csv("initial_data/Fall2023/all_active_student_Nov2023.csv")

# delete if email is blank
# student_enrollment_from_sf_oct <- student_enrollment_from_sf_oct[!(is.na(student_enrollment_from_sf_oct$Email) | student_enrollment_from_sf_oct$Email==""), ]

student_enrollment_from_sf <- student_enrollment_from_sf[!(is.na(student_enrollment_from_sf$Email) | student_enrollment_from_sf$Email==""), ]

Cat_SF_enroll <- left_join(Cat_SF, student_enrollment_from_sf) %>% 
  distinct()# no need to add , by = c("Email"="Email")
# does not work with by = c("App-instance ID" = "Contact ID"); it should be userid

Cat_SF_enroll %>% 
  select(`Effective user ID`, `Primary College`) %>%
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
                                              "Prof Year 1" = "Graduate", "Prof Year 2" = "Graduate",
                                              "Prof Year 3" = "Graduate", "Prof Year 4" = "Graduate", 
                                              "Doctoral" = "Graduate", "NA" = "Other")


Cat_SF_enroll$Class_Standing_recode <- factor(Cat_SF_enroll$Class_Standing_recode, 
                                              levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate", "Other"))

unique(Cat_SF_enroll$Class_Standing_recode)

# delete if email is blank
Cat_SF_enroll_2 <- Cat_SF_enroll[!(is.na(Cat_SF_enroll$Email) | Cat_SF_enroll$Email==""), ]

Cat_SF_enroll_3 <- Cat_SF_enroll_2 %>% 
  distinct(`Effective user ID`, .keep_all=TRUE)

#### headcounts with student info #### 
### from uaccess

Headcount_students <- read_csv("initial_data/Fall2023/Headcount Details(2)_unique_rows.csv")
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

# df$new_variable <- recode(df$old_variable,  "Old Name" = "New Name", "Old Name2" = "New Name")

Headcount_students <- Headcount_students %>%
  mutate(Academic.Program_recode = factor(`Academic Program`) %>%
           fct_recode(
             "Rogers College of Law" = "James E. Rogers College of Law",
             "Rogers College of Law" = "Law Doctoral",
             "Rogers College of Law" = "Law Masters",
             "Rogers College of Law" = "Law Non-Degree Seeking",
             "Graduate Coursework" = "Graduate Certificate",
             "Graduate Coursework" = "Graduate Degree Seeking",
             "Graduate Coursework" = "Graduate Non-Degree Seeking",
             "Undergraduate Coursework" = "Undergrad Non-Degree Seeking",
             "Undergraduate Coursework" = "Undergraduate Certificate"))

#### Change Last Login to date format ####
#### subset the dates to post 8/15 ####
# Cat_SF_enroll$last_login2 <- strptime(Cat_SF_enroll$Last.Login,"%m/%d/%Y %H:%M",tz="GMT")
Cat_SF_enroll$last_login2 <- as.Date(as.character(Cat_SF_enroll$Date), format='%Y%m%d')


# create a counter if there is a appt session
Cat_SF_enroll <- Cat_SF_enroll %>%
  dplyr::mutate(Appt = ifelse(Appt.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Edit = ifelse(Edit.Sessions > 0, 1, 0)) %>% 
  # dplyr::mutate(Case = ifelse(Cases.Sessions > 0, 1, 0)) %>% 
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
             # "Rogers College of Law" = "Law Doctoral",
             "Rogers College of Law" = "Law Masters",
             # "Rogers College of Law" = "Law Non-Degree Seeking",
             "Graduate Coursework" = "Graduate Certificate",
             "Graduate Coursework" = "Graduate Non-Degree Seeking",
             "Graduate Coursework" = "Graduate Degree Seeking",
             "Undergraduate Coursework" = "Undergrad Non-Degree Seeking",
             "Undergraduate Coursework" = "Undergraduate Certificate"))

# Cat_SF_enroll_test <- fct_explicit_na(Cat_SF_enroll$Primary.College_recode, "Other")
# 
unique(Cat_SF_enroll$Primary.College_recode)
summary(Cat_SF_enroll$Primary.College_recode)
# 
#### subset the dates to post 8/15 ####
# Cat_SF_enroll$last_login2 <- strptime(Cat_SF_enroll$Last.Login,"%m/%d/%Y %H:%M",tz="GMT")

#### Academic Program Table ####
Academic_program_table <- Headcount_students %>% 
  count(Academic.Program_recode, name = "All_students_program") %>% 
  left_join(Cat_SF_enroll %>%   
              filter(last_login2 > "2023-08-08 16:25:00 GMT") %>% 
              select(`Effective user ID`, Primary.College_recode) %>% 
              distinct() %>% 
              rename(Academic.Program_recode = Primary.College_recode)    %>% 
              count(Academic.Program_recode, name = "N_users_program")) %>% 
  mutate(proportion = N_users_program/All_students_program)

write_named_csv(Academic_program_table)

#### new dataset with filtered dates ####
Cat_date_filter <- Cat_SF_enroll %>% 
  filter(last_login2 > "2023-08-08 06:25:00 GMT")

Cat_date_filter  %>%
  select(`Effective user ID`, Email, Primary.College_recode) %>%
  distinct() %>%
  group_by(Primary.College_recode) %>%
  count()

# unique(Headcount_Details.csv$Academic.Level)
unique(Headcount_students$Academic.Level_recode)

Headcount_students %>% 
  group_by(Academic.Level_recode) %>% 
  count() %>% 
  ungroup()

Cat_SF_enroll %>% 
  select(`Effective user ID`, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  count() %>% 
  ungroup()


### appt sessions ###
Cat_class_users_count <-  Cat_date_filter  %>%  
  select(`Effective user ID`,  Email, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  count(Class_Standing_recode, name = "total_class_users")


Cat_date_filter <- Cat_date_filter %>%
  mutate(Goals = ifelse(!is.na(Goal), "G", ""), 
         Appts = ifelse(!is.na(Appt), "A", ""),
         # Cases = ifelse(!is.na(Case), "C", ""),
         Edits = ifelse(!is.na(Edit), "E", ""),
         Total = str_c(Goals, Appts, Edits)) # omit | (!is.na(Goal1) 

# Total = str_c(Goals, Appts, Cases, Edits)) # omit | (!is.na(Goal1) from line 348

Cat_date_filter %>% 
  select(`Effective user ID`, Sessions, Total) %>% 
  distinct() %>% 
  group_by(Total) %>%
  count(Total, name = "App_users")


Cat_date_filter %>% 
  select(`Effective user ID`, Email) %>% 
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
  select(`Effective user ID`, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  count()

unique(Headcount_students$Campus)

Headcount_students$Program.Campus_recode1 <- recode(Headcount_students$Campus,
                                                    "Arizona International" = "AZ International & Direct",
                                                    "Arizona International Direct" = "AZ International & Direct",
                                                    "Arizona Online" = "Arizona Online", 
                                                    "Community Campus" = "Community Campus", 
                                                    "Distance" = "Distance", 
                                                    "Phoenix"="Phoenix",
                                                    "Sierra Vista" = "Sierra Vista", 
                                                    "Southern Arizona" = "Southern Arizona", 
                                                    "University of Arizona - Main" = "University of Arizona - Main")
Headcount_students$Program.Campus_recode <- recode(Headcount_students$Campus,
                                                   "Arizona International" = "Other",
                                                   "Arizona International Direct" = "Other",
                                                   "Arizona Online" = "Arizona Online", 
                                                   "Community Campus" = "Other", 
                                                   "Sierra Vista" = "Other", 
                                                   "Distance" = "Other", "Phoenix"="Other",
                                                   "Southern Arizona" = "Other", 
                                                   "University of Arizona - Main" = "University of Arizona - Main", "NA" = "Other")

unique(Cat_SF_enroll$Campus)

Cat_SF_enroll$Campus.recode1 <- recode(Cat_SF_enroll$Campus,
                                       "Arizona International" = "AZ International & Direct",
                                       "Arizona International Direct" = "AZ International & Direct",
                                       "Arizona Online" = "Arizona Online", 
                                       "Community Campus" = "Community Campus", 
                                       "Distance" = "Distance", "Phoenix"="Phoenix",
                                       "Southern Arizona" = "Southern Arizona", 
                                       "University of Arizona - Main" = "University of Arizona - Main", "NA" = "Other")

Cat_SF_enroll$Campus.recode <- recode(Cat_SF_enroll$Campus,
                                      "Arizona International" = "Other",
                                      "Arizona International Direct" = "Other",
                                      "Arizona Online" = "Arizona Online", 
                                      "Community Campus" = "Other", 
                                      "Distance" = "Other", "Phoenix"="Other",
                                      "Southern Arizona" = "Other", 
                                      "Sierra Vista" = "Other", 
                                      "University of Arizona - Main" = "University of Arizona - Main", "NA" = "Other")

# unique(Cat_date_filter$Campus)
Cat_date_filter$Campus.recode <- recode(Cat_date_filter$Campus,
                                        "Arizona International" = "Other",
                                        "Arizona International Direct" = "Other",
                                        "Arizona Online" = "Arizona Online", 
                                        "Community Campus" = "Other", 
                                        "Distance" = "Other", "Phoenix"="Other",
                                        "Sierra Vista" = "Other", 
                                        "Southern Arizona" = "Other", 
                                        "University of Arizona - Main" = "University of Arizona - Main", "NA" = "Other")
Cat_date_filter$Campus.recode1 <- recode(Cat_date_filter$Campus,
                                         "Arizona International" = "AZ International & Direct",
                                         "Arizona International Direct" = "AZ International & Direct",
                                         "Arizona Online" = "Arizona Online", 
                                         "Community Campus" = "Community Campus", 
                                         "Distance" = "Distance", "Phoenix"="Phoenix",
                                         "Southern Arizona" = "Southern Arizona", 
                                         "University of Arizona - Main" = "University of Arizona - Main", "NA" = "Other")
Cat_date_filter <- Cat_date_filter %>% 
  select(-First.Name, -Last.Name, -`Full Name`)

write_named_csv(Cat_date_filter)

Cat_date_filter %>% 
  select(`App-instance ID`) %>%
  distinct() %>% 
  count()


Cat_date_filter %>%   
  select(`App-instance ID`, Class_Standing_recode) %>% 
  distinct() %>% 
  # group_by(Class_standing_recode) %>% 
  count()

Cat_SF_enroll %>%   
  filter(last_login2 > "2023-08-08 06:25:00 GMT") %>% 
  select(`App-instance ID`, last_login2) %>% 
  distinct() %>% 
  group_by(last_login2) %>% 
  count()

#### campus tables ####
Campus_table <- Headcount_students %>%
  count(Program.Campus_recode, name = "All_students_campus")%>% 
  left_join(Cat_date_filter %>%
              select(`App-instance ID`, Campus.recode) %>%
              distinct() %>%
              rename(Program.Campus_recode = Campus.recode)    %>%
              count(Program.Campus_recode, name = "N_users_campus")) %>%
  mutate(proportion = N_users_campus/All_students_campus)

# Program.Campus_recode is from the headcount, Campus_recode is from the Cat_date_filter
Campus_table2 <- Headcount_students %>% 
  count(Program.Campus_recode1, name = "All_students_campus")%>% 
  left_join(Cat_date_filter %>%
              select(`App-instance ID`, Campus.recode1) %>% 
              distinct() %>% 
              rename(Program.Campus_recode1 = Campus.recode1)    %>% 
              count(Program.Campus_recode1, name = "N_users_campus")) %>% 
  mutate(proportion = N_users_campus/All_students_campus)
write_named_csv(Campus_table)
write_named_csv(Campus_table2)
