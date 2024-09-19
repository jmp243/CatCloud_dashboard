# CatCloud data with engagement data and funnels
# student catcloud with new features
# last run 09-17-2024

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
    "~/Documents/Trellis/CatCloud_dashboard/clean_data/cumulative_data/Student/",
    deparse(substitute(x)),".csv"))

#### read in excel file ####
# doing an exploration with last 14 days from 8/23 to 9/5
# CC_all1 <- read_csv("initial_data/Fall2024/CC_all/CC_all_Aug23-Sept5-2024.csv", skip=6)
# 
# CC_all1 <- CC_all1 %>%
#   filter(!is.na(`Effective user ID`)) %>%
#   select(-c(14)) # remove a total columns
# 
# CC_all1 <- CC_all1 %>%
#   select(-c(9)) # total number of users are not needed as they are always 1 

#### merge all CC users ####
CC_all1 <- read_csv("initial_data/cumulative_data/CC_all/CC_all_Nov1-Nov30-2023.csv", skip=6)
CC_all2a <- read_csv("initial_data/cumulative_data/CC_all/CC_all_Dec1-Dec31-2023.csv", skip=6)
CC_all2b <- read_csv("initial_data/cumulative_data/CC_all/CC_all_Jan1-Feb29-2024.csv", skip=6)
CC_all3 <- read_csv("initial_data/cumulative_data/CC_all/CC_all_Mar1-Mar31-2024.csv", skip=6)
CC_all4 <- read_csv("initial_data/cumulative_data/CC_all/CC_all_Apr1-Apr30-2024.csv", skip=6)
CC_all5 <- read_csv("initial_data/cumulative_data/CC_all/CC_all_May1-May31-2024.csv", skip=6)
CC_all6a <- read_csv("initial_data/cumulative_data/CC_all/CC_all_Jun1-Jul31-2024.csv", skip=6) # new format
CC_all6b <- read_csv("initial_data/cumulative_data/CC_all/CC_all_Aug1-Aug31-2024.csv", skip=6) # new format
CC_all6c <- read_csv("initial_data/cumulative_data/CC_all/CC_all_Sept1-Sept16-2024.csv", skip=6) # new format

CC_all <- rbind(CC_all1, CC_all2a, CC_all2b, CC_all3, CC_all4, CC_all5, CC_all6a, CC_all6b, CC_all6c)

# CC_all $CC_Date <- as.Date(as.character(CC_all$Date), format='%Y%m%d')
CC_all <- CC_all %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(`Effective user ID`, Date, `Event count`, Sessions, Views, `Engagement rate`, 
         `User engagement`, `Elapsed time from last page request`, `Average session duration`) 


# elapsed time from last page request is in milliseconds
#### new variables exploration ####
# Condensed code chunk 

CC_all <- CC_all %>%
  mutate(
    Time_elapsed_in_seconds = `Elapsed time from last page request` / 1000,
    Avg_session_in_seconds = `Average session duration`,
    User_engagement_in_seconds = `User engagement`
  ) %>%
  select(-`Average session duration`, -`User engagement`)


#### cases users ####
CC_cases1 <- read_csv("initial_data/cumulative_data/cases/Cases_Nov1-Dec31-2023.csv", skip = 7)
CC_cases2 <- read_csv("initial_data/cumulative_data/cases/Cases_Jan1-Sept16-2024.csv", skip = 7)

CC_cases <- rbind(CC_cases1, CC_cases2)

CC_cases <- CC_cases %>%
  filter(!is.na(Date)) %>%
  # filter(Totals != 'Event count') %>%
  select(`Effective user ID`, `Event count`, Sessions, Date)

CC_cases <- rename(CC_cases, Cases.Sessions = Sessions) 
CC_cases <- rename(CC_cases, Cases.Date = Date) 
CC_cases$Cases.Sessions <- as.numeric(CC_cases$Cases.Sessions)
CC_cases$Cases.Date <- as.Date(as.character(CC_cases$Cases.Date), format='%Y%m%d')

#### appointment users ####
CC_appt1 <- read_csv("initial_data/cumulative_data/appt/appt_Nov1-Dec31-2023.csv", skip = 6)
CC_appt2 <- read_csv("initial_data/cumulative_data/appt/appt_Jan1-Feb29-2024.csv", skip = 6)
CC_appt3a <- read_csv("initial_data/cumulative_data/appt/appt_Mar1-Apr30-2024.csv", skip = 6)
CC_appt3b <- read_csv("initial_data/cumulative_data/appt/appt_May1-Jun30-2024.csv", skip = 6)
CC_appt3c <- read_csv("initial_data/cumulative_data/appt/appt_July1-Sept16-2024.csv", skip = 6)

CC_appt <- rbind(CC_appt1, CC_appt2, CC_appt3a, CC_appt3b, CC_appt3c)

CC_appt  <- CC_appt %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(`Effective user ID`, Sessions)   

# rename variables 
CC_appt <- rename(CC_appt, Appt.Sessions = Sessions) #, Namespace.ID = `Namespace ID`, Stream.name = `Stream name`, App.instance.ID = `App-instance ID`

CC_appt$`Effective user ID` <- as.character(CC_appt$`Effective user ID`)

#### add edit users ####
CC_edit1 <- read_csv("initial_data/cumulative_data/add_edit/add_edit_Nov1-Dec31-2023.csv", skip = 6)
CC_edit2 <- read_csv("initial_data/cumulative_data/add_edit/add_edit_Jan1-Sept16-2024.csv", skip = 6)

CC_edit <- rbind(CC_edit1, CC_edit2)
CC_edit <- rename(CC_edit, Edit.Sessions = Sessions)

CC_edit <- CC_edit %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(`Effective user ID`, Edit.Sessions) %>% 
  distinct()

#### goals users ####
CC_goals1 <- read_csv("initial_data/cumulative_data/goals/goals_Nov1-Dec31-2023.csv", skip = 6)
CC_goals2 <- read_csv("initial_data/cumulative_data/goals/goals_Jan1-Sept16-2024.csv", skip = 6)

CC_goals <- rbind(CC_goals1, CC_goals2)
CC_goals <- rename(CC_goals, Goal.Sessions = Sessions)

CC_goals <- CC_goals %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(`Effective user ID`, Goal.Sessions) %>% 
  distinct()

#### events users ####
CC_events1 <- read_csv("initial_data/cumulative_data/events/Events_Nov1-Dec31-2023.csv", skip = 6)
CC_events2 <- read_csv("initial_data/cumulative_data/events/Events_Jan1-Sept16-2024.csv", skip = 6)

CC_events <- rbind(CC_events1, CC_events2)

CC_events <- CC_events %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(`Effective user ID`, `Total users`) %>% 
  rename(Events.Sessions = `Total users`)

#### funnel explorations ####
# Get the list of files in the folder
f <- list.files("./initial_data/cumulative_data/funnel_exploration/", full.names = TRUE)

# Remove the directory path and file extension to use the file names as dataframe names
file_names <- tools::file_path_sans_ext(basename(f))

# Read each CSV and assign to a dataframe using the original file name
for (i in seq_along(f)) {
  assign(file_names[i], read_csv(f[i], skip = 7))
}

# home start 
funnel_start_home <- funnel_start_homeAUG18_Sept16 %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(6))

# rename variables 
funnel_start_home <- rename(funnel_start_home, Home.Start.Sessions = Sessions) #, Namespace.ID = `Namespace ID`, Stream.name = `Stream name`, App.instance.ID = `App-instance ID`

# home to student services 
`student_serv_from_home` <- `ss-from-home-aug18-sept16` %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(6))

# rename variables 
student_serv_from_home <- rename(student_serv_from_home, SS.from.home.Sessions = Sessions) #, Namespace.ID = `Namespace ID`, Stream.name = `Stream name`, App.instance.ID = `App-instance ID`

# home to classes 
`classes_from_home` <- `classes_from_home_Aug18-Sept16` %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(6))

# rename variables 
classes_from_home <- rename(classes_from_home, classes.from.home.Sessions = Sessions) #, Namespace.ID = `Namespace ID`, Stream.name = `Stream name`, App.instance.ID = `App-instance ID`

# home to advising
appt_from_home <- `appt-from-home-aug18-sept16` %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(6))

# rename variables 
appt_from_home <- rename(appt_from_home, appt.from.home.Sessions = Sessions) #, Namespace.ID = `Namespace ID`, Stream.name = `Stream name`, App.instance.ID = `App-instance ID`

# merge home to other outcomes
home_to_x <- funnel_start_home %>% 
  left_join(classes_from_home) %>% 
  left_join(student_serv_from_home) %>% 
  left_join(appt_from_home)

home_to_x <- home_to_x %>% 
  rename(SF_18_ID = `Effective user ID`) %>% 
  filter(SF_18_ID != "NO_USER_ID")


# student services start
student_services_start <- `ss-start-aug18-sept16` %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(6))

# rename variables 
student_services_start <- rename(student_services_start, student.services.start.Sessions = Sessions) #, Namespace.ID = `Namespace ID`, Stream.name = `Stream name`, App.instance.ID = `App-instance ID`

# student services to classes 
`classes_from_student_services` <- `classes-from-student-serv-aug18-sept16` %>% 
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(6))

# rename variables 
classes_from_student_services <- rename(classes_from_student_services, classes.from.student.services.Sessions = Sessions) #, Namespace.ID = `Namespace ID`, Stream.name = `Stream name`, App.instance.ID = `App-instance ID`

# merge student services to classes
st_serv_to_classes <- student_services_start %>% 
  left_join(classes_from_student_services)

st_serv_to_classes <- st_serv_to_classes %>% 
  rename(SF_18_ID = `Effective user ID`) %>% 
  filter(SF_18_ID != "NO_USER_ID")
# home_to_x and st_serve_to_classes works as places where we can merge

#### read in SF csv ####
all_users_SF <- read_csv("initial_data/cumulative_data/from_SF/Sept17_2024_Community_users.csv")

# change last login to a readable date format
all_users_SF$last_login2 <- as.Date(mdy_hm(all_users_SF$`Last Login`))

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

#### merge all students based on emails to CatSF ####
# OLD_users_SF <- read_csv("initial_data/cumulative_data/from_SF/old_student_enrollment.csv")
student_enrollment_from_sf <- read_csv("initial_data/cumulative_data/from_SF/student_enrollment_as_of_Sept17_2024.csv")

# # delete if email is blank
# student_enrollment_from_sf <- student_enrollment_from_sf[!(is.na(student_enrollment_from_sf$Email) | student_enrollment_from_sf$Email==""), ]

# left join to all sf-users
all_users_SF_enroll <- all_users_SF %>% 
  left_join(student_enrollment_from_sf)
# # merge student enrollment into all_users_SF
# all_users_SF <- all_users_SF %>% 
#   left_join(student_enrollment_from_sf)
#### subset sf_users by profile
unique(all_users_SF$Profile)

CC_all_test <- CC_all %>%
  mutate(
    SF_18_ID = ifelse(nchar(`Effective user ID`) == 18, `Effective user ID`, NA),
    SF_15_ID= ifelse(nchar(`Effective user ID`) == 15, `Effective user ID`, NA)
  )

CC_all_15_id <- CC_all_test %>%
  select(SF_15_ID, Date, `Event count`, Sessions, Views, `Engagement rate`, 
         User_engagement_in_seconds, Time_elapsed_in_seconds, Avg_session_in_seconds,
         `Elapsed time from last page request`
  ) %>% 
  filter(!is.na(SF_15_ID)) 

CC_all_18_id <- CC_all_test %>%
  select(SF_18_ID, Date, `Event count`, Sessions, Views, `Engagement rate`, 
         User_engagement_in_seconds, Time_elapsed_in_seconds, Avg_session_in_seconds,
         `Elapsed time from last page request`
         ) %>% 
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
         Cases.Sessions, Events.Sessions,      Cases.Date,
         User_engagement_in_seconds, Time_elapsed_in_seconds, Avg_session_in_seconds,
         Email, `First Name`, `Last Name`, `Last Login`, Profile
         # `EDS Primary Affiliation`, `EDS Affiliations`
         ) %>% 
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


# Function to assign semester based on the given date ranges
# get_semester <- function(date_range) {
#   # Check if date_range is NA or invalid
#   if (is.na(date_range)) {
#     return(NA)
#   }
#   
#   # Define the date ranges for Spring, Summer, and Fall 2024
#   fall2023_start <- as.Date("2023-08-15")
#   fall2023_end <- as.Date("2023-12-18")
#   winter2024_start <- as.Date("2023-12-19")
#   winter2024_end <- as.Date("2024-01-10")
#   spring2024_start <- as.Date("2024-01-11")
#   spring2024_end <- as.Date("2024-05-12")
#   summer2024_start <- as.Date("2024-05-13")
#   summer2024_end <- as.Date("2024-08-20")
#   fall2024_start <- as.Date("2024-08-21")
#   fall2024_end <- as.Date("2024-12-15")
#   winter2025_start <- as.Date("2024-12-16")
#   winter2025_end <- as.Date("2025-01-12")
#   
#   # Assign semester based on the date ranges
#   if (date_range >= fall2023_start & date_range <= fall2023_end) {
#     return("Fall 2023")
#   } else if (date_range >= winter2024_start & date_range <= winter2024_end) {
#     return("Winter 2024")
#   } else if (date_range >= spring2024_start & date_range <= spring2024_end) {
#     return("Spring 2024")
#   } else if (date_range >= summer2024_start & date_range <= summer2024_end) {
#     return("Summer 2024")
#   } else if (date_range >= fall2024_start & date_range <= fall2024_end) {
#     return("Fall 2024")
#   } else if (date_range >= winter2025_start & date_range <= winter2025_end) {
#     return("Winter 2025")
#   } else {
#     return(NA)  # If the date is outside the specified ranges
#   }
# }

#### Define semester ranges ####
# semester_ranges <- data.frame(
#   semester = c("Fall 2023", "Spring 2024", "Fall 2024"),
#   start_date = as.Date(c("2023-08-15", "2024-01-11", "2024-07-21")),
#   end_date = as.Date(c("2024-01-10", "2024-07-20", "2024-12-15"))
# )
# 
# # Function to determine semester
# get_semester <- function(date) {
#   if (is.na(date)) return(NA)
#   
#   semester <- semester_ranges %>%
#     filter(date >= start_date & date <= end_date) %>%
#     pull(semester)
#   
#   if (length(semester) == 0) return(NA)  # Return NA if no match found
#   
#   return(semester)
# }
# 
# # Apply the function to the dataframe
# Cat_SF <- Cat_SF %>%
#   mutate(semester = sapply(last_login2, get_semester))
# 
# # Check for any NA values in the last_login2 column
# invalid_dates <- Cat_SF[is.na(Cat_SF$last_login2), "last_login2"]
# if (length(invalid_dates) > 0) {
#   print("Invalid or unconvertible dates found:")
#   print(invalid_dates)
# }
get_semester <- function(date_range) {
  # Check if date_range is NA or invalid
  if (is.na(date_range)) {
    return(NA)
  }

  # Define the date ranges for Spring, Summer, and Fall 2024
  fall2023_start <- as.Date("2023-08-15")
  fall2023_end <- as.Date("2024-01-10")
  spring2024_start <- as.Date("2024-01-11")
  spring2024_end <- as.Date("2024-05-15") # trying a unique date range to deal with summer enrollment
  fall2024_start <- as.Date("2024-05-16")
  fall2024_end <- as.Date("2024-12-15")

  # Assign semester based on the date ranges
  if (date_range >= fall2023_start & date_range <= fall2023_end) {
    return("Fall 2023")
  } else if (date_range >= spring2024_start & date_range <= spring2024_end) {
    return("Spring 2024")
  } else if (date_range >= fall2024_start & date_range <= fall2024_end) {
    return("Fall 2024")
  } else {
    return(NA)  # If the date is outside the specified ranges
  }
}


# Check for any NA values in the last_login2 column
invalid_dates <- Cat_SF[is.na(Cat_SF$last_login2), "last_login2"]
if (length(invalid_dates) > 0) {
  print("Invalid or unconvertible dates found:")
  print(invalid_dates)
}

# Apply the function to create the semester column
Cat_SF$semester_CC <- sapply(Cat_SF$last_login2, get_semester)



#### join catcloud to student enrollment data in SF ####
# change to left_join to the student enrollment
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
Headcount_Fall_Winter_2023 <- read_csv("initial_data/headcounts_data/Headcount_Fall_Winter_2023_YN_enroll.csv")
Headcount_Spring2024 <- read_csv("initial_data/headcounts_data/Headcount_Spring2024.csv")
Headcount_Summer2024 <- read_csv("initial_data/headcounts_data/Headcount_Summer2024.csv")
Headcount_Fall2024 <- read_csv("initial_data/headcounts_data/Headcount_Fall2024_Sept17.csv")
Headcount_students <- rbind(Headcount_Fall_Winter_2023, Headcount_Spring2024, Headcount_Summer2024, Headcount_Fall2024) %>% 
  unique()

# # Assuming student_headcounts is your dataframe and Term is the column with term values
# Headcount_students <- Headcount_students  %>%
#   mutate(semester_headcounts = case_when(
#     Term %in% c("Fall 2023", "Winter 2023") ~ "Fall 2023",
#     Term %in% c("Spring 2024", "Summer 2024") ~ "Spring 2024",
#     TRUE ~ Term  # Keep the term as-is for any other values
#   ))

Headcount_students <- Headcount_students %>%
  mutate(semester_headcounts = case_when(
    trimws(Term) %in% c("Winter 2023") ~ "Fall 2023",
    trimws(Term) %in% c("Summer 2024") ~ "Spring 2024",
    TRUE ~ Term  # Keep the term as-is for any other values
  ))

# table(Headcount_students$Term)
# table(Headcount_students$semester_headcounts)
#### convert terms to Semester ####

#### refactor headcount academic level data ####
unique(Headcount_students$`Academic Level`)

Headcount_students$Class_standing_recode <- recode(Headcount_students$`Academic Level`,
                                                     "Freshman" = "Freshman", "Sophomore" = "Sophomore", 
                                                     "Junior" = "Junior", "Senior" = "Senior", 
                                                     "Graduate" = "Graduate", "Masters" = "Graduate",
                                                     "Professional Year 1" = "Graduate", "Professional Year 2" = "Graduate",
                                                     "Professional Year 3" = "Graduate", "Professional Year 4" = "Graduate", 
                                                     "Doctoral" = "Graduate", " " = "Other")

Headcount_students$Class_standing_recode <- factor(Headcount_students$Class_standing_recode, 
                                                     levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate", NA))

unique(Headcount_students$Class_standing_recode)

# rename academic programs
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

# count UA Email address #
Headcount_students %>%
  summarise(unique_count = n_distinct(`UA Email Address`)) # almost 90000

Headcount_students %>%
  summarise(unique_count = n_distinct(`Student ID`)) # almost 91531
# defining cat date filter
Cat_date_filter <- Cat_SF_enroll %>%
  filter(last_login2 > "2023-10-31 06:25:00 GMT")

Cat_date_filter <- Cat_date_filter %>%
  mutate(Goals = ifelse(!is.na(Goal), "G", ""),
         Appts = ifelse(!is.na(Appt), "A", ""),
         Cases = ifelse(!is.na(Case), "C", ""),
         Edits = ifelse(!is.na(Edit), "E", ""),
         Events = ifelse(!is.na(Event), "eV", ""),
         # News = ifelse(!is.na(News), "N", ""),
         Total = str_c(Goals, Cases, Appts, Edits, Events)) # omit | (!is.na(Goal1)

#### Funnel view ####
Cat_date_filter <- Cat_date_filter %>% 
  left_join(home_to_x) %>% 
  left_join(st_serv_to_classes)

# # Reshape the data to long format
# home_to_x_SF_long <- home_to_x_SF  %>%
#   select(SF_18_ID, Home.Start.Sessions, classes.from.home.Sessions, SS.from.home.Sessions, appt.from.home.Sessions) %>% 
#   pivot_longer(
#     cols = c(classes.from.home.Sessions, SS.from.home.Sessions, appt.from.home.Sessions),
#     names_to = "Target",
#     values_to = "Value"
#   )
# 
# # Add the Source and Path Order columns
# home_to_x_SF_long <- home_to_x_SF  %>%
#      select(SF_18_ID, Home.Start.Sessions, classes.from.home.Sessions, SS.from.home.Sessions, appt.from.home.Sessions, 
#             student.services.start.Sessions, classes.from.student.services.Sessions) %>% 
#      pivot_longer(
#          cols = c(classes.from.home.Sessions, SS.from.home.Sessions, appt.from.home.Sessions),
#          names_to = "Target",
#          values_to = "Value"
#        )
# 
# home_to_x_SF <- home_to_x_SF %>% 
#   full_join(st_serv_to_classes)
# 
# home_to_x_SF_long <- home_to_x_SF %>%
#   mutate(Source = "Home.Start.Sessions", "Home.Start.Sessions","Home.Start.Sessions", Stu) %>%   # Source is always Home
#   mutate(Path_Order = 0)        # Initial Path Order for the first set
# # Create the duplicate rows with Path Order 1
# home_to_x_SF_long_duplicated <- home_to_x_SF_long %>%
#   mutate(Path_Order = 1)
# 
# # Combine the original and duplicated data
# home_to_x_SF_long_final_data <- bind_rows(home_to_x_SF_long, home_to_x_SF_long_duplicated )
# 
# home_to_x_SF_long_final_data <- home_to_x_SF_long_final_data %>% 
#   rename(`App-instance ID` = SF_18_ID)
# 
# write_named_csv(home_to_x_SF_long_final_data)


#### donut chart ####
# Pivot the data
donut_data <- home_to_x %>% 
  rename(`App-instance ID` = SF_18_ID) %>% 
  select(-c(`Stream name`, `Namespace ID`, `Event count`)) %>% 
  pivot_longer(cols = -c(Home.Start.Sessions, `App-instance ID`), 
               names_to = "destination_page", 
               values_to = "count")

# Calculate percentages
donut_data  <- donut_data  %>%
  filter(!(is.na(count))) %>% 
  mutate(percentage = count / sum(count) * 100)

# Add a column for sorting (optional, but useful for Tableau)
donut_data  <- donut_data  %>%
  arrange(desc(count)) %>%
  mutate(sort_order = row_number())

# Write the processed data to a CSV file
write_named_csv(donut_data)

# donut_data <- Cat_date_filter %>% 
#   select(SF_18_ID, last_login2, Home.Start.Sessions, classes.from.home.Sessions, SS.from.home.Sessions, appt.from.home.Sessions) %>% 
#   pivot_longer(cols = c(classes.from.home.Sessions, SS.from.home.Sessions, appt.from.home.Sessions),
#                    names_to = "Destination",
#                    values_to = "Value")


# donut_data_final <- donut_data %>% 
#   rename(`App-instance ID` = SF_18_ID) 
# 
# write_named_csv(donut_data)

#### rename effective user id ####
# remove non-students 
Cat_date_filter_student <- Cat_date_filter %>% 
  select(SF_15_ID, SF_18_ID, `Student Enrollment Key`, Sessions, Date, Appt.Sessions, Edit.Sessions, Goal.Sessions, Cases.Sessions, Events.Sessions, 
         User_engagement_in_seconds, Time_elapsed_in_seconds, Avg_session_in_seconds, Email, `Full Name`, Cases.Date,
         `First Name`, `Last Name`, `Last Login`, last_login2, Appt, Case, Event, Goal, Edit, `Student Enrollment Name`,
         semester_CC, Campus, Career, NetID, `Class Standing`, `Primary College`, Emplid, Class_Standing_recode,
         Primary.College_recode, Campus.recode1, Campus.recode, Goals, Appts, Cases, Edits, Events, Total) %>%
  rename(`App-instance ID` = SF_18_ID) %>% 
  rename(`Student_ID_CC` = Emplid) %>% 
  filter(!(is.na(`Student Enrollment Name`)))

#### primary college table ####
Academic_program_table_Fall2023 <- Headcount_students %>% 
  filter(semester_headcounts == "Fall 2023") %>% 
  count(Primary_College_recode, name = "All_students_program") %>% 
  left_join(Cat_date_filter_student %>%   
              filter(semester_CC == "Fall 2023") %>%
              select(`App-instance ID`, Primary.College_recode) %>% 
              distinct() %>% 
              rename(Primary_College_recode = Primary.College_recode)    %>% 
              count(Primary_College_recode, name = "N_users_program")) %>% 
  mutate(proportion = N_users_program/All_students_program)

Academic_program_table_Fall2023$semester <- "Fall 2023"
write_named_csv(Academic_program_table_Fall2023)


Academic_program_table_Spring2024 <- Headcount_students %>% 
  # select(semester_headcounts, Primary_College_recode, `Student ID`) %>% 
  filter(semester_headcounts == "Spring 2024") %>% 
  count(Primary_College_recode, name = "All_students_program") %>% 
  left_join(Cat_date_filter_student %>%   
              filter(semester_CC == "Spring 2024") %>%
              select(`App-instance ID`, Primary.College_recode) %>% 
              distinct() %>% 
              rename(Primary_College_recode = Primary.College_recode)   %>% 
              count(Primary_College_recode, name = "N_users_program")) %>% 
  mutate(proportion = N_users_program/All_students_program)
Academic_program_table_Spring2024$semester <- "Spring 2024"

write_named_csv(Academic_program_table_Spring2024)

Academic_program_table_Fall2024 <- Headcount_students %>% 
  filter(semester_headcounts == "Fall 2024") %>% 
  count(Primary_College_recode, name = "All_students_program") %>% 
  left_join(Cat_date_filter_student %>%   
              filter(semester_CC == "Fall 2024") %>%
              select(`App-instance ID`, Primary.College_recode) %>% 
              distinct() %>% 
              rename(Primary_College_recode = Primary.College_recode)    %>% 
              count(Primary_College_recode, name = "N_users_program")) %>% 
  mutate(proportion = N_users_program/All_students_program)
Academic_program_table_Fall2024$semester <- "Fall 2024"

write_named_csv(Academic_program_table_Fall2024)

Academic_program_table <- rbind(Academic_program_table_Fall2023, Academic_program_table_Spring2024, Academic_program_table_Fall2024)

write_named_csv(Academic_program_table)
#### campus tables ####
Campus_table_fall2023 <- Headcount_students %>%
  # filter(semester_headcounts == "Fall 2023") %>% 
  filter(Term == "Fall 2023") %>% 
  count(Campus_recode, name = "All_students_campus")%>% 
  left_join(Cat_date_filter_student %>%
              filter(semester_CC == "Fall 2023") %>%
              select(`App-instance ID`, Campus.recode) %>%
              distinct() %>%
              rename(Campus_recode = Campus.recode)    %>%
              count(Campus_recode, name = "N_users_campus")) %>%
  mutate(proportion = N_users_campus/All_students_campus)

Campus_table1_Fall2023 <- Headcount_students %>%
  # select(semester_headcounts, Primary_College_recode, Campus_recode1, `Student ID`) %>%
  filter(Term == "Fall 2023") %>%
  count(Campus_recode1, name = "All_students_campus")%>%
  left_join(Cat_date_filter_student %>%
              filter(semester_CC == "Fall 2023") %>%
              select(`App-instance ID`, Campus.recode1) %>%
              distinct() %>%
              rename(Campus_recode1 = Campus.recode1)    %>%
              count(Campus_recode1, name = "N_users_campus")) %>%
  mutate(proportion1 = N_users_campus/All_students_campus)

Campus_table1_Fall2023$semester <- "Fall 2023"

write_named_csv(Campus_table1)#
write_named_csv(Campus_table1_Fall2023)

Campus_table_Spring2024 <- Headcount_students %>%
  filter(semester_headcounts == "Spring 2024") %>% 
  #filter(Term == "Spring 2024") %>% 
  count(Campus_recode, name = "All_students_campus")%>% 
  left_join(Cat_date_filter_student %>%
              filter(semester_CC == "Spring 2024") %>%
              select(`App-instance ID`, Campus.recode) %>%
              distinct() %>%
              rename(Campus_recode = Campus.recode)    %>%
              count(Campus_recode, name = "N_users_campus")) %>%
  mutate(proportion = N_users_campus/All_students_campus)

write_named_csv(Campus_table_Spring2024)

Campus_table1_Spring2024 <- Headcount_students %>%
  # select(semester_headcounts, Primary_College_recode, Campus_recode1, `Student ID`) %>%
  filter(Term == "Spring 2024") %>%
  count(Campus_recode1, name = "All_students_campus")%>%
  left_join(Cat_date_filter_student %>%
              filter(semester_CC == "Spring 2024") %>%
              select(`App-instance ID`, Campus.recode1) %>%
              distinct() %>%
              rename(Campus_recode1 = Campus.recode1)    %>%
              count(Campus_recode1, name = "N_users_campus")) %>%
  mutate(proportion1 = N_users_campus/All_students_campus)

Campus_table1_Spring2024$semester <- "Spring 2024"
write_named_csv(Campus_table1_Spring2024)#

# Campus_table_fall2024 <- Headcount_students %>%
#   filter(Term == "Fall 2024") %>% 
#   count(Campus_recode, name = "All_students_campus")%>% 
#   left_join(Cat_date_filter_student %>%
#               filter(semester_CC == "Fall 2024") %>%
#               select(`App-instance ID`, Campus.recode) %>%
#               distinct() %>%
#               rename(Campus.recode = Campus.recode)    %>%
#               count(Campus_recode, name = "N_users_campus")) %>%
#   mutate(proportion = N_users_campus/All_students_campus)
# 
# write_named_csv(Campus_table_fall2024)

# 
Campus_table1_Fall2024 <- Headcount_students %>%
  # select(semester_headcounts, Primary_College_recode, Campus_recode1, `Student ID`) %>%
  filter(Term == "Fall 2024") %>%
  count(Campus_recode1, name = "All_students_campus")%>%
  left_join(Cat_date_filter_student %>%
              filter(semester_CC == "Fall 2024") %>%
              select(`App-instance ID`, Campus.recode1) %>%
              distinct() %>%
              rename(Campus_recode1 = Campus.recode1)    %>%
              count(Campus_recode1, name = "N_users_campus")) %>%
  mutate(proportion1 = N_users_campus/All_students_campus)
Campus_table1_Fall2024$semester <- "Fall 2024"
write_named_csv(Campus_table1_Fall2024)#

Campus_table1 <- rbind(Campus_table1_Fall2023, Campus_table1_Spring2024, Campus_table1_Fall2024)
write_named_csv(Campus_table1)
#### detailed class tables ####
# detailed_class_standing_table <-  Headcount_Fall2024 %>%
#   count(Class_standing_recode, name = "All_students_class_standing")%>% 
#   left_join(Cat_date_filter %>%
#               select(SF_18_ID, Class_Standing_recode) %>%
#               distinct() %>%
#               rename(Class_standing_recode = Class_Standing_recode) %>% 
#               count(Class_standing_recode, name = "N_users")) %>%
#   mutate(proportion = N_users/All_students_class_standing)
# 
# write_named_csv(detailed_class_standing_table)

detailed_class_standing_table_Fall2023 <-  Headcount_students %>%
  filter(Term == "Fall 2023") %>% 
  count(Class_standing_recode, name = "All_students_class_standing")%>% 
  left_join(Cat_date_filter_student %>%
              filter(semester_CC == "Fall 2023") %>% 
              select(`App-instance ID`, Class_Standing_recode) %>%
              distinct() %>%
              rename(Class_standing_recode = Class_Standing_recode) %>% 
              count(Class_standing_recode, name = "N_users")) %>%
  mutate(proportion = N_users/All_students_class_standing)
detailed_class_standing_table_Fall2023$semester <- "Fall 2023"
write_named_csv(detailed_class_standing_table_Fall2023)


detailed_class_standing_table_Spring2024 <-  Headcount_students %>%
  filter(Term == "Spring 2024") %>% 
  count(Class_standing_recode, name = "All_students_class_standing")%>% 
  left_join(Cat_date_filter_student %>%
              filter(semester_CC == "Spring 2024") %>% 
              select(`App-instance ID`, Class_Standing_recode) %>%
              distinct() %>%
              rename(Class_standing_recode = Class_Standing_recode) %>% 
              count(Class_standing_recode, name = "N_users")) %>%
  mutate(proportion = N_users/All_students_class_standing)
detailed_class_standing_table_Spring2024$semester <- "Spring 2024"

write_named_csv(detailed_class_standing_table_Spring2024)

detailed_class_standing_table_Fall2024 <-  Headcount_students %>%
  filter(Term == "Fall 2024") %>% 
  count(Class_standing_recode, name = "All_students_class_standing")%>% 
  left_join(Cat_date_filter_student %>%
              filter(semester_CC == "Fall 2024") %>% 
              select(`App-instance ID`, Class_Standing_recode) %>%
              distinct() %>%
              rename(Class_standing_recode = Class_Standing_recode) %>% 
              count(Class_standing_recode, name = "N_users")) %>%
  mutate(proportion = N_users/All_students_class_standing)
detailed_class_standing_table_Fall2024$semester <- "Fall 2024"

write_named_csv(detailed_class_standing_table_Fall2024)

detailed_class_standing_table <- rbind(detailed_class_standing_table_Fall2023, detailed_class_standing_table_Spring2024, detailed_class_standing_table_Fall2024)

write_named_csv(detailed_class_standing_table)

# quick checks
Cat_date_filter_student %>%
  summarise(unique_count = n_distinct(`Student Enrollment Name`)) # almost 59000

Cat_date_filter_student %>%
  summarise(unique_count = n_distinct(`Email`)) # almost 52000 but 43906 in -student

Cat_date_filter_student %>%
  summarise(unique_count = n_distinct(Student_ID_CC)) # almost 52000 but 43906 in -student
# Anti join to find people in list1 but not in list2
unmatched_list <- anti_join(Cat_date_filter_student, Headcount_students, by = c("Email" = "UA Email Address"))

unmatched_list %>%
  summarise(unique_count = n_distinct(`Email`)) # almost 181
#delete if email is blank
Cat_date_filter2_student <- Cat_date_filter_student[!(is.na(Cat_date_filter_student$Email) | Cat_date_filter_student$Email==""), ]

Headcount_students %>% 
  group_by(Term) %>% 
  summarise(unique_count = n_distinct(`Student ID`))

Headcount_students %>% 
  group_by(semester_headcounts) %>% 
  summarise(unique_count = n_distinct(`Student ID`))

Headcount_students %>% 
  group_by(Campus_recode1) %>% 
  summarise(unique_count = n_distinct(`Student ID`))

Headcount_students %>% 
  group_by(Class_standing_recode) %>% 
  summarise(unique_count = n_distinct(`Student ID`))

Cat_date_filter %>% 
  group_by(Class_Standing_recode) %>% 
  summarise(unique_count = n_distinct(`Email`))

Headcount_students %>% 
  filter(semester_headcounts == "Spring 2024") %>% 
  group_by(Campus_recode) %>% 
  summarise(unique_count = n_distinct(`Student ID`))

Cat_date_filter_student %>% 
  group_by(semester_CC) %>% 
  summarise(unique_count = n_distinct(`Email`))

#### write Cat date filter to file ####
# Cat_date_filter_student <- as.data.frame(Cat_date_filter)
write_named_csv(Cat_date_filter_student)

write_named_csv(Headcount_students)

#### tableau help ####

CC_students <- Cat_date_filter_student %>% 
  select(-c(`Full Name`, `First Name`, `Last Name`, NetID, Email))

headcount <- Headcount_students %>% 
  select(-c(`UA Email Address`, `Preferred Name`, `First Name`, `Last Name`, `Preferred First Name`, `Preferred Last Name`
            ))

write_named_csv(CC_students)
write_named_csv(headcount)
