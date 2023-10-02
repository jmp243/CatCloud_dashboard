# read in CatCloud data
# Jung Mee Park
# last run 2023-10-2
# new column for campus code

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
# library(paws)
# # library(readr)
# s3 <- paws::s3()
# s3$put_object(Bucket = "trellis-dsq", )


#### Write csv #### 
write_named_csv <- function(x) 
  write_csv(x, file = paste0(
    "~/Documents/Trellis/CatCloud_dashboard/clean_data/clean_fall2023/",
    deparse(substitute(x)),".csv"))

#### read in data ####
getwd()

# read in more data
folder <- "/Users/jungmeepark/Documents/Trellis/CatCloud_dashboard/initial_data/Fall2023"      # path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder

# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(folder,"/", file_list[i], sep=''))
  )}

# read in ad hod data
# preliminary data cleaning was done in excel

library(data.table)
ad_hoc_2023 <- read_csv("initial_data/Fall2023/ad_hoc_Aug_2023.csv")
ad_hoc_2023 <- ad_hoc_2023[-1:-2,]
write_named_csv(ad_hoc_2023)

# table with no totals
ad_hoc_2023_no_totals <- ad_hoc_2023[,-18:-19]
write_named_csv(ad_hoc_2023_no_totals)

library(janitor)
library(magrittr)
library(readxl)

ad_hoc_no_totals <- read_csv("clean_data/clean_fall2023/ad_hoc_2023_no_totals.csv")

ad_hoc_date_table <-ad_hoc_no_totals %>% 
  pivot_longer(
    cols = !`Page title-Date`, 
    names_to = "Page title", 
    values_to = "count"
  )


unique(ad_hoc_date_table$`Page title`)
ad_hoc_date_table$`Page title` <- recode(ad_hoc_date_table$`Page title`,
                                         "Services Calendar | CatCloud-Event count" = "Services Calendar-Event count", 
                                         "Services Calendar | CatCloud-Sessions" = "Services Calendar-Sessions", 
                                         "Home | CatCloud-Event count" = "Home-Event count", 
                                         "Home | CatCloud-Sessions" = "Home-Sessions", 
                                         "Hub Grades | The University of Arizona, Tucson, Arizona-Event count" = "Hub Grades-Event count", 
                                         "Hub Grades | The University of Arizona, Tucson, Arizona-Sessions" = "Hub Grades-Sessions",
                                         "Student Services | CatCloud-Event count" = "Student Services-Event count", 
                                         "Student Services | CatCloud-Sessions"  = "Student Services-Sessions",
                                         "My Calendar | CatCloud-Event count" = "Calendar-Event count", 
                                         "My Calendar | CatCloud-Sessions" = "Calendar-Sessions", 
                                         "Classes | CatCloud-Event count" = "Classes-Event count",
                                         "Classes | CatCloud-Sessions" = "Classes-Sessions",
                                         "Services Search | CatCloud-Event count"  = "Services Search-Event count", 
                                         "Services Search | CatCloud-Sessions" = "Services Search-Sessions", 
                                         "My Cases | CatCloud-Event count" = "Cases-Event count", 
                                         "My Cases | CatCloud-Sessions" = "Cases-Sessions")

# ad_hoc_transposed <- as.data.frame(t(ad_hoc_2023_no_totals))

write_named_csv(ad_hoc_date_table)
# (ad_hoc_meta <- readxl_example("initial_data/Fall2023/ad_hoc_fall2023_original.csv") %>% 
#     read_csv(sheet = 2, , skip = 6,n_max = 1))

# ad_hoc_data <- rbind(ad_hoc_data, row3 = apply(ad_hoc_data, 2, paste0, collapse = "-"))
# ad_hoc_data %>%
#   group_by(Segment) %>%
#   summarize(uniquevar = ifelse(all(),
#                                paste(dfile, Segment, sep = "::", collapse = " | "),
#                                ""),
#             dfile = paste(dfile, collapse = " | "),
#             dup = flag[1]) %>%
#   select(everything())

# ad_hoc_data["`Page title`", ] <- ad_hoc_data["`Page title`", ] + ad_hoc_data["Date", ]
# aggregate(ad_hoc_data, list(Group=replace(rownames(ad_hoc_data),rownames(ad_hoc_data) %in% c("`Page title`","Date"), "A&C")), mean)
# library(kableExtra)
# kbl_table <- ad_hoc_data %>% 
#   kbl(caption = "Name of the table") %>% 
#   collapse_rows(column = 1)

# group_and_concat <- ad_hoc_data %>%
#   group_by(Segment) %>%
#   summarise(all_names = paste(name, collapse = " | "))

# print(head(ad_hoc_data))
# ad_hoc_data.merged <- ad_hoc_data %>%
#   dplyr::group_by(Segment) %>%
#   dplyr::summarise(Col2 = paste(Col2, collapse = ","))
# print(head(dat.merged))

# early_fall2023 <- read_csv("initial_data/Fall2023/ad_hoc_2023.csv", skip = 7)
# # 
#   
# 
# names(early_fall_num)
# 
# 
# early_fall2023 %>% 
#   select(-c())  


#### read in excel file ####
Headcount_students <- read_csv("initial_data/Fall2023/Headcount Details(1)_unique_rows.csv")
# View(Headcount_Details)

# merge all CC users
CC_all <- read_csv("initial_data/Fall2023/CC_all_Aug20-Aug24_2023.csv", skip=6)
CC_all <- CC_all %>%
  filter(!is.na(`App-instance ID`)) %>%
  select(-c(9:10))

# appointment users
CC_appt1 <- read_csv("initial_data/Fall2023/appt_users_Aug24_2023.csv", skip = 6)
CC_appt1  <- CC_appt1 %>% 
  filter(!is.na(`App-instance ID`)) %>% 
  select(-c(7:8))  

CC_appt1 <- rename(CC_appt1, Appt.Sessions = Sessions) #, Namespace.ID = `Namespace ID`, Stream.name = `Stream name`, App.instance.ID = `App-instance ID`
CC_appt1 <- CC_appt1 %>% 
  select(`App-instance ID`, Appt.Sessions)

CC_appt1$`App-instance ID` <- as.character(CC_appt1$`App-instance ID`)
# add and edit users
CC_edit <- read_csv("initial_data/Fall2023/add_edit_users_Aug24_2023.csv", skip = 6)
CC_edit <- CC_edit %>% 
  filter(!is.na(`App-instance ID`)) %>% 
  select(-c(7:8))  

CC_edit <- rename(CC_edit, Edit.Sessions = Sessions)
CC_edit <- CC_edit %>% 
  select(`App-instance ID`, Edit.Sessions)

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
CC_goals <- read_csv("initial_data/Fall2023/goals_users_Aug24_2023.csv", skip = 6)
CC_goals <- CC_goals %>% 
  filter(!is.na(`App-instance ID`)) %>% 
  select(-c(7:8)) 

CC_goals <- rename(CC_goals, Goal.Sessions = Sessions)
CC_goals <- CC_goals %>% 
  select(`App-instance ID`, Goal.Sessions)


# add edits, goals, and appts to all CC users
CC_all <- left_join(CC_all, CC_appt1) %>% 
  left_join(CC_edit) %>% 
  left_join(CC_goals) 
  # left_join(CC_cases)
Cat_UserID <- CC_all
# Cat_UserID <- CC_all %>% 
#   filter(`Namespace ID` == "USER_ID") # keep only the cases we can track

#### subset sf_users by profile
# go to salesforce for this data
sf_users_IDs_emails_Fall2023 <- all_users_SF.csv %>% 
  filter(Profile == "Employee Community User" | Profile == "All Student Community User" )

#### merge sfcontact data with google
Cat_SF <- left_join(Cat_UserID, sf_users_IDs_emails_Fall2023, by = c("App-instance ID" = "User.ID")) 


Cat_SF <- Cat_SF %>% 
  select(`App-instance ID`, Sessions, Date,
         Appt.Sessions, Edit.Sessions, Goal.Sessions, 
         # Cases.Sessions,
         First.Name, Last.Name,
         Last.Login, Email) %>% 
  distinct()

#### merge all current students based on emails to CatSF ####

#### merge in new sf student data ####
student_enrollment_from_sf <- read_csv("initial_data/Fall2023/all_student_enrollment_Aug2023.csv")

# delete if email is blank
student_enrollment_from_sf <- student_enrollment_from_sf[!(is.na(student_enrollment_from_sf$Email) | student_enrollment_from_sf$Email==""), ]
# 

Cat_SF_enroll <- left_join(Cat_SF, student_enrollment_from_sf) %>% 
  distinct()# no need to add , by = c("Email"="Email")
# does not work with by = c("App-instance ID" = "Contact ID"); it should be userid

Cat_SF_enroll %>% 
  select(`App-instance ID`, `Primary College`) %>%
  distinct() %>%
  group_by(`Primary College`) %>% 
  count()

# Cat_SF_enroll <- left_join(Cat_SF_enroll, classApp_db)
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

#### headcounts with student info ####
unique(`Headcount Details(1)_unique_rows.csv`$Academic.Level)

`Headcount Details(1)_unique_rows.csv`$Academic.Level_recode <- recode(`Headcount Details(1)_unique_rows.csv`$Academic.Level,
                                                                   "Freshman" = "Freshman", "Sophomore" = "Sophomore", 
                                                                   "Junior" = "Junior", "Senior" = "Senior", 
                                                                   "Graduate" = "Graduate", "Masters" = "Graduate",
                                                                   "Professional Year 1" = "Graduate", "Professional Year 2" = "Graduate",
                                                                   "Professional Year 3" = "Graduate", "Professional Year 4" = "Graduate", 
                                                                   "Doctoral" = "Graduate", "NA" = "Other")

`Headcount Details(1)_unique_rows.csv`$Academic.Level_recode <- factor(`Headcount Details(1)_unique_rows.csv`$Academic.Level_recode, 
                                                                   levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate", "Other"))

unique(`Headcount Details(1)_unique_rows.csv`$Academic.Level_recode)
# df$new_variable <- recode(df$old_variable,  "Old Name" = "New Name", "Old Name2" = "New Name")

# create a counter if there is a appt session
Cat_SF_enroll <- Cat_SF_enroll %>%
  dplyr::mutate(Appt = ifelse(Appt.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Edit = ifelse(Edit.Sessions > 0, 1, 0)) %>% 
  # dplyr::mutate(Case = ifelse(Cases.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Goal = ifelse(Goal.Sessions > 0, 1, 0))

#### find number of categories in given groups ####
# # without student info
# 
# Headcount_Details%>%
#   group_by(`Academic Program`) %>%
#   count(wt= `Spring 2023`)

`Headcount Details(1)_unique_rows.csv` %>% 
  group_by(Academic.Program) %>% 
  count

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
Cat_SF_enroll$last_login2 <- as.Date(as.character(Cat_SF_enroll$Date), format='%Y%m%d')
#### new dataset with filtered dates ####
Cat_date_filter <- Cat_SF_enroll %>% 
  filter(last_login2 > "2023-08-08 06:25:00 GMT")

Cat_date_filter  %>%
  select(`App-instance ID`, Email, Primary.College_recode) %>%
  distinct() %>%
  group_by(Primary.College_recode) %>%
  count()

# unique(Headcount_Details.csv$Academic.Level)
unique(`Headcount Details(1)_unique_rows.csv`$Academic.Level_recode)

`Headcount Details(1)_unique_rows.csv` %>% 
  group_by(Academic.Level_recode) %>% 
  count() %>% 
  ungroup()

Cat_SF_enroll %>% 
  select(`App-instance ID`, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  count() %>% 
  ungroup()


### appt sessions ###
Cat_class_users_count <-  Cat_date_filter  %>%  
  select(`App-instance ID`, Email, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  count(Class_Standing_recode, name = "total_class_users")


Cat_date_filter <- Cat_date_filter %>%
  mutate(Goals = ifelse(!is.na(Goal), "G", ""), 
         Appts = ifelse(!is.na(Appt), "A", ""),
         # Cases = ifelse(!is.na(Case), "C", ""),
         Edits = ifelse(!is.na(Edit), "E", ""),
         Total = str_c(Goals, Appts, Edits)) # omit | (!is.na(Goal1) from line 348

         # Total = str_c(Goals, Appts, Cases, Edits)) # omit | (!is.na(Goal1) from line 348

Cat_date_filter %>% 
  select(`App-instance ID`, Sessions, Total) %>% 
  distinct() %>% 
  group_by(Total) %>%
  count(Total, name = "App_users")


Cat_date_filter %>% 
  select(`App-instance ID`, Email) %>% 
  distinct() %>% 
  count()

# Cat_date_filter %>% 
#   filter(Total == "E") 
# cat_sf_full  %>% 
#   select(App.instance.ID, Class_Standing_recode, Appt.Sessions) %>% 
#   distinct() %>% 
#   group_by(Appt.Sessions) %>% 
#   count(Class_Standing_recode, name = "N_users") %>% 
#   left_join(Cat_class_users_count) %>% 
#   mutate(proportion = N_users/total_class_users) 
# count distinct emails
Cat_date_filter %>%   
  select(`App-instance ID`, Campus, Email) %>% 
  distinct() %>% 
  count() # 33781 as of Jan 23



#### recode Academic.Program ####
# unique(Headcount_Details$`Academic Program`)
# 
# Headcount_Details <- Headcount_Details %>%
#   mutate(Academic.Program_recode = factor(`Academic Program`) %>%
#            fct_recode(
#              "Rogers College of Law" = "James E. Rogers College of Law",
#              "Rogers College of Law" = "Law Doctoral",
#              "Rogers College of Law" = "Law Masters",
#              "Rogers College of Law" = "Law Non-Degree Seeking",
#              "Graduate Coursework" = "Graduate Certificate",
#              "Graduate Coursework" = "Graduate Degree Seeking",
#              "Graduate Coursework" = "Graduate Non-Degree Seeking",
#              "Undergraduate Coursework" = "Undergrad Non-Degree Seeking",
#              "Undergraduate Coursework" = "Undergraduate Certificate"))
# 
# Cat_SF_enroll %>% 
#   select(`App-instance ID`, Primary.College_recode) %>%
#   distinct() %>%
#   group_by(Primary.College_recode) %>%
#   count() %>% 
#   ungroup()

# Academic_program_table <- Headcount_Details %>% 
#   count(Academic.Program_recode, wt = `Spring 2023`, name = "All_students_program") %>% 
#   left_join(Cat_SF_enroll %>%   
#               filter(last_login2 > "2023-01-01 16:25:00 GMT") %>% 
#               select(`App-instance ID`, Primary.College_recode) %>% 
#               distinct() %>% 
#               rename(Academic.Program_recode = Primary.College_recode)    %>% 
#               count(Academic.Program_recode, name = "N_users_program")) %>% 
#   mutate(proportion = N_users_program/All_students_program)
# ###

unique(`Headcount Details(1)_unique_rows.csv`$Academic.Program)

Headcount_students <- `Headcount Details(1)_unique_rows.csv` %>%
  mutate(Academic.Program_recode = factor(Academic.Program) %>%
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


Academic_program_table <- Headcount_students %>% 
  count(Academic.Program_recode, name = "All_students_program") %>% 
  left_join(Cat_SF_enroll %>%   
              filter(last_login2 > "2023-08-08 16:25:00 GMT") %>% 
              select(`App-instance ID`, Primary.College_recode) %>% 
              distinct() %>% 
              rename(Academic.Program_recode = Primary.College_recode)    %>% 
              count(Academic.Program_recode, name = "N_users_program")) %>% 
  mutate(proportion = N_users_program/All_students_program)

write_named_csv(Academic_program_table)

########### fix the program campus ####
# Headcount_Details %>% 
#   group_by(`Program Campus`) %>% 
#   count(wt= `Spring 2023`)

Cat_date_filter %>%   
  select(`App-instance ID`, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  count()

#### recode Arizona International and Direct ####
# unique(Headcount_Details$`Program Campus`)
# 
# Headcount_Details$Program.Campus_recode1 <- recode(Headcount_Details$`Program Campus`,
#                                                    "Arizona International" = "AZ International & Direct",
#                                                    "Arizona International Direct" = "AZ International & Direct",
#                                                    "Arizona Online" = "Arizona Online", 
#                                                    "Community Campus" = "Community Campus", 
#                                                    "Distance" = "Distance", "Phoenix"="Phoenix",
#                                                    "Southern Arizona" = "Southern Arizona", 
#                                                    "University of Arizona - Main" = "University of Arizona - Main")
# 
# Headcount_Details$Program.Campus_recode <- recode(Headcount_Details$`Program Campus`, "Arizona International" = "Other",
#                                                   "Arizona International Direct" = "Other",
#                                                   "Arizona Online" = "Arizona Online", 
#                                                   "Community Campus" = "Other", 
#                                                   "Distance" = "Other", "Phoenix"="Other",
#                                                   "Southern Arizona" = "Other", 
#                                                   "University of Arizona - Main" = "University of Arizona - Main", "NA" = "Other")

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

##### replace dates 
# Cat_date_filter <- Cat_date_filter %>% 
#   dplyr::mutate(New_time = case_when(!is.na(last_login1) ~ last_login1,
#                                   TRUE ~ last_login2)) %>% 
#   filter(New_time > "2022-08-14 16:25:00 GMT")
# mutate(New_time=ifelse(last_login2!=NA, "last_login1", last_login2))

Cat_date_filter %>% 
  select(`App-instance ID`) %>%
  distinct() %>% 
  count()


Cat_date_filter %>%   
  select(`App-instance ID`, Class_Standing_recode) %>% 
  distinct() %>% 
  # group_by(Class_standing_recode) %>% 
  count()
# 
# Cat_date_filter2 <- Cat_date_filter %>%
#   select(-First.Name, -Last.Name, -Last.Login, -`Subplan: CPP Info Name`,
#          -`Checkout Status`, -`Contact ID`)
# 
# write_named_csv(Cat_date_filter2)

Cat_SF_enroll %>%   
  filter(last_login2 > "2023-08-08 06:25:00 GMT") %>% 
  select(`App-instance ID`, last_login2) %>% 
  distinct() %>% 
  group_by(last_login2) %>% 
  count()


# skimr::skim()
# select(-last_logged_in, -last_login1, -last_login2)

# Cat_date_filter <- Cat_date_filter %>% 
#   select(-Segment)


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

#######################################

# left_join the to create proportions using headcount_students
# Headcount_students %>% 
#   group_by(Academic.Level) %>% 
#   count()

# Cat_SF_users_count2 <- Cat_date_filter  %>% 
#   select(`App-instance ID`, Class_Standing_recode) %>% 
#   distinct() %>% 
#   group_by(Class_Standing_recode) %>% 
#   rename(Academic.Level_recode = Class_Standing_recode) %>% 
#   count(Academic.Level_recode, name = "N_users")
# 
# detailed_class_standing_table2 <- Headcount_students %>% 
#   count(Academic.Level_recode, name = "All_students") %>% 
#   left_join(Cat_SF_users_count2) %>% 
#   mutate(proportion = N_users/All_students)



### headcount_students
summary(Headcount_students$Academic.Level_recode)
Headcount_students$Academic.Level_recode <- recode(Headcount_students$Academic.Level,"Freshman" = "Freshman", "Sophomore" = "Sophomore",
                                                   "Junior" = "Junior", "Senior" = "Senior",
                                                   "Graduate" = "Graduate", "Masters" = "Graduate",
                                                   "Professional Year 1" = "Graduate", "Professional Year 2" = "Graduate",
                                                   "Professional Year 3" = "Graduate", "Professional Year 4" = "Graduate",
                                                   "Doctoral" = "Graduate", "NA" = "Other")

Headcount_students$Academic.Level_recode  <- factor(Headcount_students$Academic.Level_recode,
                                                    levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate", "Other"))
Headcount_students %>% 
  group_by(Academic.Level_recode) %>% 
  count()


# rename the n columns
Cat_SF_users_count <- Cat_date_filter  %>% 
  select(`App-instance ID`, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  rename(Academic.Level_recode = Class_Standing_recode) %>% 
  count(Academic.Level_recode, name = "N_users")

detailed_class_standing_table <- Headcount_students %>%
  count(Academic.Level_recode,name = "All_students") %>% 
  left_join(Cat_SF_users_count) %>%
  mutate(proportion = N_users/All_students)

write_named_csv(detailed_class_standing_table)

Cat_headcount <- full_join(Headcount_students, Cat_date_filter, by= c("UA.Email.Address" = "Email"))
# Cat_headcount <- Cat_headcount %>% 
#   select(`Plan: CPP Info Name`, -`Subplan: CPP Info Name`, 
#          -`Student Enrollment Name`, -Term, -Academic.Plan.Code...Description, 
#          -Academic.Sub.Plan.Code...Description, -Career.x,  -`Checkout Status`, 
#          -Academic.Program.Location, -Pell.Recipient.Flag, -First.Generation.Flag, -Date) %>% 
#   distinct()

names(Cat_headcount)

Cat_headcount$Class_Standing_recode <- recode(Cat_headcount$`Class Standing`,"Freshman" = "Freshman", 
                                              "Sophomore" = "Sophomore", 
                                              "Junior" = "Junior", "Senior" = "Senior", 
                                              "Graduate" = "Graduate", "Masters" = "Graduate",
                                              "Prof 1" = "Graduate", "Prof 2" = "Graduate",
                                              "Prof 3" = "Graduate", "Prof 4" = "Graduate", 
                                              "Doctoral" = "Graduate", "NA" = "Other")

# Cat_headcount <- Cat_headcount %>% 
#   arrange(Sessions)
Cat_headcount <- Cat_headcount %>% 
  select(-First.Generation.Flag, -First.Name, -Last.Name)
write_named_csv(Cat_headcount)

# Cat_headcount %>% 
#   group_by(Class_Standing_recode) %>% 
#   count() %>% 
#   distinct()
# 
# Cat_headcount %>% 
#   group_by(Academic.Program_recode, Student.ID) %>% 
#   count() %>% 
#   distinct()

Cat_headcount2 <- Cat_headcount %>% 
  select(Student.ID, `App-instance ID`, Department, Academic.Level, Academic.Level_recode,
         Academic.Program, Academic.Program_recode, Program.Campus_recode, Program.Campus_recode1, 
         # Appt.Sessions, Edit.Sessions, Goal.Sessions, Cases.Sessions,
         Appt, Edit, Case, Goal, last_login2,
         Class_Standing_recode, Primary.College_recode, Campus.recode, Campus.recode1) %>% 
  distinct()


# Headcount_students %>% 
#   group_by(Academic.Level_recode, Student.ID) %>% 
#   count()
# 

n_distinct(Headcount_students$Student.ID)

write_named_csv(Headcount_students)

###################################### END for TABLEAU Analysis #########################
