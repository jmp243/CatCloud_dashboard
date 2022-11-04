# read in CatCloud data
# Jung Mee Park
# 2022-23-09
# 2022-17-10 files have been updated
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

# library(readr)
#### Write csv #### 
write_named_csv <- function(x) 
  write_csv(x, file = paste0(
    "~/Documents/Trellis/CatCloud_dashboard/clean_data/",
    deparse(substitute(x)),".csv"))

#### read in data ####
getwd()


folder <- "/Users/jungmeepark/Documents/Trellis/CatCloud_dashboard/initial_data"      # path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder

# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(folder,"/", file_list[i], sep=''))
  )}

#### check files ####
# change column names 
# Users_for_Add_or_Edit.csv <- rename(Users_for_Add_or_Edit.csv, Add.Event.Count = Event.count)
# Users_Goals.csv <- rename(Users_Goals.csv, Goals.Event.Count = Goals.Event.count)

`users of cat cloud services and appointments page.csv` <- rename(`users of cat cloud services and appointments page.csv`, 
                                                                  Appt.Sessions = Sessions)
`users who have added or edited class item.csv` <- rename(`users who have added or edited class item.csv`, 
                                                          Edit.Sessions = Sessions)

`classapp users.csv` <- rename(`classapp users.csv`, Goal.Sessions = Sessions)

appt_only_users.csv <- rename(appt_only_users.csv, Segment = Segment.y, Event.count = Event.count.y, 
                              Stream.name = Stream.name.y, Namespace.ID = Namespace.ID.y, Sessions = Sessions.y, 
                              App.instance.ID = User.ID)

# add to all CC users
All_CC_user_Aug15_Oct25 <- left_join(All_CC_user_Aug15_Oct25.csv, `users of cat cloud services and appointments page.csv`)
All_CC_user_Aug15_Oct25 <- left_join(All_CC_user_Aug15_Oct25, `users who have added or edited class item.csv`) %>% 
                              left_join(`classapp users.csv`)
appt_use <- left_join(appt_only_users.csv, All_CC_user_Aug15_Oct25)

# All_CC_user_Aug15_Oct25 <- left_join(All_CC_user_Aug15_Oct25, appt_only_users.csv)

# combine the rows
# All_CC_user_Aug15_Oct25 <- rbind(single_use, All_CC_user_Aug15_Oct25)

#### merge updated catcloud users to goals and users add or edit ####
# Cat_appt <- rbind(CC_Users_Making_Appts_Aug15_Oct25.csv, CC_Users_Not_Appts_Aug15_Oct25.csv)


# Cat_goals <- left_join(Updated_CatCloud_Users.csv, Users_Goals.csv)
# Cat_goals <- left_join(Cat_appt, Users_Goals.csv)
# Cat_add <- left_join(Cat_goals, Users_for_Add_or_Edit.csv)
Cat_UserID <- All_CC_user_Aug15_Oct25 %>% 
  filter(Namespace.ID == "USER_ID") # keep only the cases we can track

#### merge sfcontact data with google
Cat_SF <- left_join(Cat_UserID, sf_users_IDs_emails.csv, by = c("App.instance.ID" = "User.ID"))

appt_use <- left_join(appt_use, sf_users_IDs_emails.csv, by = c("App.instance.ID" = "User.ID")) %>% 
  distinct()
# new_Cat_SF <- Cat_SF %>% 
#   left_join(Cat_SF, `20221021_student enrollment data from SF.csv`, by = c("Email"="Email"))

# duplicated_App_ID <- Cat_SF %>%
#   group_by(Email) %>% 
#   filter(n_distinct(App.instance.ID) > 1) # there are 99 observations with duplicates
# 
# duplicated_DF <- duplicated_App_ID  %>%
#   group_by(Email) %>%
#   mutate(V = row_number()) %>%
#   ungroup() %>%
#   pivot_wider(
#     id_cols = Email,
#     names_from = V,
#     values_from =  !c(Email, V),
#     values_fill = NA) 

# not_Cat_SF <- anti_join(Updated_CatCloud_Users.csv, sf_users_IDs_emails.csv, by = c("App.instance.ID" = "User.ID"))

Cat_SF <- Cat_SF %>% 
  select(App.instance.ID, Sessions, Segment,
         Appt.Sessions, Edit.Sessions, Goal.Sessions,
         First.Name, Last.Name,
         Last.Login, Email) %>% 
  distinct()

appt_use <- appt_use %>% 
  select(App.instance.ID, Sessions, Segment,
         Appt.Sessions, Edit.Sessions, Goal.Sessions,
         First.Name, Last.Name,
         Last.Login, Email) %>% 
  distinct()


# take out the duplicated Appt only users
appt_filter <- Cat_SF %>% 
  subset(!App.instance.ID %in% appt_use$App.instance.ID) 

# combine the rows
non_duplicated_appt <- rbind(appt_use, appt_filter) %>% 
  distinct()
#### merge all current students based on emails to CatSF ####

#### merge in new sf student data ####
Cat_SF_enroll <- left_join(non_duplicated_appt, `20221021_student enrollment data from SF.csv`) %>% 
  distinct()# no need to add , by = c("Email"="Email")

unique(Cat_SF_enroll$Career)

Cat_SF_enroll$Career <- factor(Cat_SF_enroll$Career, 
                               levels=c("Undergraduate", "Graduate", "Law", "Medical School", 
                                        "Pharmacy", "Veterinary Medicine", "NA"))

unique(Cat_SF_enroll$Class.Standing)

Cat_SF_enroll$Class.Standing <- recode(Cat_SF_enroll$Class.Standing, 
                                       "Freshman" = "Freshman", "Sophomore" = "Sophomore", 
                                                "Junior" = "Junior", "Senior" = "Senior", 
                                                "Graduate" = "Graduate", "Masters" = "Masters",
                                                "Prof 1" = "Professional Year 1", "Prof 2" = "Professional Year 2",
                                                "Prof 3" = "Professional Year 3", "Prof 4" = "Professional Year 4", 
                                                "Doctoral" = "Doctoral", "NA" = "NA")

Cat_SF_enroll$Class.Standing <- factor(Cat_SF_enroll$Class.Standing, 
                                       levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate",
                                                "Masters", "Professional Year 1", 
                                                "Professional Year 2",
                                                "Professional Year 3", "Professional Year 4", "Doctoral", "NA"))
Cat_SF_enroll$Class_Standing_recode <- recode(Cat_SF_enroll$Class.Standing,"Freshman" = "Freshman", "Sophomore" = "Sophomore", 
                                       "Junior" = "Junior", "Senior" = "Senior", 
                                       "Graduate" = "Graduate", "Masters" = "Graduate",
                                       "Professional Year 1" = "Graduate", "Professional Year 2" = "Graduate",
                                       "Professional Year 3" = "Graduate", "Professional Year 4" = "Graduate", "Doctoral" = "Graduate", "NA" = "Other")


Cat_SF_enroll$Class_Standing_recode <- factor(Cat_SF_enroll$Class_Standing_recode, 
                                       levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate", "Other"))

unique(Cat_SF_enroll$Class_Standing_recode)

unique(Headcount_Details.csv$Academic.Level)

Headcount_Details.csv$Academic.Level_recode <- recode(Headcount_Details.csv$Academic.Level,"Freshman" = "Freshman", "Sophomore" = "Sophomore", 
                                              "Junior" = "Junior", "Senior" = "Senior", 
                                              "Graduate" = "Graduate", "Masters" = "Graduate",
                                              "Professional Year 1" = "Graduate", "Professional Year 2" = "Graduate",
                                              "Professional Year 3" = "Graduate", "Professional Year 4" = "Graduate", 
                                              "Doctoral" = "Graduate", "NA" = "Other")


Headcount_Details.csv$Academic.Level_recode <- factor(Headcount_Details.csv$Academic.Level_recode, 
                                              levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate", "Other"))
# create a counter if there is a appt session
# Cat_SF_enroll_Appt <- Cat_SF_enroll %>%
#   dplyr::mutate(
#     Appt = ifelse(Appt.Sessions > 0, 1, 0)
#   )

Cat_SF_enroll <- Cat_SF_enroll %>%
  dplyr::mutate(Appt = ifelse(Appt.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Edit = ifelse(Edit.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Goal = ifelse(Goal.Sessions > 0, 1, 0))


# write_named_csv(Cat_SF_enroll)

#### find number of categories in given groups ####
Headcount_Details.csv %>% 
  group_by(Academic.Program) %>% 
  count(wt= Fall.2022)

#### subset the dates to post 8/15 ####
Cat_SF_enroll$last_login2 <- strptime(Cat_SF_enroll$Last.Login,"%m/%d/%Y %H:%M",tz="GMT")

#### new dataset with filtered dates ####
Cat_date_filter <- Cat_SF_enroll %>% 
  filter(last_login2 > "2022-08-14 16:25:00 GMT")

Cat_date_filter  %>%
  select(App.instance.ID, Email, Primary.College) %>%
  distinct() %>%
  group_by(Primary.College) %>%
  count()

unique(Headcount_Details.csv$Academic.Level)

# Headcount_Details.csv$Academic.Level<- factor(Headcount_Details.csv$Academic.Level, 
#                                        levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate",
#                                                 "Masters", "Professional Year 1", 
#                                                 "Professional Year 2",
#                                                 "Professional Year 3", "Professional Year 4", "Doctoral"))

Headcount_Details.csv %>% 
  group_by(Academic.Level_recode) %>% 
  count(wt= Fall.2022)

Cat_SF_enroll %>% 
  select(App.instance.ID, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  count()


### appt sessions ###
Cat_class_users_count <-  Cat_date_filter  %>%  
  select(App.instance.ID, Email, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  count(Class_Standing_recode, name = "total_class_users")

segment_table <- Cat_date_filter  %>%
              filter(!is.na(Class_Standing_recode)) %>%
              filter(!is.na(Email)) %>%
              select(App.instance.ID, Class_Standing_recode, Segment) %>%
              distinct() %>%
              group_by(Segment) %>%
              count(Class_Standing_recode, name = "N_users") %>%
              left_join(Cat_class_users_count) %>%
  mutate(proportion = N_users/total_class_users)

# OD21 <- OD21 %>%
#   mutate(OPEN = case_when(
#     # "OPEN" will be == 2
#     APP == 1 ~ 2,
#     (SCO12 >= 1 & SCO12 <= 50) ~ 2,
#     NOW == 1 ~ 2,
#     EWEEK == 1 ~ 2,
#     ROLL == 1 & (AT == 1 | AT == 2) ~ 2,
#     ROLL == 1 & AT != 1 & AT != 2 & AT != 3 & AT != -8 ~ 2,
#     
#     # "OPEN" will be == -8
#     ROLL == 1 & AT == -8 ~ -8,
#     APP == -8 | NOW == -8 | ROLL == -8 | EWEEK == -8 ~ -8,
#     AGE == 16 & (SCO12 == 97 | SCO12 == -9) ~ -8,
#     
#     # "OPEN" will be == 1
#     TRUE ~ 1
#   )

mydata <- tibble(ID = c("s1", "s2", "s3", "s4", "s5", "s6"),
                 Goals = c(1, NA, 1, 1, 1, NA),
                 Appts = c(1, 1, NA, NA, 1, NA),
                 Edits = c(NA, 1, 1, NA, 1, 1))

mydata %>% 
  mutate(Goals = ifelse(!is.na(Goals), "G", ""),
         Appts = ifelse(!is.na(Appts), "A", ""),
         Edits = ifelse(!is.na(Edits), "E", ""),
         Total = str_c(Goals, Appts, Edits))

# or 
mydata %>% 
  mutate(across(-ID, .fns = ~ifelse(!is.na(.x), 
                                    str_extract(cur_column(), "^."), "")),
         Total = str_c(Goals, Appts, Edits))


Cat_date_filter <- Cat_date_filter %>% 
  mutate(Goals = ifelse(!is.na(Goal), "G", ""),
         Appts = ifelse(!is.na(Appt), "A", ""),
         Edits = ifelse(!is.na(Edit), "E", ""),
         Total = str_c(Goals, Appts, Edits))

write_named_csv(Cat_date_filter)
# cat_sf_full  %>% 
#   select(App.instance.ID, Class_Standing_recode, Appt.Sessions) %>% 
#   distinct() %>% 
#   group_by(Appt.Sessions) %>% 
#   count(Class_Standing_recode, name = "N_users") %>% 
#   left_join(Cat_class_users_count) %>% 
#   mutate(proportion = N_users/total_class_users) 
# count distinct emails
Cat_date_filter %>%   
  select(App.instance.ID, Campus, Email) %>% 
  distinct() %>% 
  count() # 22111


withDegree_CC <- Cat_date_filter %>% 
  # filter(!is.na(Degree)) %>% 
  # filter(Degree != "")
  select(-Degree, -Primary.College.Code) %>% 
  distinct()
  # summarise(across(Class_Standing_recode, list(n = ~length(.x), prop = ~sum(.x == 1) / n())))
Cat_SF_wide <- withDegree_CC %>%
  group_by(App.instance.ID) %>%
  mutate(
    group = row_number()
  ) %>%
  pivot_wider(
    id_cols = App.instance.ID,
    names_from = group,
    values_from = Primary.College,
    names_sort = TRUE
  ) %>%
  rename(Study1 = "1") %>% 
  rename(Study2 = "2") %>% 
  rename(Study3 = "3") %>%  
  ungroup() %>%
  arrange(App.instance.ID)

# Cat_SF_wide <- Cat_SF_wide %>%
#   select(1:4)

#### recode Academic.Program ####
library(forcats)
Headcount_Details.csv <- Headcount_Details.csv %>%
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

#### recode Primary.College ####
Cat_SF_enroll <- Cat_SF_enroll %>%
  mutate(Primary.College_recode = factor(Primary.College) %>%
           fct_recode(
             "Rogers College of Law" = "James E. Rogers College of Law",
             "Rogers College of Law" = "Law Doctoral",
             "Rogers College of Law" = "Law Masters",
             # "Rogers College of Law" = "Law Non-Degree Seeking",
             "Graduate Coursework" = "Graduate Certificate",
             "Graduate Coursework" = "Graduate Non-Degree Seeking",
             "Graduate Coursework" = "Graduate Degree Seeking",
             "Undergraduate Coursework" = "Undergrad Non-Degree Seeking",
             "Undergraduate Coursework" = "Undergraduate Certificate"))


Academic_program_table <- Headcount_Details.csv %>% 
  count(Academic.Program_recode, wt = Fall.2022, name = "All_students_program") %>% 
  left_join(Cat_SF_enroll %>%   
              filter(last_login2 > "2022-08-14 16:25:00 GMT") %>% 
              select(App.instance.ID, Primary.College_recode) %>% 
              distinct() %>% 
              rename(Academic.Program_recode = Primary.College_recode)    %>% 
              count(Academic.Program_recode, name = "N_users_program")) %>% 
  mutate(proportion = N_users_program/All_students_program)

write_named_csv(Academic_program_table)


# merge in majors to cat_sf_enroll2
Cat_SF_enroll_1 <- Cat_SF_enroll %>% 
  select(-Primary.College, -Primary.College.Code, -Primary.College_recode, -Degree) %>% 
  filter(!is.na(Email)) %>% 
  filter(!is.na(Class_Standing_recode)) %>% 
  distinct()

cat_sf_full <- left_join(Cat_SF_enroll_1, Cat_SF_wide) %>%
  distinct()


#### check for single session users
single_use_cc <- cat_sf_full %>% 
  filter(Sessions==1) %>% 
  distinct()


single_use_table <- cat_sf_full %>% 
  count(Class_Standing_recode, name = "All_students_program") %>% 
  left_join(single_use_cc %>%   
              filter(last_login2 > "2022-08-14 16:25:00 GMT") %>% 
              select(App.instance.ID, Class_Standing_recode) %>% 
              distinct() %>% 
              # rename(Academic.Program_recode = Primary.College_recode)    %>% 
              count(Class_Standing_recode, name = "single_use_count")) %>% 
  mutate(proportion = single_use_count/All_students_program)



####
cc_count <-  cat_sf_full  %>%  
  filter(last_login2 > "2022-08-14 16:25:00 GMT") %>% 
  select(App.instance.ID, Email, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  count(Class_Standing_recode, name = "total_class_users")

cat_sf_full  %>% 
  select(App.instance.ID, Class_Standing_recode, Appt.Sessions) %>% 
  distinct() %>% 
  group_by(Appt.Sessions) %>% 
  count(Class_Standing_recode, name = "N_users") %>% 
  left_join(Cat_class_users_count) %>% 
  mutate(proportion = N_users/total_class_users) 
# cat_sf_full <- cat_sf_full %>%
#   dplyr::rename(
#   Study1 = "1",
#   Study2 = "2",
#   Study3 = "3",
#   Study4 = "4",
#   Study5 = "5")
# n_distinct(cat_sf_full$Email) #18001
########### fix the program campus ####
Headcount_Details.csv %>% 
  group_by(Program.Campus) %>% 
  count(wt= Fall.2022)

Cat_date_filter %>%   
  select(App.instance.ID, Campus) %>% 
  distinct() %>% 
  group_by(Campus) %>% 
  count()

#### recode Arizona Global and Direct ####
Headcount_Details.csv$Program.Campus_recode <- recode(Headcount_Details.csv$Program.Campus,
                                                      "Arizona Global" = "Arizona Global & AG Direct",
                                                      "Arizona Global Direct" = "Arizona Global & AG Direct",
                                                      "Arizona Online" = "Arizona Online", 
                                                      "Community Campus" = "Community Campus", 
                                                      "Distance" = "Distance", "Phoenix"="Phoenix",
                                                      "Southern Arizona" = "Southern Arizona", 
                                                      "University of Arizona - Main" = "University of Arizona - Main")

Cat_SF_enroll$Campus_recode <- recode(Cat_SF_enroll$Campus,
                               "Arizona Global" = "Arizona Global & AG Direct",
                               "Arizona Global Direct" = "Arizona Global & AG Direct",
                               "Arizona Online" = "Arizona Online", 
                               "Community Campus" = "Community Campus", 
                               "Distance" = "Distance", "Phoenix"="Phoenix",
                               "Southern Arizona" = "Southern Arizona", 
                               "University of Arizona - Main" = "University of Arizona - Main", "NA" = "Other")

Cat_date_filter$Campus_recode <- recode(Cat_date_filter$Campus,
                                      "Arizona Global" = "Arizona Global & AG Direct",
                                      "Arizona Global Direct" = "Arizona Global & AG Direct",
                                      "Arizona Online" = "Arizona Online", 
                                      "Community Campus" = "Community Campus", 
                                      "Distance" = "Distance", "Phoenix"="Phoenix",
                                      "Southern Arizona" = "Southern Arizona", 
                                      "University of Arizona - Main" = "University of Arizona - Main", "NA" = "Other")

write_named_csv(Cat_date_filter)
#### campus tables ####
Campus_table <- Headcount_Details.csv %>% 
  count(Program.Campus_recode, wt = Fall.2022, name = "All_students_campus")%>% 
  left_join(Cat_SF_enroll %>%
              filter(last_login2 > "2022-08-14 16:25:00 GMT") %>%
              select(App.instance.ID, Campus_recode) %>% 
              distinct() %>% 
              rename(Program.Campus_recode = Campus_recode)    %>% 
              count(Program.Campus_recode, name = "N_users_campus")) %>% 
  mutate(proportion = N_users_campus/All_students_campus)

write_named_csv(Campus_table)
#######################################



# rename the n columns
Cat_SF_users_count <- Cat_date_filter  %>% 
  select(App.instance.ID, Class_Standing_recode) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  rename(Academic.Level_recode = Class_Standing_recode) %>% 
  count(Academic.Level_recode, name = "N_users")

# left_join the to create proportions
detailed_class_standing_table <- Headcount_Details.csv %>% 
  count(Academic.Level_recode, wt = Fall.2022, name = "All_students") %>% 
  left_join(Cat_SF_users_count) %>% 
  mutate(proportion = N_users/All_students)

write_named_csv(detailed_class_standing_table)

# freshmen who made an appt 
freshman_cc <- Cat_date_filter %>% 
  filter(Class_Standing_recode == "Freshman") %>% 
  select(App.instance.ID, Class_Standing_recode, Appt) %>% 
  distinct() %>% 
  group_by(Class_Standing_recode) %>% 
  count(Class_Standing_recode, name = "total_class_users")

cat_sf_full  %>% 
  select(App.instance.ID, Class_Standing_recode, Appt.Sessions) %>% 
  distinct() %>% 
  group_by(Appt.Sessions) %>% 
  count(Class_Standing_recode, name = "N_users") %>% 
  left_join(Cat_class_users_count) %>% 
  mutate(proportion = N_users/total_class_users) 

Cat_date_filter %>% 
  select(App.instance.ID, Class_Standing_recode, Appt) %>% 
  filter(Class_Standing_recode == "Freshman") %>% 
  filter(!is.na(Class_Standing_recode)) %>% 
  group_by(Appt) %>% 
  count(Appt, name = "Appt_freshman")  %>% 
  # left_join(freshman_cc) %>% 
  mutate(proportion = Appt_freshman/6864)

#### CatCloud users with at least one ClassApp use #### 
CC_Edit <- cat_sf_full %>% 
  filter(Edit == 1) %>%  
  distinct()

CC_No_Edit <- cat_sf_full %>% 
  filter(is.na(Edit)) %>%  
  distinct()

write_named_csv(CC_Edit)
write_named_csv(CC_No_Edit)

# undergraduate certificate program 
Cat_SF_enroll %>% 
  filter(Primary.College.Code == "UCERT") 

# rename the n columns for CAMPUS
# # left_join the to create proportions
# cases2214$counter<-1
# names(cases2214)
# 
# cases2214_agg<-cases2214 %>%
#   group_by(ContactId, RecordType.Name) %>%
#   summarise(casecount = sum(counter)) %>%
#   arrange(ContactId, RecordType.Name)

# write_named_csv(UG_campus_table)

# rename the n columns for COLLEGE
# left_join the to create proportions

# subset to undergraduates only 
UG_Headcount <- Headcount_Details.csv %>% 
  filter(Career == "Undergraduate")


UG_Cat_users <- Cat_SF_enroll %>% 
  filter(Career == "Undergraduate") 


UG_campus_table <- UG_Headcount %>% 
  count(Program.Campus, wt = Fall.2022, name = "All_students_campus")%>% 
  left_join(UG_Cat_users %>%    
              select(App.instance.ID, Campus) %>% 
              distinct() %>% 
              rename(Program.Campus = Campus)    %>% 
              count(Program.Campus, name = "N_users_campus")) %>% 
  mutate(proportion = N_users_campus/All_students_campus)

# write_named_csv(UG_Cat_users)

UG_Acad_prog_table <- UG_Headcount %>% 
  count(Academic.Program, wt = Fall.2022, name = "All_students_program") %>% 
  left_join(UG_Cat_users %>%    
              select(App.instance.ID, Primary.College) %>% 
              distinct() %>% 
              rename(Academic.Program = Primary.College)    %>% 
              count(Academic.Program, name = "N_users_program")) %>% 
  mutate(proportion = N_users_program/All_students_program)

write_named_csv(UG_Acad_prog_table)

# rename the n columns for Career
# left_join the to create proportions
All_class_standing_table <- Headcount_Details.csv %>% 
  count(Career, wt = Fall.2022, name = "All_students_career") %>% 
  left_join(Cat_SF_enroll %>%  
              select(App.instance.ID, Career) %>% 
              distinct() %>% 
              count(Career, name = "N_users_career")) %>% 
  mutate(proportion = N_users_career/All_students_career)
# write_named_csv(All_class_standing_table)
################################################################### END of Merge ####
# Cat_SF_enroll2 <- Cat_SF_enroll %>% 
#   select(-Plan..CPP.Info.Name) %>% 
#   distinct()

# n_distinct(Cat_SF_enroll2$Email) #18001
# sapply(Cat_SF_enroll2, function(x) n_distinct(x))
# Cat_SF_enroll <- Cat_SF_enroll %>% 
#   select(-Primary.College.Code) %>% 
#   distinct
# 
# Cat_SF_wide <- Cat_SF_enroll %>%
#   group_by(App.instance.ID) %>%
#   mutate(
#     group = row_number()
#   ) %>%
#   pivot_wider(
#     id_cols = App.instance.ID, 
#     names_from = group,
#     values_from = Primary.College,
#     names_sort = TRUE
#   ) %>%
#   ungroup() %>%
#   arrange(App.instance.ID)
# 
# Cat_SF_wide <- Cat_SF_wide %>% 
#   select(1:6)
# 
# # merge in majors to cat_sf_enroll2 
# cat_sf_full <- left_join(Cat_SF_wide, Cat_SF_enroll) %>% 
#   distinct()
# 
# cat_sf_full <- cat_sf_full %>% 
#   dplyr::rename(
#   Study1 = "1",
#   Study2 = "2",
#   Study3 = "3",
#   Study4 = "4",
#   Study5 = "5")
# n_distinct(cat_sf_full$Email) #18001
# 
# write_named_csv(cat_sf_full)
# 
# names(cat_sf_full)
# 
# cat_sf_dedup <- cat_sf_full %>% 
#   filter(!is.na(Admit.Term))
# 
# 
# cat_sf_dup <- cat_sf_full %>% 
#   filter(is.na(Admit.Term))
# 
# 
# summary_tables <- cat_sf_full %>%
#   group_by(NetID, App.instance.ID, Career, Class.Standing) %>% 
#   summarize(Goals.Event.count = sum(Goals.Event.count, na.rm = TRUE),
#             Add.Event.Count = sum(Add.Event.Count, na.rm = TRUE),
#             Event.count = sum(Event.count, na.rm = TRUE)) %>% 
#   ungroup() #abriones1028 has two app.instances
# 
# goals_count <- cat_sf_full %>%
#   group_by(NetID, App.instance.ID, Career, Class.Standing) %>% 
#   count(Goals.Event.count, wt = Goals.Event.count, na.rm = TRUE) %>% 
#   ungroup() #abriones1028 has two app.instances

# https://gist.github.com/MCMaurer/0d303a1062c87c97eaa865f4097eba22

# library(tidyverse)
# 
# tibble(group = c("A", "A", "B", "C"),
#        major = c("bio", "chem", "bio", "art")) %>% 
#   group_by(group) %>% 
#   summarise(major = str_c(major, collapse = "/")) %>% 
#   separate(major, into = c("major1", "major2"), sep = "/")

# drop if Admit Type is missing
# cat_sf_full_na <- cat_sf_full %>% 
#   filter(!is.na(Admit.Type) & Admit.Type != "")
# 
# sapply(cat_sf_full_na, function(x) n_distinct(x))
# # 
# cat_SF_NA <- cat_sf_full %>%
#   filter(is.na(Primary.College))
# 
# library(stringr)
# # split_NA_email <- str_split_fixed(cat_SF_NA$Email, "@", n = 2)
# 
# split_NA_email <- cat_SF_NA %>%
#   extract(Email, c("firstpart", "secondpart", "thirdpart"), 
#           "([A-Za-z0-9_.]+)@([a-z.]+)\\.([a-z]+)$", remove = FALSE)
# 
# unmatched_UA_emails <- split_NA_email %>% 
#   filter(secondpart == "arizona") %>% 
#   select(App.instance.ID, Sessions, Engaged.sessions, Event.count, Views, Goals.Event.count, Add.Event.Count,
#          First.Name, Last.Name, Last.Login, Email)
# 
# write_named_csv(unmatched_UA_emails)
# # some are students like manuelvalenzuela@arizona.edu and barretojaviera@arizona.edu
# 
# 
# # 
# # cat_SF_missing <- cat_sf_full %>% 
# #   filter(Admit.Type != "")
# 
# 
# ###################################################
# all_students_tidy <- `20221017_All Current Students_GPA_Major_forCC.csv` %>%
#   filter(Program.Status == "AC") %>%
#   ungroup() %>%
#   tibble::rowid_to_column(var = "unique_Email") %>%
#   gather(key = "Email_type", value = "Email", matches("*.Email.Address"))%>%
#   mutate(Email_type = recode(Email_type,
#                          UA.Email.Address = "UA_email",
#                          Alternate.Email.Address = "Alt_email",
#                          Personal..Email.Address = "Personal_email"))
# 
# 
# 
# Cat_SF_student_emails <- left_join(Cat_SF, all_students_tidy,
#                            by = c("Email" = "Email"))
# 
# # Cat_SF_Students <- Cat_SF_Students %>%
# #   select(-Alternate.Email.Address, -Personal..Email.Address, -Academic.Load, -Academic.Standing, 
# #          -Degree.Earned.Date, -Units.Taken.included.in.GPA) %>%
# #   distinct()
# # Cat_SF_Students <- Cat_SF_Students %>%
# #   select(-Units.Taken.included.in.GPA) %>%
# #   distinct()
# 
# Cat_SF_Students <- Cat_SF_student_emails %>%
#   select(-unique_Email, -Academic.Load, -Academic.Standing, -Degree.Earned.Date, -Email_type, -Units.Taken.included.in.GPA) %>%
#   distinct()
# 
# #### reorder the academic levels ####
# unique(Cat_SF_Students$Academic.Level...Beginning.of.Term)
# 
# Cat_SF_Students$Academic.Level...Beginning.of.Term <- factor(Cat_SF_Students$Academic.Level...Beginning.of.Term, 
#                                                              levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate",
#                                                                       "Masters", "Professional Year 1", "Professional Year 2",
#                                                                       "Professional Year 3", "Professional Year 4", "Doctoral"))
# names(Cat_SF_Students)
# names(`UAccess_Student Academic Information.csv`)
# 
# #### recode the NA ing Academic.Standing, Degree Checkout, Expected graduation term ####
# Cat_SF_Students[Cat_SF_Students == "-"] <- NA
# Cat_SF_Students[Cat_SF_Students == "- "] <- NA
# Cat_SF_Students[Cat_SF_Students == " -"] <- NA
# # Cat2_SF_Students <- merge(x = Cat2_SF, y = Cat_SF_Students,
# #                           by.x = "App.instance.ID", by.y = "App.instance.ID")
# # 
# # Cat2_SF_Students <- Cat2_SF_Students %>% 
# #   select(-Stream.name, -Namespace.ID, -Email.x, -First.Name.x, -Last.Name.x, -Views.x,
# #          -Sessions.x, -Engaged.sessions.x, -Event.count.x, -Last.Login.x, -Term.GPA, -Degree.Earned.Date)
# 
# # Cat_SF_Students2 <- as.data.frame(Cat_SF_Students)
# write_named_csv(Cat_SF_Students)
# 
# #### evaluate data ####
# colSums(is.na(Cat_SF_Students))
# 
# Cat_SF_Students %>% 
#   # group_by(Academic.Level...Beginning.of.Term, Goals.Event.count) %>% 
#   count(Academic.Level...Beginning.of.Term, wt = Goals.Event.count) 
# 
# Cat_SF_Students %>% 
#   # group_by(Academic.Level...Beginning.of.Term, Goals.Event.count) %>% 
#   count(Academic.Level...Beginning.of.Term, wt = Add.Event.Count) 
# 
# Cat_SF_Students %>% 
#   count(Academic.Level...Beginning.of.Term, wt = Event.count) 
# 
# # merge in info from the UAccess_Student Academic Information
# 
# UAccess <- `UAccess_Student Academic Information.csv` %>% 
#   separate(Full.Name, c('Last.Name', 'First.Name'), sep = ',') %>% 
#   mutate(First.Name = trimws(First.Name))
# 
# UAccess2 <- left_join(`UAccess_Student Academic Information.csv`, UAccess)
# 
# new_df <- left_join(Cat_SF_Students, `UAccess_Student Academic Information.csv`)
# 
# 
# # df2 <- df %>% mutate(a = ifelse(a %in% "", c, a))
# # new_df <- new_df %>% 
# #  mutate(Full.Name = ifelse(Full.Name %in% NA, str_c(Last.Name, ",", First.Name), Full.Name))
# 
# new_df <- new_df %>% 
#   mutate(Full.Name = ifelse(is.na(Full.Name), str_c(Last.Name, ",", First.Name), Full.Name))
# 
# new_df2 <- left_join(new_df, `UAccess_Student Academic Information.csv`, by = c("Full.Name" = "Full.Name")) #how to do a near match
# 
# 
# colSums(is.na(new_df2))
# 
# new_df2 <- new_df2 %>% 
#   select(App.instance.ID, Sessions, Engaged.sessions, Event.count, Views, Goals.Event.count, Add.Event.Count, 
#          First.Name, Last.Name, Last.Login, Email, Student.ID.y, Residency.y, College,
#          Academic.Level...Beginning.of.Term.y, Academic.Program.Campus.y, Career.y, Primary.Major.Plan.y,
#          Double.Major.Flag.y, Cumulative.GPA.y)
# 
# no_info <- new_df2 %>% 
#   filter(is.na(College))
# 
# no_acad_info <- new_df2 %>% 
#   filter(is.na(Academic.Level...Beginning.of.Term.y))
# 
# # uaccess_no_acad <- left_join(no_acad_info, UAccess2, by = c("Last.Name" = "Last.Name"))
# # 
# # firstName_uaccess <- uaccess_no_acad %>% 
# #   filter(First.Name.y == First.Name.x)
# # 
# # new_df3 <- left_join(new_df2, firstName_uaccess)
# ### using multiple email match 
# Cat_SF_Students2$Academic.Level...Beginning.of.Term <- factor(Cat_SF_Students2$Academic.Level...Beginning.of.Term, 
#                                                               levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate",
#                                                                        "Masters", "Professional Year 1", "Professional Year 2",
#                                                                        "Professional Year 3", "Professional Year 4", "Doctoral"))
# Cat_SF_Students2 %>% 
#   count(Academic.Level...Beginning.of.Term, wt = Event.count) 
# colSums(is.na(`20221017_All Current Students_GPA_Major_forCC.csv`))
# 
# unique(Cat_SF_Students$College)
# table(Cat_SF_Students$College)
# 
# ### find the differences
# not_UA_email <- anti_join(Cat_SF_Students, Cat_SF_Students2)
# 
# # Cat_SF_Students2 %>% 
# #   group_by(College) %>% 
# #   filter
# 
# df_na <- Cat_SF_Students2 %>% filter_at(vars(College,Career),
#                                         all_vars(is.na(.)))
# 
# # na_Cat_SF <- left_join(df_na, sf_users_IDs_emails.csv, by = c("App.instance.ID" = "User.ID"))
# write_named_csv(df_na)

# #### read in excel file ####
# library(readxl)
# Headcount_Details <- read_excel("initial_data/Headcount Details.xlsx")
# View(Headcount_Details)
# 
# Headcount_Details 
# 
# Headcount_Details %>% 
#   mutate(row = row_number()) %>% 
#   filter(row != 1) %>% 
#   fill(`Program Campus`, .direction = "down")
