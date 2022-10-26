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
Users_for_Add_or_Edit.csv <- rename(Users_for_Add_or_Edit.csv, Add.Event.Count = Event.count)

#### merge updated catcloud users to goals and users add or edit ####
Cat_goals <- left_join(Updated_CatCloud_Users.csv, Users_Goals.csv)
Cat_add <- left_join(Cat_goals, Users_for_Add_or_Edit.csv)
Cat_add <- Cat_add %>% 
  filter(Namespace.ID == "USER_ID") # keep only the cases we can track

#### merge sfcontact data with google
Cat_SF <- left_join(Cat_add, sf_users_IDs_emails.csv, by = c("App.instance.ID" = "User.ID"))

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
  select(App.instance.ID, Sessions, Engaged.sessions, Event.count, Views, Goals.Event.count, Add.Event.Count,
         First.Name, Last.Name,
         Last.Login, Email)

#### merge all current students based on emails to CatSF ####

#### merge in new sf student data ####
Cat_SF_enroll <- left_join(Cat_SF, `20221021_student enrollment data from SF.csv`, by = c("Email"="Email"))

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
                                       "Graduate" = "Graduate", "Masters" = "Masters",
                                       "Prof 1" = "Professional", "Prof 2" = "Professional",
                                       "Prof 3" = "Professional", "Prof 4" = "Professional", "Doctoral" = "Doctoral", "NA" = "Other")


Cat_SF_enroll$Class_Standing_recode <- factor(Cat_SF_enroll$Class_Standing_recode, 
                                       levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate",
                                                "Masters", "Professional", "Doctoral", "Other"))

unique(Cat_SF_enroll$Class_Standing_recode)

# write_named_csv(Cat_SF_enroll)

#### subset the dates to post 8/15 ####
Cat_SF_enroll$last_login2 <- strptime(Cat_SF_enroll$Last.Login,"%m/%d/%Y %H:%M",tz="GMT")

Cat_date_filter <- Cat_SF_enroll %>% 
  filter(last_login2 > "2022-08-14 16:25:00 GMT")

#### find number of categories in given groups ####
Headcount_Details.csv %>% 
  group_by(Academic.Program) %>% 
  count(wt= Fall.2022)

Cat_SF_enroll  %>% 
  select(App.instance.ID, Email, Primary.College) %>% 
  distinct() %>% 
  group_by(Primary.College) %>% 
  count()

unique(Headcount_Details.csv$Academic.Level)

Headcount_Details.csv$Academic.Level<- factor(Headcount_Details.csv$Academic.Level, 
                                       levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate",
                                                "Masters", "Professional Year 1", 
                                                "Professional Year 2",
                                                "Professional Year 3", "Professional Year 4", "Doctoral"))

Headcount_Details.csv %>% 
  group_by(Academic.Level) %>% 
  count(wt= Fall.2022)

Cat_SF_enroll %>% 
  select(App.instance.ID, Class.Standing) %>% 
  distinct() %>% 
  group_by(Class.Standing) %>% 
  count()

# rename the n columns
Cat_SF_users_count <- Cat_SF_enroll %>%  
  select(App.instance.ID, Class.Standing) %>% 
  distinct() %>% 
  group_by(Class.Standing) %>% 
  rename(Academic.Level = Class.Standing) %>% 
  count(Academic.Level, name = "N_users")

# left_join the to create proportions
detailed_class_standing_table <- Headcount_Details.csv %>% 
  count(Academic.Level, wt = Fall.2022, name = "All_students") %>% 
  left_join(Cat_SF_users_count) %>% 
  mutate(proportion = N_users/All_students)

# write_named_csv(detailed_class_standing_table)
########### fix the program campus ####
Headcount_Details.csv %>% 
  group_by(Program.Campus) %>% 
  count(wt= Fall.2022)

Cat_SF_enroll %>%   
  select(App.instance.ID, Campus) %>% 
  distinct() %>% 
  group_by(Campus) %>% 
  count()

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

Headcount_Details.csv %>% 
  count(Academic.Program, wt = Fall.2022, name = "All_students_program") %>% 
  left_join(Cat_SF_enroll %>%    
              select(App.instance.ID, Primary.College) %>% 
              distinct() %>% 
              rename(Academic.Program = Primary.College)    %>% 
              count(Academic.Program, name = "N_users_program")) %>% 
  mutate(proportion = N_users_program/All_students_program)

# rename the n columns for Career
# left_join the to create proportions
All_class_standing_table <- Headcount_Details.csv %>% 
  count(Career, wt = Fall.2022, name = "All_students_career") %>% 
  left_join(Cat_SF_enroll %>%  
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
