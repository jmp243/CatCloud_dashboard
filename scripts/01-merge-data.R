# read in CatCloud data
# Jung Mee Park
# 2022-23-09
# 2022-12-10 files have been updated

#### load libraries ####
library("googleAnalyticsR")
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
names(`UAccess_Student Academic Information.csv`)
#### merge data and rename datafile ####
# SF_UAccess <- merge(x = `UAccess_Student Academic Information.csv`, y = SFContactData.csv,
#                     by.x = "Student.ID", by.y = "Emplid") 
# 

# 
# #### updated CatCloud APP instance ID matches useriD
# #### count the NA ####
# colSums(is.na(SF_UAccess))
# 
# #### create a concatenated column with EDS affiliations
# # split EDS affiliations with ; separator
# 
# SF_UAccess[c('eds1', 'eds2', 'eds3', 'eds4', 'eds5')] <- str_split_fixed(SF_UAccess$EDS.Affiliations, ";", 5)

# change column names 
Users_for_Add_or_Edit.csv <- rename(Users_for_Add_or_Edit.csv, Add.Event.Count = Event.count)

#### merge updated catcloud users to goals and users add or edit ####
Cat_goals <- left_join(Updated_CatCloud_Users.csv, Users_Goals.csv)
Cat_add <- left_join(Cat_goals, Users_for_Add_or_Edit.csv)
# Cat2_SF <- merge(x = Cat_SF, y = Users_for_Add_or_Edit.csv,
#                  by.x = "App.instance.ID", by.y = "App.instance.ID")

# Cat_SF <- merge(x = Updated_CatCloud_Users.csv, y = sf_users_IDs_emails.csv,
#                     by.x = "App.instance.ID", by.y = "User.ID") 

#### merge sfcontact data with google
Cat_SF <- left_join(Cat_add, sf_users_IDs_emails.csv, by = c("App.instance.ID" = "User.ID"))

# not_Cat_SF <- anti_join(Updated_CatCloud_Users.csv, sf_users_IDs_emails.csv, by.x = "App.instance.ID", by.y = "User.ID")
not_Cat_SF <- anti_join(Updated_CatCloud_Users.csv, sf_users_IDs_emails.csv, by = c("App.instance.ID" = "User.ID"))

# write_named_csv(not_Cat_SF)
Cat_SF <- Cat_SF %>% 
  select(App.instance.ID, Sessions, Engaged.sessions, Event.count, Views, Goals.Event.count, Add.Event.Count,
         First.Name, Last.Name,
         Last.Login, Email)

#### merge all current students based on emails to CatSF ####
# Cat_SF_Students <- merge(x = Cat_SF, y = `20221012_All Current Students_GPA_Major_forCC.csv`,
#                          by.x = "Email", by.y = "UA.Email.Address") 
Cat_SF_Students <- left_join(Cat_SF, `20221012_All Current Students_GPA_Major_forCC.csv`, 
                             by = c("Email" = "UA.Email.Address"))

Cat_SF_Students <- Cat_SF_Students %>% 
  select(-Alternate.Email.Address, -Personal..Email.Address, -Academic.Load, -Academic.Standing, -Degree.Earned.Date)

#### reorder the academic levels ####
unique(Cat_SF_Students$Academic.Level...Beginning.of.Term)

Cat_SF_Students$Academic.Level...Beginning.of.Term <- factor(Cat_SF_Students$Academic.Level...Beginning.of.Term, 
                                                             levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate",
                                                                      "Masters", "Professional Year 1", "Professional Year 2",
                                                                      "Professional Year 3", "Professional Year 4", "Doctoral"))
names(Cat_SF_Students)
names(`UAccess_Student Academic Information.csv`)

#### recode the NA ing Academic.Standing, Degree Checkout, Expected graduation term ####
Cat_SF_Students[Cat_SF_Students == "-"] <- NA


# Cat2_SF_Students <- merge(x = Cat2_SF, y = Cat_SF_Students,
#                           by.x = "App.instance.ID", by.y = "App.instance.ID")
# 
# Cat2_SF_Students <- Cat2_SF_Students %>% 
#   select(-Stream.name, -Namespace.ID, -Email.x, -First.Name.x, -Last.Name.x, -Views.x,
#          -Sessions.x, -Engaged.sessions.x, -Event.count.x, -Last.Login.x, -Term.GPA, -Degree.Earned.Date)


write_named_csv(Cat_SF_Students)

#### evaluate data ####
Cat_SF_Students %>% 
  # group_by(Academic.Level...Beginning.of.Term, Goals.Event.count) %>% 
  count(Academic.Level...Beginning.of.Term, wt = Goals.Event.count) 

Cat_SF_Students %>% 
  # group_by(Academic.Level...Beginning.of.Term, Goals.Event.count) %>% 
  count(Academic.Level...Beginning.of.Term, wt = Add.Event.Count) 



