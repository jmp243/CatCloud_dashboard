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
# names(`UAccess_Student Academic Information.csv`)
#### merge data and rename datafile ####
# SF_UAccess <- merge(x = `UAccess_Student Academic Information.csv`, y = sf_users_IDs_emails.csv,
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
Cat_add <- Cat_add %>% 
  filter(Namespace.ID == "USER_ID")
# Cat2_SF <- merge(x = Cat_SF, y = Users_for_Add_or_Edit.csv,
#                  by.x = "App.instance.ID", by.y = "App.instance.ID")

# Cat_SF <- merge(x = Updated_CatCloud_Users.csv, y = sf_users_IDs_emails.csv,
#                     by.x = "App.instance.ID", by.y = "User.ID") 

#### merge sfcontact data with google
Cat_SF <- left_join(Cat_add, sf_users_IDs_emails.csv, by = c("App.instance.ID" = "User.ID"))

duplicated_App_ID <- Cat_SF %>%
  group_by(Email) %>% 
  filter(n_distinct(App.instance.ID) > 1) # there are 99 observations with duplicates

duplicated_DF <- duplicated_App_ID  %>%
  group_by(Email) %>%
  mutate(V = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = Email,
    names_from = V,
    values_from =  !c(Email, V),
    values_fill = NA) 

  # filter(!anyDuplicated(filter(n_distinct(date) > 1, n_distinct(value) > 1)))
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
# https://stackoverflow.com/questions/47358243/dplyr-left-join-on-case-when

# Cat_SF_Students <- left_join(Cat_SF, `20221017_All Current Students_GPA_Major_forCC.csv`,
#                              by = c("Email" = "UA.Email.Address"))

# additional_emails <- left_join(Cat_SF, `20221012_All Current Students_GPA_Major_forCC.csv`, 
#                              by = c("Email" = "Personal..Email.Address"))

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

write_named_csv(Cat_SF_enroll)

#### find number of categories in given groups ####
Headcount_Details.csv %>% 
  group_by(Program.Campus) %>% 
  count(wt= Fall.2022)

Headcount_Details.csv %>% 
  group_by(Academic.Program) %>% 
  count(wt= Fall.2022)

Cat_SF_enroll %>% 
  group_by(Primary.College) %>% 
  count()

Headcount_Details.csv %>% 
  group_by(Academic.Level) %>% 
  count(wt= Fall.2022)

Cat_SF_enroll %>% 
  group_by(Class.Standing) %>% 
  count()


unique(Headcount_Details.csv$Academic.Level)

Headcount_Details.csv$Academic.Level<- factor(Headcount_Details.csv$Academic.Level, 
                                       levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate",
                                                "Masters", "Professional Year 1", 
                                                "Professional Year 2",
                                                "Professional Year 3", "Professional Year 4", "Doctoral"))

Cat_SF_enroll_CS <- Cat_SF_enroll %>% 
   filter(!is.na(Class.Standing))


# Cat_SF_enroll2 <- Cat_SF_enroll %>% 
#   select(-Plan..CPP.Info.Name) %>% 
#   distinct()

# n_distinct(Cat_SF_enroll2$Email) #18001
# sapply(Cat_SF_enroll2, function(x) n_distinct(x))

Cat_SF_wide <- Cat_SF_enroll %>%
  group_by(App.instance.ID) %>%
  mutate(
    group = row_number()
  ) %>%
  pivot_wider(
    id_cols = App.instance.ID, 
    names_from = group,
    values_from = Plan..CPP.Info.Name,
    names_sort = TRUE
  ) %>%
  ungroup() %>%
  arrange(App.instance.ID)

Cat_SF_wide <- Cat_SF_wide %>% 
  select(1:8)

# merge in majors to cat_sf_enroll2 
cat_sf_full <- left_join(Cat_SF_enroll, Cat_SF_wide)

cat_sf_full <- cat_sf_full %>% 
  dplyr::rename(
  Study1 = "1",
  Study2 = "2",
  Study3 = "3",
  Study4 = "4",
  Study5 = "5",
  Study6 = "6",
  Study7 = "7"
)
n_distinct(cat_sf_full$Email) #18001

write_named_csv(cat_sf_full)

names(cat_sf_full)
# 
# cat_sf_dedup <- cat_sf_full %>% 
#   filter(!is.na(Admit.Term))
# 
# 
# cat_sf_dup <- cat_sf_full %>% 
#   filter(is.na(Admit.Term))
# 

summary_tables <- cat_sf_full %>%
  group_by(NetID, App.instance.ID, Career, Class.Standing) %>% 
  summarize(Goals.Event.count = sum(Goals.Event.count, na.rm = TRUE),
            Add.Event.Count = sum(Add.Event.Count, na.rm = TRUE),
            Event.count = sum(Event.count, na.rm = TRUE)) %>% 
  ungroup() #abriones1028 has two app.instances

goals_count <- cat_sf_full %>%
  group_by(NetID, App.instance.ID, Career, Class.Standing) %>% 
  count(Goals.Event.count, wt = Goals.Event.count, na.rm = TRUE) %>% 
  ungroup() #abriones1028 has two app.instances

# https://gist.github.com/MCMaurer/0d303a1062c87c97eaa865f4097eba22

library(tidyverse)

tibble(group = c("A", "A", "B", "C"),
       major = c("bio", "chem", "bio", "art")) %>% 
  group_by(group) %>% 
  summarise(major = str_c(major, collapse = "/")) %>% 
  separate(major, into = c("major1", "major2"), sep = "/")

# drop if Admit Type is missing
cat_sf_full_na <- cat_sf_full %>% 
  filter(!is.na(Admit.Type) & Admit.Type != "")

sapply(cat_sf_full_na, function(x) n_distinct(x))
# 
cat_SF_NA <- cat_sf_full %>%
  filter(is.na(Primary.College))

library(stringr)
# split_NA_email <- str_split_fixed(cat_SF_NA$Email, "@", n = 2)

split_NA_email <- cat_SF_NA %>%
  extract(Email, c("firstpart", "secondpart", "thirdpart"), 
          "([A-Za-z0-9_.]+)@([a-z.]+)\\.([a-z]+)$", remove = FALSE)

unmatched_UA_emails <- split_NA_email %>% 
  filter(secondpart == "arizona") %>% 
  select(App.instance.ID, Sessions, Engaged.sessions, Event.count, Views, Goals.Event.count, Add.Event.Count,
         First.Name, Last.Name, Last.Login, Email)

write_named_csv(unmatched_UA_emails)
# some are students like manuelvalenzuela@arizona.edu and barretojaviera@arizona.edu


# 
# cat_SF_missing <- cat_sf_full %>% 
#   filter(Admit.Type != "")


###################################################
all_students_tidy <- `20221017_All Current Students_GPA_Major_forCC.csv` %>%
  filter(Program.Status == "AC") %>%
  ungroup() %>%
  tibble::rowid_to_column(var = "unique_Email") %>%
  gather(key = "Email_type", value = "Email", matches("*.Email.Address"))%>%
  mutate(Email_type = recode(Email_type,
                         UA.Email.Address = "UA_email",
                         Alternate.Email.Address = "Alt_email",
                         Personal..Email.Address = "Personal_email"))



Cat_SF_student_emails <- left_join(Cat_SF, all_students_tidy,
                           by = c("Email" = "Email"))

# Cat_SF_Students <- Cat_SF_Students %>%
#   select(-Alternate.Email.Address, -Personal..Email.Address, -Academic.Load, -Academic.Standing, 
#          -Degree.Earned.Date, -Units.Taken.included.in.GPA) %>%
#   distinct()
# Cat_SF_Students <- Cat_SF_Students %>%
#   select(-Units.Taken.included.in.GPA) %>%
#   distinct()

Cat_SF_Students <- Cat_SF_student_emails %>%
  select(-unique_Email, -Academic.Load, -Academic.Standing, -Degree.Earned.Date, -Email_type, -Units.Taken.included.in.GPA) %>%
  distinct()

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
Cat_SF_Students[Cat_SF_Students == "- "] <- NA
Cat_SF_Students[Cat_SF_Students == " -"] <- NA
# Cat2_SF_Students <- merge(x = Cat2_SF, y = Cat_SF_Students,
#                           by.x = "App.instance.ID", by.y = "App.instance.ID")
# 
# Cat2_SF_Students <- Cat2_SF_Students %>% 
#   select(-Stream.name, -Namespace.ID, -Email.x, -First.Name.x, -Last.Name.x, -Views.x,
#          -Sessions.x, -Engaged.sessions.x, -Event.count.x, -Last.Login.x, -Term.GPA, -Degree.Earned.Date)

# Cat_SF_Students2 <- as.data.frame(Cat_SF_Students)
write_named_csv(Cat_SF_Students)

#### evaluate data ####
colSums(is.na(Cat_SF_Students))

Cat_SF_Students %>% 
  # group_by(Academic.Level...Beginning.of.Term, Goals.Event.count) %>% 
  count(Academic.Level...Beginning.of.Term, wt = Goals.Event.count) 

Cat_SF_Students %>% 
  # group_by(Academic.Level...Beginning.of.Term, Goals.Event.count) %>% 
  count(Academic.Level...Beginning.of.Term, wt = Add.Event.Count) 

Cat_SF_Students %>% 
  count(Academic.Level...Beginning.of.Term, wt = Event.count) 

# merge in info from the UAccess_Student Academic Information

UAccess <- `UAccess_Student Academic Information.csv` %>% 
  separate(Full.Name, c('Last.Name', 'First.Name'), sep = ',') %>% 
  mutate(First.Name = trimws(First.Name))

UAccess2 <- left_join(`UAccess_Student Academic Information.csv`, UAccess)

new_df <- left_join(Cat_SF_Students, `UAccess_Student Academic Information.csv`)


# df2 <- df %>% mutate(a = ifelse(a %in% "", c, a))
# new_df <- new_df %>% 
#  mutate(Full.Name = ifelse(Full.Name %in% NA, str_c(Last.Name, ",", First.Name), Full.Name))

new_df <- new_df %>% 
  mutate(Full.Name = ifelse(is.na(Full.Name), str_c(Last.Name, ",", First.Name), Full.Name))

new_df2 <- left_join(new_df, `UAccess_Student Academic Information.csv`, by = c("Full.Name" = "Full.Name")) #how to do a near match


colSums(is.na(new_df2))

new_df2 <- new_df2 %>% 
  select(App.instance.ID, Sessions, Engaged.sessions, Event.count, Views, Goals.Event.count, Add.Event.Count, 
         First.Name, Last.Name, Last.Login, Email, Student.ID.y, Residency.y, College,
         Academic.Level...Beginning.of.Term.y, Academic.Program.Campus.y, Career.y, Primary.Major.Plan.y,
         Double.Major.Flag.y, Cumulative.GPA.y)

no_info <- new_df2 %>% 
  filter(is.na(College))

no_acad_info <- new_df2 %>% 
  filter(is.na(Academic.Level...Beginning.of.Term.y))

# uaccess_no_acad <- left_join(no_acad_info, UAccess2, by = c("Last.Name" = "Last.Name"))
# 
# firstName_uaccess <- uaccess_no_acad %>% 
#   filter(First.Name.y == First.Name.x)
# 
# new_df3 <- left_join(new_df2, firstName_uaccess)
### using multiple email match 
Cat_SF_Students2$Academic.Level...Beginning.of.Term <- factor(Cat_SF_Students2$Academic.Level...Beginning.of.Term, 
                                                              levels=c("Freshman", "Sophomore", "Junior", "Senior", "Graduate",
                                                                       "Masters", "Professional Year 1", "Professional Year 2",
                                                                       "Professional Year 3", "Professional Year 4", "Doctoral"))
Cat_SF_Students2 %>% 
  count(Academic.Level...Beginning.of.Term, wt = Event.count) 
colSums(is.na(`20221017_All Current Students_GPA_Major_forCC.csv`))

unique(Cat_SF_Students$College)
table(Cat_SF_Students$College)

### find the differences
not_UA_email <- anti_join(Cat_SF_Students, Cat_SF_Students2)

# Cat_SF_Students2 %>% 
#   group_by(College) %>% 
#   filter

df_na <- Cat_SF_Students2 %>% filter_at(vars(College,Career),
                                        all_vars(is.na(.)))

# na_Cat_SF <- left_join(df_na, sf_users_IDs_emails.csv, by = c("App.instance.ID" = "User.ID"))
write_named_csv(df_na)

