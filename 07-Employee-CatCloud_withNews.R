# new view for Employee CatCloud
# starting with soft launch in June 2024
# adding employee catcloud users
# last run 09-27-2024

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
    "~/Documents/Trellis/CatCloud_dashboard/clean_data/clean_fall2024/Employee/",
    deparse(substitute(x)),".csv"))

#### read in excel file ####
#### merge all CC users ####
# CC_all1 <- read_csv("initial_data/Fall2024/CC_all/CC_all_Nov1-Nov30-2023.csv", skip=6)
# CC_all2a <- read_csv("initial_data/Fall2024/CC_all/CC_all_Dec1-Dec31-2023.csv", skip=6)
# CC_all2b <- read_csv("initial_data/Fall2024/CC_all/CC_all_Jan1-Feb29-2024.csv", skip=6)
# CC_all3 <- read_csv("initial_data/Fall2024/CC_all/CC_all_Mar1-Mar31-2024.csv", skip=6)
# CC_all4 <- read_csv("initial_data/Fall2024/CC_all/CC_all_Apr1-Apr30-2024.csv", skip=6)
CC_all5 <- read_csv("initial_data/Fall2024/CC_all/CC_all_May1-May31-2024.csv", skip=6)
CC_all6a <- read_csv("initial_data/Fall2024/CC_all/CC_all_Jun1-Jul31-2024.csv", skip=6) # new format
CC_all6b <- read_csv("initial_data/Fall2024/CC_all/CC_all_Aug1-Aug31-2024.csv", skip=6) # new format
CC_all6c <- read_csv("initial_data/Fall2024/CC_all/CC_all_Sept1-Sept30-2024.csv", skip=6) # new format
CC_all6d <- read_csv("initial_data/Fall2024/CC_all/CC_all_Oct1-Oct16-2024.csv", skip=6) # new format

CC_all <- rbind(CC_all5, CC_all6a, CC_all6b, CC_all6c, CC_all6d)

# CC_all $CC_Date <- as.Date(as.character(CC_all$Date), format='%Y%m%d')
CC_all <- CC_all %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(`Effective user ID`, Date, `Event count`, Sessions, Views, `Engagement rate`, 
         `User engagement`, `Elapsed time from last page request`, `Average session duration`) 
# CC_all <- rbind(CC_all1, CC_all2a, CC_all2b, CC_all3, CC_all4, CC_all5)

#### cases users ####
# CC_cases1 <- read_csv("initial_data/Fall2024/cases/cases_Jan_Sept2_2024.csv", skip = 7)
CC_cases1 <- read_csv("initial_data/Fall2024/cases/Cases_May1-Sept30-2024.csv", skip = 7)
CC_cases2 <- read_csv("initial_data/Fall2024/cases/Cases_Oct1-Oct16-2024.csv", skip = 7)

CC_cases <- rbind(CC_cases1, CC_cases2)


CC_cases <- CC_cases %>%
  select(`Effective user ID`, `Event count`, Sessions)
# 
CC_cases <- rename(CC_cases, Cases.Sessions = Sessions) 
# CC_cases <- rename(CC_cases, Cases.Date = Date) 
CC_cases$Cases.Sessions <- as.numeric(CC_cases$Cases.Sessions)
# CC_cases$Cases.Date <- as.Date(as.character(CC_cases$Cases.Date), format='%Y%m%d')

#### appointment users ####

# CC_appt1 <- read_csv("initial_data/Fall2024/appt/appt_Jan1-Feb29-2024.csv", skip = 6)
# CC_appt2 <- read_csv("initial_data/Fall2024/appt/appt_Mar1-Apr30-2024.csv", skip = 6)
CC_appt3a <- read_csv("initial_data/Fall2024/appt/appt_May1-Jun30-2024.csv", skip = 6)
CC_appt3b <- read_csv("initial_data/Fall2024/appt/appt_July1-Sept30-2024.csv", skip = 6)
CC_appt3c <- read_csv("initial_data/Fall2024/appt/appt_Oct1-Oct16-2024.csv", skip = 6)


CC_appt <- rbind(CC_appt3a, CC_appt3b, CC_appt3c)

CC_appt  <- CC_appt %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(`Effective user ID`, Sessions)   


# rename variables 
CC_appt <- rename(CC_appt, Appt.Sessions = Sessions) #, Namespace.ID = `Namespace ID`, Stream.name = `Stream name`, App.instance.ID = `App-instance ID`

CC_appt <- CC_appt %>%
  select(`Effective user ID`, Appt.Sessions) %>% 
  distinct() 

CC_appt$`Effective user ID` <- as.character(CC_appt$`Effective user ID`)

#### events users ####
CC_events1 <- read_csv("initial_data/Fall2024/events/Events_May1-Sept30-2024.csv", skip = 6)
CC_events2 <- read_csv("initial_data/Fall2024/events/Events_Oct1-Oct16-2024.csv", skip = 6)

CC_events <- rbind(CC_events1, CC_events2)
CC_events <- CC_events %>%
  filter(!is.na(`Stream name`)) %>%
  select(`Effective user ID`, `Total users`) %>% 
  rename(Events.Sessions = `Total users`)

#### import news data ####
News_users1 <- read_csv("initial_data/Fall2024/news/News_May1-Sept30_2024.csv", skip=6)
News_users2 <- read_csv("initial_data/Fall2024/news/News_Oct1-Oct16_2024.csv", skip=6)

CC_news  <- rbind(News_users1, News_users2)
CC_news <- CC_news %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(5))

CC_news <- CC_news %>%
  filter(!is.na(`Stream name`)) %>%
  # filter(Totals != 'Event count') %>%
  select(`Effective user ID`, `Total users`) %>% 
  rename(News.Sessions = `Total users`)

#### read in SF csv ####
all_users_SF <- read_csv("initial_data/Fall2024/from_SF/Oct8_2024_community_users_profile.csv")

# change last login to a readable date format
all_users_SF$last_login2 <- as.Date(mdy_hm(all_users_SF$`Last Login`))
# as.Date(as.character(all_users_SF$`Last Login`), format='%Y%m%d')

all_users_SF <- all_users_SF %>% 
  filter(last_login2 > "2024-04-08") 

# SF users with Parent Org
SF_parent_org_orig <- read_csv("initial_data/Fall2024/from_SF/Active_univ_affiliations_10_16_2024.csv")

SF_parent_org <- SF_parent_org_orig %>% 
  select(-`Affiliation Key`) %>%
  unique()
# Perform full join
all_users_SF_PO <- all_users_SF %>%
  left_join(SF_parent_org)

# all_users_SF_PO <- all_users_SF_PO %>%
#   mutate(NetID_1 = coalesce(NetID...11, NetID...12)) %>% 
#   select(-NetID...11, -NetID...12)
names(all_users_SF_PO)

# coalesce old and new information
# all_users_SF_PO <- all_users_SF_PO %>%
#   mutate(NetID = coalesce(NetID, NetID_1))
# 
# all_users_SF_PO  <- all_users_SF_PO  %>% 
#   select(-NetID_1) %>% 
#   distinct()

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
all_users_SF_PO <- all_users_SF_PO %>% mutate(`SF_18_ID` = SFID_Convert(`User ID`)) %>% 
  rename(SF_15_ID = 'User ID')

#### subset sf_users by profile
unique(all_users_SF_PO$Profile)

#### merge sfcontact data with GA4 data
CC_all_test <- CC_all %>%
  mutate(
    SF_18_ID = ifelse(nchar(`Effective user ID`) == 18, `Effective user ID`, NA),
    SF_15_ID= ifelse(nchar(`Effective user ID`) == 15, `Effective user ID`, NA)
  )
CC_all_15_id <- CC_all_test %>%
  select(SF_15_ID, Date, `Event count`, Sessions, Views
  ) %>% 
  filter(!is.na(SF_15_ID)) 

CC_all_18_id <- CC_all_test %>%
  select(SF_18_ID, Date, `Event count`, Sessions, Views) %>% 
  filter(!is.na(SF_18_ID)) 

# CC_all file has sf15 and sf18 ids
CC_all_15_id <- left_join(CC_all_15_id, all_users_SF_PO) 
CC_all_18_id <- left_join(CC_all_18_id, all_users_SF_PO) 
CC_all_new <- rbind(CC_all_15_id, CC_all_18_id)

# rename variable names
CC_appt <- CC_appt %>% rename(SF_18_ID = `Effective user ID`)
CC_case <- CC_cases %>% rename(SF_18_ID = `Effective user ID`)
CC_event <- CC_events %>% rename(SF_18_ID = `Effective user ID`)
CC_news <- CC_news %>% rename(SF_18_ID = `Effective user ID`)

# join CC to all SF users to get id information
CC_all_new <- left_join(CC_all_new, all_users_SF_PO)
CC_appt <- left_join(CC_appt, all_users_SF_PO)
CC_case <- left_join(CC_case, all_users_SF_PO)
CC_news <- left_join(CC_news, all_users_SF_PO)
CC_event <- left_join(CC_event, all_users_SF_PO)

#### merge columns to CC all data ####
Cat_SF <- CC_all_new %>% left_join(CC_appt) %>% 
  left_join(CC_case) %>%left_join(CC_event) %>% 
  left_join(CC_news)

Cat_SF <- Cat_SF %>% 
  select(SF_15_ID, SF_18_ID, Sessions, Date, 
         Appt.Sessions, Cases.Sessions, Events.Sessions, News.Sessions,
         `Parent Organization`, `Organization: Account Name`, NetID, 
         Email, `First Name`, `Last Name`, `Last Login`, Profile, 
         `EDS Primary Affiliation`, `EDS Affiliations`) %>% 
  distinct()

table(Cat_SF$`EDS Primary Affiliation`)

# retain an original column copy
Cat_SF$EDS_PRIMARY_AFFIL_ORIG <- Cat_SF$`EDS Primary Affiliation`


# Filter out the specific values from the 'EDS Primary Affiliation' column
Cat_SF <- Cat_SF %>%
  filter(!`EDS Primary Affiliation` %in% c("degree-completer", "former-student", "affiliate",
                                           "student", "studentworker", "admit", "gradasst"))

#### Change Last Login to date format ####
Cat_SF$last_login2 <- as.Date(as.character(Cat_SF$Date), format='%Y%m%d')

# create a counter if there is a appt session
Cat_SF_enroll <- Cat_SF %>%
  dplyr::mutate(Appt = ifelse(Appt.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Case = ifelse(Cases.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Event = ifelse(Events.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(New = ifelse(News.Sessions > 0, 1, 0)) 


# group primary eds affiliations for CatCloud data
# remove former student 
# Cat_SF_enroll <- Cat_SF_enroll %>%
#   filter(`EDS Primary Affiliation` != "former-student")

Cat_SF_enroll <- Cat_SF_enroll %>%
  mutate(`EDS Primary Affiliation` = case_when(
    `EDS Primary Affiliation` %in% c("emeritus", "retiree", "former-dcc", "former-faculty", "former-member", 
                                     "former-staff") 
    ~ "former-member",
    TRUE ~ `EDS Primary Affiliation`  # Retain other values as they are
  ))


#### new dataset with filtered dates ####
Cat_date_filter <- Cat_SF_enroll %>% 
  filter(last_login2 > "2024-04-21 06:25:00 GMT") 

Cat_date_filter <- Cat_date_filter %>%
  mutate(Appts = ifelse(!is.na(Appt), "A", ""),
         Cases = ifelse(!is.na(Case), "C", ""),
         Events = ifelse(!is.na(Event), "eV", ""),
         News = ifelse(!is.na(New), "N", ""),
         Total = str_c(Cases, Appts, Events, News)) # omit | (!is.na(Goal1) 
# 
# Cat_date_filter <- Cat_date_filter %>% 
#   select(-EDS_PRIMARY_AFFIL_ORIG)




#### saturation by Affiliation type ####
# active_aff <- read_csv("initial_data/Fall2024/from_SF/active_univ_affiliation_9-4-24.csv") # might be a redundant step


# group primary eds affiliations
all_users_SF_PO$EDS_Primary_aff_orig <- all_users_SF_PO$`EDS Primary Affiliation`

# Filter out the specific values from the 'EDS Primary Affiliation' column
all_users_SF_PO <- all_users_SF_PO %>%
  filter(!`EDS Primary Affiliation` %in% c("degree-completer", "former-student", "affiliate",
                                           "student", "studentworker", "admit", "gradasst"))
# remove degree completer
all_users_SF_PO <- all_users_SF_PO %>%
  mutate(`EDS Primary Affiliation` = case_when(
    `EDS Primary Affiliation` %in% c("emeritus", "retiree", "former-dcc", "former-faculty", "former-member", 
                                     "former-staff") 
    ~ "former-member",
    TRUE ~ `EDS Primary Affiliation`  # Retain other values as they are
  ))

#### primary affiliation table ####
EDS_Primary_aff_table <- all_users_SF_PO %>%
  count(`EDS Primary Affiliation`, name = "All_Affiliation")%>% 
  left_join(Cat_date_filter %>%
              select(SF_18_ID, `EDS Primary Affiliation`) %>%
              distinct() %>%
              # rename(Program.Campus_recode = Campus.recode)    %>%
              count(`EDS Primary Affiliation`, name = "N_users_affil")) %>%
  mutate(proportion = N_users_affil/All_Affiliation)
write_named_csv(EDS_Primary_aff_table)

# SF Univ Parent Org table #
SF_Parent_org_table <- SF_parent_org %>%
  count(`Parent Organization`, name = "All_Parent_org")%>% 
  left_join(Cat_date_filter %>%
              select(SF_18_ID, `Parent Organization`) %>%
              distinct() %>%
              # rename(Program.Campus_recode = Campus.recode)    %>%
              count(`Parent Organization`, name = "N_users_P_org")) %>%
  mutate(proportion = N_users_P_org/All_Parent_org)

write_named_csv(SF_Parent_org_table)

# all Parent org table #
All_Parent_org_table <- all_users_SF_PO %>%
  count(`Parent Organization`, name = "All_Parent_org")%>% 
  left_join(Cat_date_filter %>%
              select(SF_18_ID, `Parent Organization`) %>%
              distinct() %>%
              # rename(Program.Campus_recode = Campus.recode)    %>%
              count(`Parent Organization`, name = "N_users_P_org")) %>%
  mutate(proportion = N_users_P_org/All_Parent_org)

write_named_csv(All_Parent_org_table)

#delete if email is blank
Cat_date_filter2employee <- Cat_date_filter[!(is.na(Cat_date_filter$Email) |Cat_date_filter$Email==""), ]

#### write Cat date filter to file ####
#### rename effective user id ####
Cat_date_filter <- Cat_date_filter %>% 
  rename(`App-instance ID` = SF_18_ID)


Cat_date_filter_employee <- Cat_date_filter |>
  separate_longer_delim(cols = `EDS Affiliations`, delim = "; ")

Cat_date_filter_employee <- Cat_date_filter_employee %>% 
  filter(`EDS Affiliations` != "member") %>% 
  filter(`EDS Affiliations` != "employee")

# Cat_date_filter_employee <- as.data.frame(Cat_date_filter)

write_named_csv(Cat_date_filter_employee)

#### single line employees ####
# employees_login <- Cat_date_filter_employee %>% 
#   # select(NetID, Email, `EDS Primary Affiliation`, `EDS Affiliations`, `Parent Organization`,
#   #        last_login2) %>% 
#   filter(last_login2 > "2024-06-01") %>% 
#   distinct()
# 
# last 90 days
# To filter for the last 90 days in R using the last_login2 date variable, you can use the Sys.Date() function to get the current date and then subtract 90 days. Here's an example:
# r

# Ensure the last_login2 column is in Date format
## If it is not already, you can convert it like this:

Cat_date_filter_employee$last_login2 <- as.Date(Cat_date_filter_employee$last_login2)

# # Filter for the last 90 days
# last_101_days_data <- Cat_date_filter_employee[Cat_date_filter_employee$last_login2 >= Sys.Date() - 101, ]
# last_101_days_data_email_count2 <- length(unique(last_101_days_data$Email))
# print(last_101_days_dataemail_count2)
# 
# 
# table(last_99_days_data$`EDS Primary Affiliation`) %>% 
#   unique()
# 
# names(last_99_days_data)
# write_named_csv(last_99_days_data)
# 
# last_90_days_data <- Cat_date_filter_employee[Cat_date_filter_employee$last_login2 >= Sys.Date() - 90, ]
# write_named_csv(last_90_days_data)
# 
# # take out affiliates 
# last_90_days_data <- last_90_days_data %>% 
#   select(-Date, -`Last Login`) %>% 
#   filter(!is.na(`EDS Primary Affiliation`)) %>%
#   unique()
# 
# last_90_days_data_email_only <- last_90_days_data %>%  
#   select(Email, `EDS Primary Affiliation`) %>% 
#   unique(  ) 
# write_named_csv(last_90_days_data_email_only)
# 
# ## filtering data
# last_99_days_data <- last_99_days_data %>% 
#   select(-Date, -`Last Login`) %>% 
#   filter(!is.na(`EDS Primary Affiliation`)) %>%
#   unique()
# 
# last_99_days_data_email_only <- last_99_days_data %>%  
#   select(Email, `EDS Primary Affiliation`) %>% 
#   unique(  ) 
# write_named_csv(last_90_days_data_email_only)
# 
# last_99_days_data_email_only <- last_99_days_data %>%  
#   select(Email, `EDS Primary Affiliation`) %>% 
#   unique(  ) 
# write_named_csv(last_99_days_data_email_only)
# 
# last_90_days_data_email_Parent_Org <- last_90_days_data %>% 
#   select(Email, `First Name`, `Last Name`, `Parent Organization`, `Organization: Account Name`, `EDS Primary Affiliation`) %>% 
#   distinct()
# 
# unique_email_count <- length(unique(last_90_days_data$Email))
# print(unique_email_count)
# 
# write_named_csv(last_90_days_data_email_Parent_Org)
# unique_email_count2 <- length(unique(last_90_days_data_email_single_line$Email))
# print(unique_email_count2)
# 
# # old data
# employees_who_logged_in_since_6_2024 <- read_csv("clean_data/clean_fall2024/employees_who_logged_in_since_6_2024.csv")
# View(employees_who_logged_in_since_6_2024)
# employees_who_logged_in<- employees_who_logged_in_since_6_2024 %>% 
#   filter(`EDS Primary Affiliation` == "staff" | `EDS Primary Affiliation` == "faculty" | `EDS Primary Affiliation` == "dcc")
# unique_email_count3 <- length(unique(employees_who_logged_in$Email))
# print(unique_email_count3)
