# new view for Employee CatCloud
# adding employee catcloud users
# last run 08-30-2024


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
    "~/Documents/Trellis/CatCloud_dashboard/clean_data/clean_fall2024/",
    deparse(substitute(x)),".csv"))

#### read in excel file ####
#### merge all CC users ####
CC_all1 <- read_csv("initial_data/Fall2024/CC_all/CC_all_Jan-Feb29-2024.csv", skip=6)
CC_all2a <- read_csv("initial_data/Fall2024/CC_all/CC_all_Mar-2024.csv", skip=6)
CC_all2b <- read_csv("initial_data/Fall2024/CC_all/CC_all_Apr-2024.csv", skip=6)
CC_all3 <- read_csv("initial_data/Fall2024/CC_all/CC_all_May-2024.csv", skip=6)
CC_all4 <- read_csv("initial_data/Fall2024/CC_all/CC_all_Jun-Jul-2024.csv", skip=6)
CC_all5 <- read_csv("initial_data/Fall2024/CC_all/CC_all_Aug25-2024.csv", skip=6)

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

CC_all <- rbind(CC_all1, CC_all2a, CC_all2b, CC_all3, CC_all4, CC_all5)

#### cases users ####
CC_cases1 <- read_csv("initial_data/Fall2024/cases/cases_Jan_Aug25_2024.csv", skip = 7)
# CC_cases2 <- read_csv("initial_data/Fall2024/cases-Apr1-Aug13-2024.csv", skip = 7)

CC_cases1 <- CC_cases1 %>%
  filter(!is.na(Date)) %>%
  # filter(Totals != 'Event count') %>%
  select(`Effective user ID`, `Event count`, Sessions)

CC_cases <- rename(CC_cases1, Cases.Sessions = Sessions)

CC_cases$Cases.Sessions <- as.numeric(CC_cases$Cases.Sessions)
#### appointment users ####
CC_appt1 <- read_csv("initial_data/Fall2024/appt/appt-Jan-Mar15-2024.csv", skip = 6)
CC_appt2 <- read_csv("initial_data/Fall2024/appt/appt-users-newID-Mar16-May12-2024.csv", skip = 6)
CC_appt3 <- read_csv("initial_data/Fall2024/appt/Appt-May13-Aug25-2024.csv", skip = 6)

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
# rename variables 
CC_appt <- rename(CC_appt, Appt.Sessions = Sessions) #, Namespace.ID = `Namespace ID`, Stream.name = `Stream name`, App.instance.ID = `App-instance ID`

CC_appt <- CC_appt %>%
  select(`Effective user ID`, Appt.Sessions) %>% 
  distinct() 
# change numbers into characters 
CC_appt$`Effective user ID` <- as.character(CC_appt$`Effective user ID`)


#### events users ####
CC_events1 <- read_csv("initial_data/Fall2024/events/events-Jan-Aug25_2024.csv", skip = 6)
# CC_events2 <- read_csv("initial_data/Fall2024/events-Apr1-Aug13-2024.csv", skip = 6)
# CC_events1 <- rbind(CC_events1, CC_events2)

CC_events <- CC_events1 %>%
  filter(!is.na(`Stream name`)) %>%
  # filter(Totals != 'Event count') %>%
  select(`Effective user ID`, `Total users`) %>% 
  rename(Events.Sessions = `Total users`)

#### import news data ####
News_users <- read_csv("initial_data/Fall2024/news/News_Jan-Aug27_2024.csv", skip=6)
CC_news <- News_users %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(5))

CC_news <- CC_news %>%
  filter(!is.na(`Stream name`)) %>%
  # filter(Totals != 'Event count') %>%
  select(`Effective user ID`, `Total users`) %>% 
  rename(News.Sessions = `Total users`)

#### read in SF csv ####
all_users_SF <- read_csv("initial_data/Fall2024/from_SF/Aug25_2024_community_users_profile.csv")

# change last login to a readable date format
all_users_SF$last_login2 <- as.Date(mdy_hm(all_users_SF$`Last Login`))
# as.Date(as.character(all_users_SF$`Last Login`), format='%Y%m%d')

all_users_SF <- all_users_SF %>% 
  filter(last_login2 > "2023-08-08") 

# SF users with Parent Org
SF_parent_org <- read_csv("initial_data/Fall2024/from_SF/SF_Parent_organization_Aug26_2024.csv")

# Perform full join
all_users_SF_PO <- all_users_SF %>%
  left_join(SF_parent_org)

all_users_SF_PO <- all_users_SF_PO %>%
  mutate(NetID_1 = coalesce(NetID...10, NetID...11)) %>% 
  select(-NetID...10, -NetID...11)
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
         `Parent Organization`, `Organization: Account Name`,
         Email, `First Name`, `Last Name`, `Last Login`, Profile, 
         `EDS Primary Affiliation`, `EDS Affiliations`) %>% 
  distinct()

# not merging in student enrollment data 
#### Change Last Login to date format ####
Cat_SF$last_login2 <- as.Date(as.character(Cat_SF$Date), format='%Y%m%d')

# create a counter if there is a appt session
Cat_SF_enroll <- Cat_SF %>%
  dplyr::mutate(Appt = ifelse(Appt.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Case = ifelse(Cases.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(Event = ifelse(Events.Sessions > 0, 1, 0)) %>% 
  dplyr::mutate(New = ifelse(News.Sessions > 0, 1, 0)) 


# retain an original column copy
Cat_SF_enroll$EDS_PRIMARY_AFFIL_ORIG <- Cat_SF_enroll$`EDS Primary Affiliation`

# group primary eds affiliations for CatCloud data
Cat_SF_enroll <- Cat_SF_enroll %>%
  mutate(`EDS Primary Affiliation` = case_when(
    `EDS Primary Affiliation` %in% c("emeritus", "retiree", "former-dcc", "former-faculty", "former-member", 
                                     "former-staff", "former-student") 
    ~ "former-member",
    TRUE ~ `EDS Primary Affiliation`  # Retain other values as they are
  ))


#### new dataset with filtered dates ####
Cat_date_filter <- Cat_SF_enroll %>% 
  filter(last_login2 > "2024-01-01 06:25:00 GMT") 

Cat_date_filter <- Cat_date_filter %>%
  mutate(Appts = ifelse(!is.na(Appt), "A", ""),
         Cases = ifelse(!is.na(Case), "C", ""),
         Events = ifelse(!is.na(Event), "eV", ""),
         News = ifelse(!is.na(New), "N", ""),
         Total = str_c(Cases, Appts, Events, News)) # omit | (!is.na(Goal1) 

Cat_date_filter <- Cat_date_filter %>% 
  select(-EDS_PRIMARY_AFFIL_ORIG)


#### rename effective user id ####
Cat_date_filter <- Cat_date_filter %>% 
  rename(`App-instance ID` = SF_18_ID)

#### saturation by Affiliation type ####
active_aff <- read_csv("initial_data/Fall2024/from_SF/active_univ_affiliations_SF_Aug25_2024.csv")


# group primary eds affiliations
active_aff$EDS_Primary_aff_orig <- active_aff$`EDS Primary Affiliation`

active_aff <- active_aff %>%
  mutate(`EDS Primary Affiliation` = case_when(
    `EDS Primary Affiliation` %in% c("emeritus", "retiree", "former-dcc", "former-faculty", "former-member", 
                                     "former-staff", "former-student") 
    ~ "former-member",
    TRUE ~ `EDS Primary Affiliation`  # Retain other values as they are
  ))

all_users_SF$EDS_Primary_aff_orig <- all_users_SF$`EDS Primary Affiliation`
all_users_SF <- all_users_SF %>%
  mutate(`EDS Primary Affiliation` = case_when(
    `EDS Primary Affiliation` %in% c("emeritus", "retiree", "former-dcc", "former-faculty", "former-member", 
                                     "former-staff", "former-student") 
    ~ "former-member",
    TRUE ~ `EDS Primary Affiliation`  # Retain other values as they are
  ))

# create a table based on affiliations #
EDS_Primary_aff_table <- all_users_SF %>%
  count(`EDS Primary Affiliation`, name = "All_Affiliation")%>% 
  left_join(Cat_date_filter %>%
              select(`App-instance ID`, `EDS Primary Affiliation`) %>%
              distinct() %>%
              # rename(Program.Campus_recode = Campus.recode)    %>%
              count(`EDS Primary Affiliation`, name = "N_users_affil")) %>%
  mutate(proportion = N_users_affil/All_Affiliation)
write_named_csv(EDS_Primary_aff_table)

# SF Univ Parent Org table #
SF_Parent_org_table <- SF_parent_org %>%
  count(`Parent Organization`, name = "All_Parent_org")%>% 
  left_join(Cat_date_filter %>%
              select(`App-instance ID`, `Parent Organization`) %>%
              distinct() %>%
              # rename(Program.Campus_recode = Campus.recode)    %>%
              count(`Parent Organization`, name = "N_users_P_org")) %>%
  mutate(proportion = N_users_P_org/All_Parent_org)

write_named_csv(SF_Parent_org_table)

# all Parent org table #
All_Parent_org_table <- active_aff %>%
  count(`Parent Organization`, name = "All_Parent_org")%>% 
  left_join(Cat_date_filter %>%
              select(`App-instance ID`, `Parent Organization`) %>%
              distinct() %>%
              # rename(Program.Campus_recode = Campus.recode)    %>%
              count(`Parent Organization`, name = "N_users_P_org")) %>%
  mutate(proportion = N_users_P_org/All_Parent_org)

write_named_csv(All_Parent_org_table)


#### write Cat date filter to file ####
Cat_date_filter_employee <- as.data.frame(Cat_date_filter)
write_named_csv(Cat_date_filter_employee)
