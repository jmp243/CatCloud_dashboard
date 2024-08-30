# student catcloud with new features
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
    "~/Documents/Trellis/CatCloud_dashboard/clean_data/clean_fall2024/Student/",
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

CC_appt$`Effective user ID` <- as.character(CC_appt$`Effective user ID`)

#### add edit users ####
CC_edit <- read_csv("initial_data/Fall2024/add_edit/add_edit_Jan-Aug25-2024.csv", skip = 6)

CC_edit <- CC_edit %>% 
  filter(!is.na(`Effective user ID`)) %>% 
  select(-c(7:8))  

CC_edit <- rename(CC_edit, Edit.Sessions = Sessions)
CC_edit <- CC_edit %>% 
  select(`Effective user ID`, Edit.Sessions) %>% 
  distinct()

#### goals users ####
CC_goals1 <- read_csv("initial_data/Fall2024/goals/goals-Jan-Aug25-2024.csv", skip = 6)

CC_goals <- CC_goals1 %>%
  filter(!is.na(`Effective user ID`)) %>%
  select(-c(7:8))

CC_goals <- rename(CC_goals, Goal.Sessions = Sessions)
CC_goals <- CC_goals %>%
  select(`Effective user ID`, Goal.Sessions) %>% 
  distinct()

#### events users ####
CC_events1 <- read_csv("initial_data/Fall2024/events/events-Jan-Aug25_2024.csv", skip = 6)

CC_events <- CC_events1 %>%
  filter(!is.na(`Stream name`)) %>%
  select(`Effective user ID`, `Total users`) %>% 
  rename(Events.Sessions = `Total users`)

#### read in SF csv ####
all_users_SF <- read_csv("initial_data/Fall2024/from_SF/Aug25_2024_community_users_profile.csv")

# change last login to a readable date format
all_users_SF$last_login2 <- as.Date(mdy_hm(all_users_SF$`Last Login`))

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
