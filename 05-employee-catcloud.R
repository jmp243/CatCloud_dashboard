# Jung Mee Park
# 11-06-2023
# retrieving catcloud data using a new skuid tag
# last run 07-01-2024

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

# load in cat date filter data
Cat_date_filter <- read_csv("initial_data/Spring2024/employee_cc/Cat_date_filter copy.csv")

#### Employee Matching ####
Employees <- read_csv("initial_data/Spring2024/employee_cc/employee_from_SF.csv")

#### add employee ID ####
Employees <- Employees %>% 
  select(NetID, `EDS Primary Affiliation`, `Parent Organization`) %>% 
  filter(!is.na(NetID))


Employees_emails <- Employees %>% 
  select(`EDS Primary Affiliation`, `Parent Organization`, Email) %>% 
  filter(!is.na(Email))

Cat_date_filter_nonNA <- Cat_date_filter %>% 
  filter(!is.na(Email))
Cat_date_filter_emails <- Employees_emails %>% left_join(Cat_date_filter_nonNA, relationship = "many-to-many")

Cat_date_filter_emails2 <- Cat_date_filter_nonNA %>% left_join(Employees_emails, relationship = "many-to-many") # failed

table(Cat_date_filter_emails2$`EDS Primary Affiliation`)
#### subset the workers ####
table(Cat_date_filter_emails$`EDS Primary Affiliation`)


#### create a Write csv function#### 
write_named_csv <- function(x) 
  write_csv(x, file = paste0(
    "~/Documents/Trellis/CatCloud_dashboard/clean_data/clean_spring2024/",
    deparse(substitute(x)),".csv"))
write_named_csv(Cat_date_filter)

write_named_csv(Cat_date_filter_emails2)
