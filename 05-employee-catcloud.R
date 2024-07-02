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
  select(NetID, `EDS Primary Affiliation`, `Parent Organization`)

Cat_date_filter_new <- Employees %>% left_join(Cat_date_filter, relationship = "many-to-many")

#### create a Write csv function#### 
write_named_csv <- function(x) 
  write_csv(x, file = paste0(
    "~/Documents/Trellis/CatCloud_dashboard/clean_data/clean_spring2024/",
    deparse(substitute(x)),".csv"))
write_named_csv(Cat_date_filter)
