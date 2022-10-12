# read in CatCloud data
# Jung Mee Park
# 2022-23-09

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
#### read in data ####
getwd()


folder <- "/Users/jungmeepark/Documents/Trellis/CatCloud_dashboard/initial_data"      # path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder

# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(folder,"/", file_list[i], sep=''))
  )}

#### merge data and rename datafile ####
SF_UAccess <- merge(x = `UAccess_Student Academic Information.csv`, y = SFContactData.csv,
                    by.x = "Student.ID", by.y = "Emplid") 

#### recode the NA ing Academic.Standing, Degree Checkout, Expected graduation term ####
SF_UAccess[SF_UAccess == "-"] <- NA  

#### count the NA ####
colSums(is.na(SF_UAccess))

#### create a concatenated column with EDS affiliations
# split EDS affiliations with ; separator

SF_UAccess[c('eds1', 'eds2', 'eds3', 'eds4', 'eds5')] <- str_split_fixed(SF_UAccess$EDS.Affiliations, ";", 5)


