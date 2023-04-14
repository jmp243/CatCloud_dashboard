# UA online focus
# 2023-04-14

# libraries
library(tidyverse)

fall_cc <- read_csv("clean_data/Cat_date_filter.csv")
names(fall_cc)
spring_cc <- read_csv("clean_data/clean_spring2023/Cat_date_filter.csv")
names(spring_cc)

# same number of columns 
fall_cc <- fall_cc %>% 
  select(-Total)

spring_cc <- spring_cc %>% 
  select(-Cases.Sessions, -Case, -Cases, -Total, -Campus.recode)
names(spring_cc)

spring_cc <- spring_cc %>% 
  rename(Campus.recode = Campus.recode1)

# rename columns to match fall and spring
fall_cc_short <- fall_cc %>% 
  select(`App-instance ID`, Appt.Sessions, Edit.Sessions, Goal.Sessions, First.Name, Last.Name, Email, 
         NetID, Primary.College, Primary.College_recode, 
         Campus, Campus_recode, 
         Class.Standing, Class_Standing_recode, 
         Appt, Edit, Goal, Appts, Edits, Goals, last_login2)


spring_cc_short <- spring_cc %>% 
  select(`App-instance ID`, Appt.Sessions, Edit.Sessions, Goal.Sessions, First.Name, Last.Name, Email, 
         NetID, `Primary College`, Primary.College_recode, 
         Campus, Campus.recode, 
         `Class Standing`, Class_Standing_recode, 
         Appt, Edit, Goal, Appts, Edits, Goals, last_login2) %>% 
  rename(Primary.College = `Primary College`) %>% 
  rename(Class.Standing = `Class Standing`) %>% 
  rename(Campus_recode = Campus.recode)

# merge the fall and spring data
names(fall_cc_short)
names(spring_cc_short)
all_cc_short <- rbind(fall_cc_short, spring_cc_short)

# add back totals columns
all_cc_short <- all_cc_short %>%
  mutate(Goals = ifelse(!is.na(Goal), "G", ""), 
         Appts = ifelse(!is.na(Appt), "A", ""),
         Edits = ifelse(!is.na(Edit), "E", ""),
         Total = str_c(Goals, Appts, Edits)) # omit | (!is.na(Goal1) from line 348

#### Write csv #### 
write_named_csv <- function(x) 
  write_csv(x, file = paste0(
    "~/Documents/Trellis/CatCloud_dashboard/clean_data/clean_spring2023/",
    deparse(substitute(x)),".csv"))

write_named_csv(all_cc_short)
