##### Lab week 4 - Hypothesis and p-value ####
### Area Studies - Mary Zhang ###

# Calling packages and options ----
library(readr)
library(tidyverse)

# Calling dataset ----
students <- read.csv("Data/students.csv")

# Finding the mean of studied hours -----
  mean(students$hours)

# Running one-sample t-test
  t.test(x = students$hours, mu = 8)

# Reporting the results
  # "The  mean  of  study  hours  during  weekends  by  students  in  the  sample  was  5.5, 
  # which  was  significantly  different  from  8  hours  (t  =  -6.63,  df  =  51,  p  <  .001). 
  # Therefore,  we  can  reject  the  null  hypothesis  that  a  'typical'  student  in  this  high 
  # school had 8 study hours per day during weekends".