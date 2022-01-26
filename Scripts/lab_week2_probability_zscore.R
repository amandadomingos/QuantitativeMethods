##### Lab week 2 - probability and z-scores ####
### Area Studies - TA: Cora ###

# Calling packages and options ----
library(readr)
library(tidyverse)

# Calling dataset ----
disasters <- read.csv("Data/disasters.csv")

# Creating a plot from the count of natural disasters ----
  plot(disasters$year, disasters$events,
       type = "o", #for just a linear plot, change for "l"
       col= "black",
       xlab = "Year",
       ylab = "Total number of natural disastets",
       main = "Yearly global natural disaster (1980-2018)")
  
# Computing descriptive statistics ----
    # Mean and standard deviation
      mean(disasters$events)
      sd(disasters$events)
    
    # Histogram of the number of yearly disasters
      hist(disasters$events,
           xlab = "Count of yearly disasters",
           ylab = "Frequency of yearly disastets",
           main = "Histogram of years disastets (1980-2018")
      
# Computing Z-scores ----
      
    #Creating new variable called "z-score"
      disasters <- disasters %>% 
        mutate(zscore = (events-mean(events)/sd(events)))
      
    #Calculating the probability of experiencing 100, 200 and 300 natural disasters per year
      disasters <- disasters %>% 
        mutate(z100 = (100-mean(events)/sd(events)),
               z200 = (200-mean(events)/sd(events)),
               z300 = (300-mean(events)/sd(events)))
      
    #Calculting p-norm 
      disasters <- disasters %>% 
        mutate(pnorm100 = 100*(1-pnorm(disasters$z100)))

# Average by group: pre vs. post 2000.
      pre_2000 <- disasters %>% 
        filter(year <= 2000) %>% 
        mutate(mean_pre_2000 = mean(events))
      
      post_2000 <- disasters %>% 
        filter(year >= 2000) %>% 
        mutate(mean_post_2000 = mean(events))

      


