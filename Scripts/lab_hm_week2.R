##### Homework week 2 ####
### Area Studies - Cora ###

# Calling packages and options ----
  library(readr)
  library(tidyverse)
  library(dplyr)
  library(ggplot2)

# Calling dataset ----
  fertility_literacy <- read.csv("Data/fertility_literacy.csv")

# Descriptive statistics: literacy -----
  #Boxplot
    boxplot(Literacy~Year,data=fertility_literacy, main="Literacy rate around the world (1980-2019)",
          xlab="Years", ylab="Literacy rate")
  
  # Mean, Median and Standard deviation (1985, 1999, and 2016)
    fertility_literacy %>%
      filter(Year == "1985") %>% 
      mutate(mean_85 = mean(Fertility, na.rm = T),
             median_85 = median(Fertility, na.rm = T),
             sd_85 = sd(Fertility, na.rm = T))
    
    fertility_literacy %>%
      filter(Year == "1999") %>% 
      mutate(mean_99 = mean(Fertility, na.rm = T),
             median_99 = median(Fertility, na.rm = T),
             sd_99 = sd(Fertility, na.rm = T))
    
    fertility_literacy %>%
      filter(Year == "2016") %>% 
      mutate(mean_16 = mean(Fertility, na.rm = T),
             median_16 = median(Fertility, na.rm = T),
             sd_16 = sd(Fertility, na.rm = T))
    
# Descriptive statistics: Fertility -----
  
    #Fertility over time: Brazil and Spain (1980-2018)
      #subsetting dataset
        br_sp <- fertility_literacy %>% 
        filter(CountryName == "Brazil" | CountryName == "Spain")
    
      # Plot
        br_sp %>%
          ggplot( aes(x=Year, y=Fertility, group=CountryName, color=CountryName)) +
          geom_line(linetype = "dashed") +
        ggtitle("Fertility rate in Brazil and Spain (1980-2019)") +
        ylab("Fertility rate") + xlab("Years")+
          theme_bw()
        
      # Z-scores of fertility for every country in 1994.
        fertility_literacy <- fertility_literacy %>% 
          filter(Year == "1994") %>% 
          mutate(zscore1994 = (Fertility - mean(Fertility, na.rm = T)/sd(Fertility, na.rm = T)))
        
        #Histogram
          hist(fertility_literacy$zscore1994)
        
# Bonus question -----
    #Computing the probability of a woman having 2 or 3 children per year (2012 and 2019)
      fertility_2012 <- fertility_literacy %>% 
            filter(Year == "2012") %>% 
            mutate(z2 = ((2-mean(Fertility, na.rm = T)/sd(Fertility, na.rm = T))),
                   z3 = (3-mean(Fertility, na.rm = T)/sd(Fertility, na.rm = T)))

      fertility_2019 <- fertility_literacy %>% 
            filter(Year == "2019") %>% 
            mutate(z2 = ((2-mean(Fertility, na.rm = T)/sd(Fertility, na.rm = T))),
                   z3 = (3-mean(Fertility, na.rm = T)/sd(Fertility, na.rm = T)))
  