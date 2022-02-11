##### Homework week 4 ####
### Area Studies - Cora ###

# Calling packages and options ----
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Calling dataset ----
development <- read.csv("Data/Dataset4.csv")

# Selecting continents -----
development <- development %>% 
  mutate(Continent = case_when(Country == "Brazil" | Country == "Argentina" |
                                 Country == "Colombia" | Country == "Venezuela" |
                                 Country == "Peru" | Country == "Paraguay" | Country == "Uruguay" |
                                 Country == "Chile" | Country == "Bolivia" | Country == "Equador" |
                                 Country == "Suriname" | Country == "French Guiana" | Country == "Guiana"~ "South America",
                               Country == "Albania" | Country == "Andorra" | Country == "Armenia" |Country == "Austria" |
                                 Country == "Azerbaijan" | Country == "Belarus" | Country == "Belgium" |
                                 Country == "Bosnia and Herzegovina" | Country == "Bulgaria" | Country == "Croatia" | Country == "Cyprus" |
                                 Country == "Czechia" | Country == "Denmark" | Country == "Estonia" | Country == "Finland" | Country == "France" |
                                 Country == "Georgia" | Country == "Germany" |Country == "Greece" | Country == "Hungary" |
                                 Country ==  "Iceland" | Country == "Ireland" | Country == "Italy" | Country == "Kazakhstan" |
                                 Country == "Kosovo" | Country == "Latvia"  | Country == "Liechtenstein" |
                                 Country == "Lithuania" |Country == "Luxembourg" | Country == "Malta" | Country == "Moldova" | Country == "Monaco" |
                                 Country == "Montenegro" | Country == "Netherlands" | Country == "North Macedonia" |
                                 Country == "Norway" | Country == "Poland" | Country == "Portugal" | Country == "Romania" | Country == "Russia" |
                                 Country == "San Marino" | Country == "Serbia" | Country == "Slovakia" | Country == "Slovenia" | Country == "Spain" | Country == "Sweden" |
                                 Country == "Switzerland" | Country == "Turkey" | Country == "Ukraine" |
                                 Country == "United Kingdom" | Country == "Vatican City" ~ "Europe",
                               TRUE ~ "Rest of the world"))


# Plot -----
  development %>%
  group_by(Continent, Year) %>% 
  mutate(mean_country_year = mean(percFossil, na.rm = T)) %>% 
    ggplot( aes(x=Year, y= mean_country_year, group=Continent, color=Continent)) +
    geom_line() +
    ggtitle("Fossil usage comparision (1990-2015)") +
    ylab("Mean percentage fossil fuel usage") + xlab("Years")+
    theme_bw()

# Selecting years -----
  ano_1995<- development %>% 
    filter(Year == 1995) %>% 
    mutate(average_year_consu = mean(percFossil, na.rm = T))
  
  t.test(ano_1995$average_year_consu, mu = 76)
  
  
  
# Test t -----
  ano_13 <- development %>% 
    filter(Year == 2013) %>% 
    mutate(average_year_consu = mean(percFossil, na.rm = T))
  
  t.test(ano_13$average_year_consu, mu=76)
  

  
  
  
  