##### Lab week 3 - sampling and confidence intervals ####
### Area Studies - TA: Cora ###

# Calling packages and options ----
library(readr)
library(tidyverse)

# Calling dataset ----
primary <- read.csv("Data/primary-2.csv")

# Creating a random sample of size 60 from the population of London primary schools ----
  samp <- sample(x = primary$SpendPerStudent, size = 60)

  # Describing the distribution of sample
    summary(samp)
    hist(samp)

# Mean of sample ----
    sample_mean <- mean(samp)
    
# Confidence intervals -----
    se_sample <- sd(samp) / sqrt(length(samp)) #First, lets calculate the standard error for sample
    lower_ci_sample <-sample_mean - 2.04*se_sample #computing the lower Confidence Interval for the sample
    upper_ci_sample <-sample_mean + 2.04*se_sample #computing the upper confidence interval for the sample
    combined_ci <- c(lower_ci_sample, upper_ci_sample) #then, we can combine both CIs using concatenate function
    
# Computing mean spend per student for the population of london primary schools
    mean_pop <- mean(primary$SpendPerStudent)
    
    

    