
# Setup -------------------------------------------------------------------
library("ggplot2") 
library(dplyr)
library(tidyverse)

all_data <- read.csv("incarceration_trends.csv")


# Introduction + Summary --------------------------------------------------

  # Include at least 5 relevant values of interest


# Trends over time chart --------------------------------------------------

# compare prison population rates in: 
  # Asian American/Pacific Islander, Black, Latinx, Native American, White


prison_trends <- all_data %>% 
  select(year,aapi_prison_pop_rate, black_prison_pop_rate, latinx_prison_pop_rate, native_prison_pop_rate, white_prison_pop_rate) %>% 
  drop_na()

ggplot(prison_trends, aes(x=year)) + 
  geom_smooth(aes(y= aapi_prison_pop_rate, color = "Asian American/Pacific Islander")) + 
  geom_smooth(aes(y= black_prison_pop_rate, color = "Black")) +
  geom_smooth(aes(y= latinx_prison_pop_rate, color = "Latinx")) +
  geom_smooth(aes(y= native_prison_pop_rate, color = "Native American")) +
  geom_smooth(aes(y= white_prison_pop_rate, color = "White")) +
  ggtitle("Prison Population Rates per Year, Categorized by Race") + 
  labs(y= "Population Rate", x = "Year")+
  labs(colour= "Race")
  


# Variable Comparison Chart -----------------------------------------------

# compare total prison population to total black prison population
  # total_prison_pop
  # black_prison_pop

prison_comp <- all_data %>% 
  select(year, total_prison_pop_rate, black_prison_pop_rate)
  drop_na()
  
ggplot(prison_comp, aes(x=year))+
  xlim(1990, NA)+
  geom_smooth(aes(y= total_prison_pop_rate, color = "Total Prison Population Rate")) + 
  geom_smooth(aes(y= black_prison_pop_rate, color = "Black Prison Population Rate"))+
  ggtitle("Rate of Black Population Imprisoned Compared to Total Rate of Imprisonment ") + 
  labs(y= "Population Rate", x = "Year")+
  labs(colour= "Group Rates")
  

# Map ---------------------------------------------------------------------

  

  