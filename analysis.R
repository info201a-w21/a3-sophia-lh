
# Setup -------------------------------------------------------------------
library("usdata")
library("ggplot2") 
library(dplyr)
library(tidyverse)
library(maps)
library(knitr)

all_data <- read.csv("incarceration_trends.csv")


# Introduction + Summary --------------------------------------------------
  # What state is the average black prison population rate the highest in?
    highest_state <- all_data %>% 
      group_by(state) %>% 
      summarize(avg_black_rate = mean(black_prison_pop_rate, na.rm = T)) %>% 
      filter(avg_black_rate == max(avg_black_rate, na.rm =T)) %>% 
      pull(state)
  
  # What county is the average black prison population rate the highest in?
    highest_county <- all_data %>% 
      group_by(county_name) %>% 
      summarize(avg_black_rate = mean(black_prison_pop_rate, na.rm = T)) %>% 
      filter(avg_black_rate == max(avg_black_rate, na.rm =T)) %>% 
      pull(county_name)

  # What state is the average black prison population rate the lowest in?
    lowest_state <- all_data %>% 
      group_by(state) %>% 
      summarize(avg_black_rate = mean(black_prison_pop_rate, na.rm = T)) %>% 
      filter(avg_black_rate == min(avg_black_rate, na.rm =T)) %>% 
      pull(state)
    
  # What is the average value of the black prison population across all states?
    avg_rate <- all_data %>% 
      #group_by(state) %>% 
      summarize(avg_black_rate = mean(black_prison_pop_rate, na.rm = T)) %>% 
      pull(sum(avg_black_rate)/length(avg_black_rate))
    
  # How much has the average black prison population rate changed over the last 20 years?
    avg_rate_1996 <- all_data %>% 
      filter(year == 1996) %>% 
      summarize(avg_black_rate = mean(black_prison_pop_rate, na.rm = T)) %>% 
      pull(sum(avg_black_rate)/length(avg_black_rate))
    
    avg_rate_2016 <- all_data %>% 
      filter(year == 2016) %>% 
      summarize(avg_black_rate = mean(black_prison_pop_rate, na.rm = T)) %>% 
      pull(sum(avg_black_rate)/length(avg_black_rate))
    
  change_rate <- avg_rate_2016 - avg_rate_1996
    
# Trends over time chart --------------------------------------------------

# compare prison population rates in: 
  # Asian American/Pacific Islander, Black, Latinx, Native American, White


prison_trends <- all_data %>% 
  select(year,aapi_prison_pop_rate, black_prison_pop_rate, latinx_prison_pop_rate, native_prison_pop_rate, white_prison_pop_rate) 

prison_trends <- drop_na(prison_trends)
  

  trends <- ggplot(prison_trends, aes(x=year)) + 
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

prison_comp <- drop_na(prison_comp)
  
comp <- ggplot(prison_comp, aes(x=year))+
  xlim(1990, NA)+
  geom_smooth(aes(y= total_prison_pop_rate, color = "Total Prison Population Rate")) + 
  geom_smooth(aes(y= black_prison_pop_rate, color = "Black Prison Population Rate"))+
  ggtitle("Rate of Black Population Imprisoned Compared to Total Rate of Imprisonment ") + 
  labs(y= "Population Rate", x = "Year")+
  labs(colour= "Group Rates")
  

# Map ---------------------------------------------------------------------
shapefile <- map_data("state")

# compare AVERAGE black prison population rate across states

state_data <- all_data %>% 
  #filter(year == max(year)) %>% 
  group_by(state) %>% 
  summarize(avg_black_rate = mean(black_prison_pop_rate, na.rm = T))


shapefile <- shapefile %>% 
  mutate(state = state2abbr(region)) %>% 
  left_join(state_data, by = "state")
  
  
map <- shapefile %>% 
  ggplot(aes(long, lat, group = group, fill = avg_black_rate)) +
  geom_polygon(color = NA) +
  labs(fill = "Average Rate")+
  ggtitle("Average Rates of Black Population Imprisoned by State")+
  theme_minimal()


  