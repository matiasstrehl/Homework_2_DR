# Homework 2 Data Wrangling 

#-------------------------#
#   Person_1 PART 2       #
#-------------------------#

# Set up packages
library(tidyverse)
library(janitor)
library(dplyr)

# 1: setting up the data
#-----------------------#

# Part 1.a)
airbnb <- tibble(read_csv("assign_2.csv"))

# Part 1.b)
# Take a look of the data
colnames(airbnb)
View(airbnb)

# Part 1.c) Private question
airbnb <- rename(airbnb, neighborhood = neighbourhood)

# 2: Piping practice and creating summary statistics
#---------------------------------------------------#

# Part 2.a)
neighborhoods <- airbnb %>% 
                    count(neighborhood) %>%
                    tibble()

# Part 2.b)
# Remove NA from our tibble
neighborhoods <- airbnb %>% 
  count(neighborhood) %>%
  filter(!is.na(neighborhood)) %>%
  tibble()

# Arrange column in descending order and show only 20 obs.
neighborhoods <- neighborhoods %>%
  arrange(desc(n)) %>%
  head(20)

# Part 2.c)
# New tibble only with the 20 most visited neighborhoods
airbnb_top_neighborhoods <- airbnb %>%
  filter(neighborhood %in% neighborhoods$neighborhood)

# checking that the new tibble is correct  
airbnb_top_neighborhoods %>%
  distinct(neighborhood)  

# Part 2.d)
# Summary statistics
summary_stats_top_neighborhoods <- airbnb_top_neighborhoods %>%
  group_by(neighborhood) %>%
  summarize(avg_square_feet = mean(square_feet, na.rm = T),
            avg_price = mean(price, na.rm = T),
            sd_price = sd(price, na.rm = T),
            max_price = max(price, na.rm = T),
            min_price = min(price, na.rm = T)) %>%
  arrange(desc(avg_square_feet)) %>%
  tibble()

# Square footage is not the only input to price.

# Part 2.e Private question
highest_avg_square_ft <- summary_stats_top_neighborhoods %>%
  select(avg_square_feet) %>%
  slice(1) %>%
  pull(avg_square_feet)

# Part 2.f Private question
second_avg_price <- summary_stats_top_neighborhoods %>%
  select(avg_square_feet) %>%
  slice(2) %>%
  pull(avg_square_feet)

#








  






















