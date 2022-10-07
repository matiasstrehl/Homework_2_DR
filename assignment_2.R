# Homework 2 Data Wrangling
# Shuhei Kaneko
# Part 2 
library("tidyverse")

# Q1
# a)
airbnb <- read_csv("assign_2.csv")
# c)
airbnb <- airbnb %>% rename(neighborhood = neighbourhood)

# Q2
#a)
neighborhoods <- airbnb %>% count(neighborhood)
# b)
neighborhoods <- neighborhoods %>% filter(neighborhood != "NA") %>%
  arrange(desc(n)) %>% head(20)
#c)
airbnb_top_neighborhoods <- airbnb %>% filter(neighborhood %in% neighborhoods$neighborhood)
#d)
summary_stats_top_neighborhoods <- airbnb_top_neighborhoods %>% group_by(neighborhood) %>%
  summarize(avg_square_feet = mean(square_feet, na.rm = TRUE), avg_price = mean(price, na.rm = TRUE),
            sd_price = sd(price, na.rm = TRUE), max_price = max(price, na.rm = TRUE), min_price = min(price, na.rm = TRUE)) %>%
  arrange(desc(avg_square_feet))
# e)
highest_avg_square_ft <- as.numeric(summary_stats_top_neighborhoods[1, "avg_square_feet"])
#f)
second_avg_price <- as.numeric((summary_stats_top_neighborhoods %>% arrange(desc(avg_price)))[2, "avg_price"])

