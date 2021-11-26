##########   DATA PROCESSING    ############
library(ggplot2)
library(tidyverse)
library(lubridate)

# read full year data
data <- read.csv("data/Sales_Full_2019.csv")

# get product prices
prices <- data %>% 
  distinct(Product, Price.Each) %>% 
  arrange(Product)

# aggregate sales by week
salesbyweek.long <- data %>% 
  group_by(Product, Week.Num) %>% 
  summarise(sales = sum(Quantity.Ordered))

salesbyweek <- pivot_wider(salesbyweek.long, names_from = Week.Num, values_from = sales) %>% 
  arrange(Product)

# save files
write.csv(prices, "data/prices.csv")
write.csv(salesbyweek, "data/salesByWeek.csv")
