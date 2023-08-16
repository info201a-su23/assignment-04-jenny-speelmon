library(dplyr)
library(tidyverse)

raw_prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")

raw_jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")

# prison data in 2013 with added location col
prison_2013 <- raw_prison[,c(3, 4, 5, 9, 13)] %>%
  filter(year == 2013) %>%
  mutate(location = paste(county_name, state, sep = ", "))

# average value of Black prison population in 2013 across all counties
avg_black_prison <- prison_2013 %>%
  summarize(avg_black_prison = mean(black_prison_pop, na.rm = TRUE)) %>%
  pull(avg_black_prison)

avg_black_prison_function <- function() {
  i <- avg_black_prison
  return(i)
}

# location of highest Black prison pop in 2013
location_highest_black_prison <- prison_2013 %>%
  group_by(location) %>%
  summarize(location_high = max(black_prison_pop)) %>%
  drop_na() %>%
  filter(location_high == max(location_high)) %>%
  pull(location)

location_highest_black_prison_function <- function() {
  i <- location_highest_black_prison
  return(i)
}

# value of location with highest Black prison pop in 2013
highest_black_prison <- prison_2013 %>%
  group_by(location) %>%
  summarize(location_high = max(black_prison_pop)) %>%
  drop_na() %>%
  filter(location_high == max(location_high)) %>%
  pull(location_high)

highest_black_prison_function <- function() {
  i <- highest_black_prison
  return(i)
}

# jail data in 2013 with added location col
jail_2013 <- raw_jail[,c(3, 4, 5, 9, 17)] %>%
  filter(year == 2013) %>%
  mutate(location = paste(county_name, state, sep = ", "))

# average value of Black jail population in 2013 across all counties
avg_black_jail <- jail_2013 %>%
  summarize(avg_black_jail = mean(black_jail_pop, na.rm = TRUE)) %>%
  pull(avg_black_jail)

avg_black_jail_function <- function() {
  i <- avg_black_jail
  return(i)
}

# location of highest Black jail pop in 2013
location_highest_black_jail <- jail_2013 %>%
  group_by(location) %>%
  summarize(location_high = max(black_jail_pop)) %>%
  drop_na() %>%
  filter(location_high == max(location_high)) %>%
  pull(location)

location_highest_black_jail_function <- function() {
  i <- location_highest_black_jail
  return(i)
}

# value of location with highest Black jail pop in 2013
highest_black_jail <- jail_2013 %>%
  group_by(location) %>%
  summarize(location_high = max(black_jail_pop)) %>%
  drop_na() %>%
  filter(location_high == max(location_high)) %>%
  pull(location_high)

highest_black_jail_function <- function() {
  i <- highest_black_jail
  return(i)
}