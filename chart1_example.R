library(ggplot2)
library(dplyr)
library(tidyverse)

raw_prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")

# prison data in 2013 with added location col
prison_2013 <- raw_prison[,c(3, 4, 5, 9, 13)] %>%
  filter(year == 2013) %>%
  mutate(location = paste(county_name, state, sep = ", "))

# list of top 10 values of locations with highest Black prison pop in 2013
top_ten_highest <- prison_2013[,c(5, 6)] %>%
  drop_na() %>%
  arrange(desc(black_prison_pop)) %>%
  slice_head(n = 10)

# prison data from 2003 to 2013 with added location col
prison_ten_years <- raw_prison[,c(3, 4, 5, 9, 13)] %>%
  filter(year >= 2003 & year <= 2013) %>%
  mutate(location = paste(county_name, state, sep = ", ")) %>%
  filter(location == "Cook County, IL" | location == "Los Angeles County, CA" 
         | location == "Harris County, TX" | location == "New York County, NY"
         | location == "Philadelphia County, PA" | location == "Wayne County, MI"
         | location == "Dallas County, TX" | location == "Baltimore city, MD"
         | location == "Cuyahoga County, OH" | location == "Milwaukee County, WI") %>%
  drop_na()

line_graph <- ggplot(prison_ten_years, aes(x = year, y = black_prison_pop, color = location)) +
  geom_line() +
  labs(title = "Black Prison Population of the Top Ten Cities by 2013, 2003-2013",
       x = "Year",
       y = "Black Prison Population",
       color = "Location") +
  theme_minimal()
line_graph + scale_x_continuous(breaks = seq(2003, 2013, by = 1))
#line_graph + scale_y_continuous(breaks = seq(5000, 30000, by = 2500))