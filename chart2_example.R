library(ggplot2)
library(dplyr)
library(tidyverse)
library(gridExtra)

raw_prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")

raw_jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")

# prison data during 1970-2018 in Cook County, IL
prison_over_years <- raw_prison[,c(3, 4, 5, 9, 13)] %>%
  filter(year >= 1970 & year <= 2018) %>%
  mutate(location = paste(county_name, state, sep = ", ")) %>%
  filter(location == "Cook County, IL") %>%
  drop_na()

# jail data during 1970-2018 in Cook County, IL
jail_over_years <- raw_jail[,c(3, 4, 5, 9, 17)] %>%
  filter(year >= 1970 & year <= 2018) %>%
  mutate(location = paste(county_name, state, sep = ", ")) %>%
  filter(location == "Cook County, IL") %>%
  drop_na()

# prison plot
prison_plot <- ggplot(prison_over_years, aes(x = black_prison_pop, y = total_prison_pop)) +
  geom_point() +
  labs(title = "Black versus Total Prison Population in Cook County, IL, 1970-2018",
       x = "Black Prison Population",
       y = "Total Prison Population") +
  theme_minimal()

# jail plot
jail_plot <- ggplot(jail_over_years, aes(x = black_jail_pop, y = total_jail_pop)) +
  geom_point() +
  labs(title = "Black versus Total Jail Population in Cook County, IL, 1970-2018",
       x = "Black Jail Population",
       y = "Total Jail Population") +
  theme_minimal()

# combine plots into one page
grid.arrange(prison_plot, arrangeGrob(jail_plot, ncol = 1), nrow = 2)
