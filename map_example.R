library(tidyverse)
library(dplyr)
library(ggplot2)
library(maps)

raw_jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")

# jail data in 2018
jail_2018 <- raw_jail[,c(3, 4, 5, 9, 17)] %>%
  filter(year == 2018) %>%
  drop_na()

jail_2018$state <- state.name[match(jail_2018$state, state.abb)]

states_map <- map_data("state")

region <- tolower(jail_2018$state)

# states by Black jail population
df <- data.frame(region, jail_2018$black_jail_pop)

names(df) <- c("state", "black_jail_pop")

# state by average Black jail population
new_df <- aggregate(df$black_jail_pop, list(df$state), FUN = mean)

names(new_df) <- c("state", "avg_black_jail_pop")

# map
mp <- ggplot(new_df, aes(map_id = state)) +
  geom_map(aes(fill = avg_black_jail_pop), map = states_map) +
  labs(title = "Average Black Jail Population in the U.S. in 2018",
       fill = "Black Jail Population") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"))

mp <- mp + expand_limits(x = states_map$long, y = states_map$lat)

mp <- mp + scale_fill_gradient(low = 'grey90', high = 'darkgreen', limits = c(0, 300))

mp
