# Load packages
library(readODS)
library(dplyr)
library(ggplot2)
library(tidyr)

# Function to calculate moving average
mav <- function(x, n = 5)
{
  moving_average <- stats::filter(x, rep(1 / n, n), sides = 1)
  moving_average[is.na(moving_average)] <- 0
  moving_average
}

# Read data
data <- read_ods("My_music_top_50_2020.ods")

# Number of unique artists
nrow(distinct(data, artist))

# Analyse artists
artists <- data %>%
  group_by(artist) %>% 
  summarise(n_songs = n()) %>% 
  arrange(desc(n_songs))

ggplot(artists, aes(x = artist, y = n_songs)) +
  geom_col() +
  coord_flip()

mean(artists$n_songs)
median(artists$n_songs)

# Analyse genres
genres <- data %>%
  group_by(genre) %>% 
  summarise(n_songs = n()) %>% 
  arrange(desc(n_songs))

ggplot(genres, aes(x = genre, y = n_songs)) +
  geom_col() +
  coord_flip()

# Analyse language
table(data$language)

# Analyse years
years <- data %>% 
  group_by(year) %>% 
  summarise(n_songs = n()) %>% 
  complete(year = c(1970:2020), fill = list(n_songs =0))

ggplot(years, aes(x = year, y = n_songs)) +
  geom_line()

# With weights
data$weight = 1 / (1 + exp(0.15 * (data$rank - 25)))

qplot(x = data$rank, y = data$weight, geom = "line")

data %>% 
  group_by(artist) %>%
  summarise(index = sum(weight)) %>% 
  arrange(-index)

data %>% 
  group_by(year) %>%
  summarise(index = sum(weight)) %>% 
  complete(year = 1970:2020, fill = list(index = 0)) %>% 
  ggplot(aes(x = year, y = mav(index, 5))) +
  geom_line()
