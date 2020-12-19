# Load packages
library(readODS)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tm)
library(wordcloud2)

set.seed(42)

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
n_artists <- nrow(distinct(data, artist))
songs_per_artist <- nrow(data) / n_artists

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
songs_in_english <- table(data$language)[1]
songs_in_german <- table(data$language)[2]
songs_in_russian <- table(data$language)[3]
songs_in_finnish <- table(data$language)[4]

# Analyse years
years <- data %>% 
  group_by(year) %>% 
  summarise(n_songs = n()) %>% 
  complete(year = c(1970:2020), fill = list(n_songs =0))

ggplot(years, aes(x = year, y = n_songs)) +
  geom_line()

# With weights
data$weight = 1 / (1 + exp(0.15 * (data$rank - 25)))

# Visualize weights to see if they are selected properly
weights_plot <- qplot(x = data$rank, y = data$weight, geom = "line")

artists_plot <- data %>% 
  group_by(artist) %>%
  summarise(index = sum(weight)) %>% 
  arrange(-index) %>% 
  ggplot(aes(x = reorder(artist, index), y = index)) +
  geom_col() +
  coord_flip()

genres_plot <- data %>% 
  group_by(genre) %>% 
  summarise(index = sum(weight)) %>%
  ggplot(aes(x = reorder(genre, index), y = index)) +
  geom_col() +
  coord_flip()

languages_plot <- data %>% 
  group_by(language) %>% 
  summarise(index = sum(weight)) %>% 
  ggplot(aes(x = reorder(language, index), y = index)) +
  geom_col()

years_plot <- data %>% 
  group_by(year) %>%
  summarise(index = sum(weight)) %>% 
  complete(year = 1970:2020, fill = list(index = 0)) %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = mav(index, 5))) +
  geom_line(aes(y = index), alpha = .5)

# Text analysis
songs_texts <- Corpus(DataframeSource(select(filter(data, language == "Английский"), doc_id = song, text)))

songs_texts <- tm_map(songs_texts, content_transformer(tolower))

songs_texts <- tm_map(songs_texts, removeWords, stopwords())

songs_texts <- tm_map(songs_texts, removePunctuation)

songs_texts <- tm_map(songs_texts, stripWhitespace)

songs_texts <- tm_map(songs_texts, stemDocument)

dtm <- TermDocumentMatrix(songs_texts, control = list(weighting = weightTf))
dtm <- as.matrix(dtm)
dtm <- sort(rowSums(dtm), decreasing = TRUE)
dtm <- data.frame(word = names(dtm), freq = dtm)

word_cloud <- wordcloud2(dtm[1:30,], size = 0.5)

songs_texts_ge <- Corpus(DataframeSource(select(filter(data, language == "Немецкий"), doc_id = song, text)))

songs_texts_ge <- tm_map(songs_texts_ge, content_transformer(tolower))

songs_texts_ge <- tm_map(songs_texts_ge, removeWords, stopwords("german"))

songs_texts_ge <- tm_map(songs_texts_ge, removePunctuation)

songs_texts_ge <- tm_map(songs_texts_ge, stripWhitespace)

songs_texts_ge <- tm_map(songs_texts_ge, stemDocument)

dtm_ge <- TermDocumentMatrix(songs_texts_ge, control = list(weighting = weightTf))
dtm_ge <- as.matrix(dtm_ge)
dtm_ge <- sort(rowSums(dtm_ge), decreasing = TRUE)
dtm_ge <- data.frame(word = names(dtm_ge), freq = dtm_ge)

songs_texts_ru <- Corpus(DataframeSource(select(filter(data, language == "Русский"), doc_id = song, text)))

songs_texts_ru <- tm_map(songs_texts_ru, content_transformer(tolower))

songs_texts_ru <- tm_map(songs_texts_ru, removeWords, stopwords("ru"))

songs_texts_ru <- tm_map(songs_texts_ru, removePunctuation)

songs_texts_ru <- tm_map(songs_texts_ru, stripWhitespace)

dtm_ru <- TermDocumentMatrix(songs_texts_ru, control = list(weighting = weightTf))
dtm_ru <- as.matrix(dtm_ru)
dtm_ru <- sort(rowSums(dtm_ru), decreasing = TRUE)
dtm_ru <- data.frame(word = names(dtm_ru), freq = dtm_ru)

