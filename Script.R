# Load packages
library(readODS)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tm)
library(wordcloud2)

# Set random number generator
set.seed(42)

# Function to calculate moving average; used for years_plot
mav <- function(x, n = 5)
{
  moving_average <- stats::filter(x, rep(1 / n, n), sides = 1)
  moving_average[is.na(moving_average)] <- 0
  moving_average
}

# Make and set theme for all the plots
plots_theme <- theme_bw(base_size = 16, base_family = "PT Sans") +
  theme(line = element_line(size = 1, color ="#333333"),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
theme_set(plots_theme)
plots_color <- "#800094"

# Read data
data <- read_ods("My_music_top_50_2020.ods")

# Number of unique artists
n_artists <- nrow(distinct(data, artist))
songs_per_artist <- nrow(data) / n_artists


# Analysis without weights

# Artists
artists_no_weights <- data %>%
  group_by(artist) %>% 
  summarise(n_songs = n()) %>% 
  arrange(-n_songs)

# Genres
genres_no_weights <- data %>%
  group_by(genre) %>% 
  summarise(n_songs = n()) %>% 
  arrange(-n_songs)

# Languages
languages_no_weights <- table(data$language)

# Years
years_no_weights <- data %>% 
  group_by(year) %>% 
  summarise(n_songs = n()) %>% 
  complete(year = c(1970:2020), fill = list(n_songs = 0))


# Analyse with weights
# First of all, set weights
# Weighting function and its parameters were selected manually by the method of “trial and error“
data$weight = 1 / (1 + exp(0.15 * (data$rank - 25)))

# Visualize weights
weights_plot <- ggplot(data, aes(x = rank, y = weight)) +
  geom_line(color = plots_color, size = 3) +
  scale_x_continuous(breaks = c(1, 25, 50), labels = c(1, 25, 50), name = "Номер песни") +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0,5", "1"), name = "Вес") +
  ggplot2::annotate(geom = "text", x = 1, y = 0.95, label = "Lindemann — Allesfresser", angle = 90, hjust = 1) +
  ggplot2::annotate(geom = "text", x = 25, y = 0.45, label = "Ленинград — Жу-жу", angle = 90, hjust = 1) +
  ggplot2::annotate(geom = "text", x = 50, y = 0.05, label = "Red Hot Chili Peppers — Californication", angle = 90, hjust = 0) +
  theme(panel.grid = element_blank(),
        axis.line = element_line())
ggsave("weights_plot.png", weights_plot, width = unit(8, "cm"), height = unit(5, "cm"), dpi = 150)

# Artists
artists <- data %>% 
  group_by(artist) %>%
  summarise(index = sum(weight)) %>% 
  arrange(-index)

artists_plot <- ggplot(artists, aes(x = reorder(artist, index), y = index)) +
  geom_col() +
  coord_flip()

# Genres
genres <- data %>% 
  group_by(genre) %>% 
  summarise(index = sum(weight)) %>% 
  arrange(-index)

genres_plot <- ggplot(genres, aes(x = reorder(genre, index), y = index)) +
  geom_col() +
  coord_flip()

# Languages
languages <- data %>% 
  group_by(language) %>% 
  summarise(index = sum(weight)) %>% 
  arrange(-index)

languages_plot <- ggplot(languages, aes(x = reorder(language, index), y = index)) +
  geom_col(fill = plots_color) +
  scale_y_continuous(expand = expansion(add = c(0, 2))) +
  labs(x = "Язык текста песни", y = "Индекс") +
  theme(panel.grid.major.x = element_blank())
ggsave("languages_plot.png", languages_plot, width = unit(8, "cm"), height = unit(5, "cm"), dpi = 150)

# Years
years <- data %>% 
  group_by(year) %>%
  summarise(index = sum(weight)) %>% 
  complete(year = 1970:2020, fill = list(index = 0)) %>% 
  mutate(year = as.numeric(year))

years_plot <- ggplot(years, aes(x = year)) +
  geom_line(aes(y = mav(index, 5))) +
  geom_line(aes(y = index), alpha = .5)


# Analysis of songs texts
# Select only songs in English and load them as a corpora
songs_texts <- Corpus(DataframeSource(select(filter(data, language == "Английский"), doc_id = song, text)))

# Preprocess texts
songs_texts <- tm_map(songs_texts, content_transformer(tolower))
songs_texts <- tm_map(songs_texts, removeWords, stopwords())
songs_texts <- tm_map(songs_texts, stemDocument)
songs_texts <- tm_map(songs_texts, removePunctuation)
songs_texts <- tm_map(songs_texts, stripWhitespace)
songs_texts <- tm_map(songs_texts, removeWords, c("ill", "ive", "’s", "dont"))

# Build data frame of terms with frequences for a wordcloud
tdm <- TermDocumentMatrix(songs_texts)
tdm <- as.matrix(tdm)
tdm <- sort(rowSums(tdm), decreasing = TRUE)
words_freq <- data.frame(word = names(tdm), freq = tdm)

# Manual cleanup to remove strange "’s" and to restore two words after stemming
# Necessary for a better wordcloud
words_freq <- words_freq[words_freq$word != "’s" & words_freq$word != "’m", ]
words_freq[words_freq$word == "caus", "word"] <- "cause"
words_freq[words_freq$word == "unstopp", "word"] <- "unstoppable"
words_freq[words_freq$word == "babi", "word"] <- "baby"
words_freq[words_freq$word == "morn", "word"] <- "morning"
words_freq[words_freq$word == "californ", "word"] <- "California"

# Draw a wordcloud of the first 50 words
word_cloud <- wordcloud2(words_freq[1:50,], size = 0.5)

# Terms by tf-idf
tdm_tf_idf <- TermDocumentMatrix(songs_texts, control = list(weighting = weightTfIdf))
tdm_tf_idf <- as.matrix(tdm_tf_idf)
tdm_tf_idf <- sort(rowSums(tdm_tf_idf), decreasing = TRUE)
words_tf_idf <- data.frame(word = names(tdm_tf_idf), freq = tdm_tf_idf)
