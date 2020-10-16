library(tidyverse)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(tidytext)
library(wordcloud2)
library(stringr)
library(textdata)

text <- readLines(file.choose())
text

# In order to turn it into a tidy text dataset, we first need to put it into a data frame.

length = length(text)
text_df <- tibble(line = 1:length, text = text)
text_df

# Create the tidy text object
tidy_df <- text_df %>%
  unnest_tokens(word, text)
tidy_df

# Remove the stop words
tidy_df_rm <- tidy_df %>%
  anti_join(stop_words)
tidy_df_rm

# Count the most frequent words
tidy_df_rm %>%
  count(word, sort = TRUE) 

# Activity: Tidy Text Format

text <- readLines(file.choose())
text

length = length(text)
text_df <- tibble(line = 1:length, text = text)
text_df

tidy_df <- text_df %>%
  unnest_tokens(word, text)
tidy_df

tidy_df_rm <- tidy_df %>%
  anti_join(stop_words)
tidy_df

tidy_df_rm %>%
  count(word, sort = TRUE) 

# Word Cloud

tidy_df_rm %>%
  count(word, sort=TRUE) %>%
  wordcloud2(word,size=2)

# Activity: Word Cloud

text <- readLines(file.choose())
text

length = length(text)
text_df <- tibble(line = 1:length, text = text)
text_df

tidy_df <- text_df %>%
  unnest_tokens(word, text)
tidy_df

tidy_df_rm <- tidy_df %>%
  anti_join(stop_words)
tidy_df

tidy_df_rm %>%
  count(word, sort=TRUE) %>%
  wordcloud2(word,size=2)

# Sentiment Analysis
nrc_sentiment <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")

tidy_df_rm %>%
  inner_join(nrc_sentiment) %>%
  count(word, sort = TRUE)

# Sentiment Analysis

text <- readLines(file.choose())
text

length = length(text)
text_df <- tibble(line = 1:length, text = text)
text_df

tidy_df <- text_df %>%
  unnest_tokens(word, text)
tidy_df

tidy_df_rm <- tidy_df %>%
  anti_join(stop_words)
tidy_df

tidy_df_rm %>%
  inner_join(nrc_sentiment) %>%
  count(word, sort = TRUE)
