library(rtweet)
library(igraph)
library(dplyr)
library(tidytext)
library(tm)
library(tidyr)
library(ggraph)
library(tidycensus)
library(ggplot2)
library(RPostgres)
library(RColorBrewer)
library(DBI)
library(rccmisc)
library(here)

sf::sf_use_s2(TRUE)

tornadoesTweetsByHour <- ts_data(tornadoes, by="hours")
ts_plot(tornadoes, by="hours")
ts_plot(tornado, by="hours")

tornadoesText = tornadoes %>% select(text) %>% plain_tweets()
tornadoesWords = tornadoesText %>% unnest_tokens(word, text)
count(tornadoesWords)
stop_words = stop_words %>%
  add_row(word="t.co",lexicon = "SMART") %>%
  add_row(word="tornado",lexicon = "Search")
tornadoesWords =  tornadoesWords %>% anti_join(stop_words)

tornadoesWords %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Unique words",
       y = "Count",
       title = "Count of unique words found in tweets")

tornadoesWordPairs = tornadoesText %>%
  mutate(text = removeWords(tolower(text), stop_words$word)) %>%
  unnest_tokens(paired_words, text, token = "ngrams", n = 2) %>%
  separate(paired_words, c("word1", "word2"),sep=" ") %>%
  count(word1, word2, sort=TRUE)

tornadoesWordPairs %>%
  filter(n >= 100 & !is.na(word1) & !is.na(word2)) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network of Tweets about Tornadoes",
       x = "", y = "") +
  theme_void()

