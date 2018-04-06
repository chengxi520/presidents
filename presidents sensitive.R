library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(tidyverse)

clinton <- data_frame(president="clinton", text = read_lines("./clinton first state of the union.txt"))
obama <- data_frame(president="obama", text = read_lines("./obama first state of the union.txt"))
presidents <- rbind(clinton, obama)
tidy_presidents <- presidents %>% unnest_tokens(word, text)

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

nrc_anger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")

nrc_anticipation <- get_sentiments("nrc") %>% 
  filter(sentiment == "anticipation")

clinton_joy <- tidy_presidents %>%
  filter(president == "clinton") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

clinton_anger <- tidy_presidents %>%
  filter(president == "clinton") %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

clinton_anticipation <- tidy_presidents %>%
  filter(president == "clinton") %>%
  inner_join(nrc_anticipation) %>%
  count(word, sort = TRUE)

obama_joy <- tidy_presidents %>%
  filter(president == "obama") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

obama_anger <- tidy_presidents %>%
  filter(president == "obama") %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

clinton_anticipation <- tidy_presidents %>%
  filter(president == "clinton") %>%
  inner_join(nrc_anticipation) %>%
  count(word, sort = TRUE)

obama_anticipation <- tidy_presidents %>%
  filter(president == "obama") %>%
  inner_join(nrc_anticipation) %>%
  count(word, sort = TRUE)

president_sentiment <- tidy_presidents %>%
  inner_join(get_sentiments("bing")) %>%
  count(president, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

president_sentiment
