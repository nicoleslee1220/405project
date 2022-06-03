
xpath_main <- Sys.getenv("PATH_MY_MAIN_DATA")
xpath_nlp <- file.path(xpath_main, "analysis", "nlp", "_assets")

unigram <- read.csv(file.path(xpath_nlp, "unigram.csv"), header = TRUE, sep = ",")

library(tidytext)
library(dplyr)
library(widyr)

recipe_idf <- unigram %>% pairwise_count(word, recipeID, diag = TRUE) %>%
  filter(item1 == item2) %>% select(-item2)

recipe_len <- length(unique(unigram$recipeID))

recipe_tfidf <- unigram %>% count(word, sort = TRUE) %>%
  rename(tf = n) %>% left_join(recipe_idf %>% rename(word = item1), by = "word") %>%
  rename(idf = n) %>% mutate(tf = tf/recipe_len, idf = log(recipe_len/idf), tf_idf = tf * idf) %>%
  arrange(-tf_idf)
  
  # 6513 unique words
  # log() not ln(); mean() not avg()

library(ggplot2)
ggplot(recipe_tfidf) +
  geom_text(aes(x = idf, y = tf, label = word), color = "blue")

# choose first 100 words
top100 <- recipe_tfidf[1:100,]
  # double brackets are for lists, not data frames

write.csv(top100, file.path(xpath_nlp, "top100.csv"), row.names = FALSE)
