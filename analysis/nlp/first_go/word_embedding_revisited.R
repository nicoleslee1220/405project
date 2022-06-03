
xpath_main <- Sys.getenv("PATH_MY_MAIN_DATA")
xpath_nlp <- file.path(xpath_main, "analysis", "nlp", "_assets")

word2d_df <- read.csv(file.path(xpath_nlp, "word2d_df.csv"), header = TRUE, sep = ",")
top100 <- read.csv(file.path(xpath_nlp, "top100.csv"), header = TRUE, sep = ",")

library(dplyr)

word2d_100 <- word2d_df %>% filter(word %in% top100$word)

library(ggplot2)

ggplot(word2d_100) +
  geom_text(aes(x = V1, y = V2, label = word), col = "blue") +
  coord_cartesian(ylim = c(-0.05, 0.01))
  # this plot shows the closer words are, the more often they occur next to each other in a sentence