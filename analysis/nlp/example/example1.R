options(timeout = 1000000)
getOption("timeout")
# download.file("https://snap.stanford.edu/data/finefoods.txt.gz", "finefoods.txt.gz")

reviews <- read_lines("finefoods.txt.gz") 
reviews <- reviews[str_sub(reviews, 1, 12) == "review/text:"]
  # str_sub extracts strings starting at position index and at ending index 
reviews <- str_sub(reviews, start = 14)
  # start at actual string; account for space after :
reviews <- iconv(reviews, to = "UTF-8")

head(reviews, 2)

library(keras)
tokenizer <- text_tokenizer(num_words = 20000)
tokenizer %>% fit_text_tokenizer(reviews)