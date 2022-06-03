
library(rjson)
library(dplyr)

xpath_main <- Sys.getenv("PATH_MY_MAIN_DATA")
xpath_nlp <- file.path(xpath_main, "analysis", "nlp", "_assets")
corp_df <- as_tibble(read.csv(file.path(xpath_nlp, "corp_df.csv"), header = TRUE, sep = ","))
  # read.csv automatically converts character integers to integer type
  # dplyr recognizes column names
    # ~: defines a function; ".": piped-in placeholder

library(tokenizers)

text_to_ngram <- function(df, num){
  temp_df <- data.frame(recipeID = NULL, ngram = NULL) 
  for (i in 1:length(df$instructions)){
    recipe <- df$recipeID[i]
    instruct <- df$instructions[i]
    ngrams <- unlist(tokenize_ngrams(instruct, n = num))
    len <- length(ngrams)
    temp_df <- rbind.data.frame(temp_df, data.frame(recipeID = rep(recipe, len), ngram = ngrams))
  }
  return(temp_df)
}

library(tidytext)

ngrams <- corp_df %>% text_to_ngram(8) %>% mutate(ngramID = row_number()) %>%
  tidyr::unite(skipgramID, recipeID, ngramID) %>% unnest_tokens(word, ngram) %>% 
  filter(word != "spin" & word != "dry")

text_to_word <- function(df){
  temp_df <- data.frame(recipeID = NULL, word = NULL) 
  for (i in 1:length(df$instructions)){
    recipe <- df$recipeID[i]
    instruct <- df$instructions[i]
    words <- unlist(strsplit(instruct, " "))
    len <- length(words)
    temp_df <- rbind.data.frame(temp_df, data.frame(recipeID = rep(recipe, len), word = words))
  }
  return(temp_df)
}

unigram <- corp_df %>% text_to_word()
write.csv(unigram, file.path(xpath_nlp, "unigram.csv"), row.names = FALSE)

unigram_prob <- corp_df %>% text_to_word() %>%
  filter(word != "spin" & word != "dry") %>% count(word, sort = TRUE) %>%
  mutate(p = n/sum(n))

library(widyr)

ngram_prob <- ngrams %>% pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n/sum(n))

ratio_prob <- ngram_prob %>% filter(n > 20) %>% 
  left_join(unigram_prob %>% select(item1 = word, p1 = p), by = "item1") %>%
  left_join(unigram_prob %>% select(item2 = word, p2 = p), by = "item2") %>%
  mutate(p_ratio = p/p1/p2) %>% 
  arrange(-p_ratio)
  # select() can rename columns

# ratio_prob %>% filter(item1 == "spin") %>% arrange(-p_ratio)

sparse_mat <- ratio_prob %>% mutate(pmi = log10(p_ratio)) %>% 
  cast_sparse(item1, item2, pmi)

library(irlba)

# svd 
sparse_mat@x[is.na(sparse_mat@x)] <- 0
sparse_svd <- svd(sparse_mat, 256)
wordProb_df <- sparse_svd$u
rownames(wordProb_df) <- rownames(sparse_mat)
  # attributes(sparse_mat) does not have rownnames

# evaluate similar words by cosine similarity
synonym <- function(selectedWord_vec){
  similarity <- wordProb_df %*% selectedWord_vec %>% data.frame(similarity = .) %>% 
    arrange(-similarity) %>% head(10)
}
  # matrix multiplication: rownames of first and columnnames of second are kept

(synonym(wordProb_df["orange",]))

output <- wordProb_df["orange", ] - wordProb_df["zest", ] + wordProb_df["banana", ] 
(synonym(output))
  # doesn't work?

# svd to 2 dimensions 
twodim_svd <- svd(sparse_mat, 2)
twodim_df <- as.data.frame(twodim_svd$u)
twodim_df$word <- rownames(sparse_mat)

write.csv(twodim_df, file.path(xpath_nlp, "word2d_df.csv"), row.names = FALSE)

library(ggplot2)
ggplot(twodim_df) +
  geom_text(aes(x = V1, y = V2, label = word), color = "blue", size = 3) +
  coord_cartesian(xlim = c(-0.025, 0.025), ylim = c(-0.025, 0.025)) + 
  labs(title = "2D SVD")


