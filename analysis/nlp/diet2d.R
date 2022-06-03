
# setup
xpath_main <- Sys.getenv("PATH_MY_MAIN_DATA")
xpath_nlp <- file.path(xpath_main, "analysis", "nlp", "_assets")

library(RMySQL)
drv <- dbDriver("MySQL")

xdbsock <- ""
xdbuser <- Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_USER")
xpw     <- Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_PW")
xdbname <- Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_DBNAME")
xdbhost <- Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_HOST")
xdbport <- as.integer( Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_PORT") )

con <- dbConnect(drv = drv, user = xdbuser, password = xpw, dbname = xdbname, 
                 host = xdbhost, port = xdbport, sock = xdbsock)
clean_df <- dbGetQuery(con, "SELECT * FROM spoonacular_clean")

library(dplyr)
vegan_df <- clean_df %>% filter(vegan == "TRUE")
glutenFree_df <- clean_df %>% filter(glutenFree == "TRUE")
dairyFree_df <- clean_df %>% filter(dairyFree == "TRUE")
reg_df <- clean_df %>% filter(vegan == "FALSE" & glutenFree == "FALSE" & dairyFree == "FALSE")
  # . places the piped object to desired location
  # with(clean_df, which(vegan == "FALSE" & glutenFree == "FALSE" & dairyFree == "FALSE")) %>% length()
    # "FALSE": class character; FALSE: class logical

library(SnowballC)
library(tm)
library(textstem) 
library(tokenizers)
library(tidytext)
library(widyr)
library(irlba)

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

diet2d <- function(diet_df, rmWords_vec){
  # text preprocessing 
  corp <- Corpus(VectorSource(diet_df$instructions))
  corp_clean <- tm_map(corp, removeNumbers)
  corp_clean <- tm_map(corp_clean, removePunctuation)
  corp_clean <- tm_map(corp_clean, stripWhitespace)
  corp_clean <- tm_map(corp_clean, content_transformer(tolower))
  corp_clean <- tm_map(corp_clean, removeWords, stopwords('english'))
  corp_clean <- tm_map(corp_clean, lemmatize_strings)
  
  corp_vec <- unlist(unlist(corp_clean, recursive = FALSE))
  corp_vec <- corp_vec[-length(corp_vec)]
  corp_df <- data.frame(recipeID = as.character(diet_df$DT), instructions = corp_vec)
  
  # 2d svd
  ngrams <- corp_df %>% text_to_ngram(8) %>% mutate(ngramID = row_number()) %>%
    tidyr::unite(skipgramID, recipeID, ngramID) %>% unnest_tokens(word, ngram) %>% 
    filter(!(word %in% rmWords_vec))
  unigram <- corp_df %>% text_to_word()
  
  ngram_prob <- ngrams %>% pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
    mutate(p = n/sum(n))
  unigram_prob <- corp_df %>% text_to_word() %>%
    filter(!(word %in% rmWords_vec)) %>% count(word, sort = TRUE) %>%
    mutate(p = n/sum(n))
  ratio_prob <- ngram_prob %>% filter(n > 20) %>% 
    left_join(unigram_prob %>% select(item1 = word, p1 = p), by = "item1") %>%
    left_join(unigram_prob %>% select(item2 = word, p2 = p), by = "item2") %>%
    mutate(p_ratio = p/p1/p2) %>% 
    arrange(-p_ratio)
  
  sparse_mat <- ratio_prob %>% mutate(pmi = log10(p_ratio)) %>% 
    cast_sparse(item1, item2, pmi)
  sparse_mat@x[is.na(sparse_mat@x)] <- 0
  word2d_svd <- svd(sparse_mat, 2)
  word2d_df <- as.data.frame(word2d_svd$u)
  word2d_df$word <- rownames(sparse_mat)
  
  # td-idf
  recipe_len <- length(unique(unigram$recipeID))
  
  recipe_idf <- unigram %>% pairwise_count(word, recipeID, diag = TRUE) %>%
    filter(item1 == item2) %>% select(-item2)
  recipe_tfidf <- unigram %>% count(word, sort = TRUE) %>%
    rename(tf = n) %>% left_join(recipe_idf %>% rename(word = item1), by = "word") %>%
    rename(idf = n) %>% mutate(tf = tf/recipe_len, idf = log(recipe_len/idf), tf_idf = tf * idf) %>%
    arrange(-tf_idf)
  
  top100 <- recipe_tfidf[1:100,]
  word2d_100 <- word2d_df %>% filter(word %in% top100$word)
  return(word2d_100)
}

vegan2d <- diet2d(vegan_df, c("fat", "serve"))
glutenFree2d <- diet2d(glutenFree_df, "fat")
dairyFree2d <- diet2d(dairyFree_df, "fat")
reg2d <- diet2d(reg_df, c("dough", "make", "drain", "heat", "add", "large", "cook"))

write.csv(vegan2d, file.path(xpath_nlp, "vegan2d.csv"), row.names = FALSE)
write.csv(glutenFree2d, file.path(xpath_nlp, "glutenFree2d.csv"), row.names = FALSE)
write.csv(dairyFree2d, file.path(xpath_nlp, "dairyFree2d.csv"), row.names = FALSE)
write.csv(reg2d, file.path(xpath_nlp, "reg2d.csv"), row.names = FALSE)

library(ggplot2)

png(file.path(xpath_nlp, "vegan.png"), width = 1280, height = 720)
ggplot(vegan2d) +
  geom_text(aes(x = V1, y = V2, label = word), col = "blue", size = 5) +
  labs(title = "Vegan Diet") +
  theme(plot.title = element_text(size = 20))
dev.off()
  # png() stops plots from showing up on R IDE 

png(file.path(xpath_nlp, "glutenFree.png"), width = 1280, height = 720) 
ggplot(glutenFree2d) +
  geom_text(aes(x = V1, y = V2, label = word), col = "blue", size = 5) +
  labs(title = "Gluten-Free Diet") +
  theme(plot.title = element_text(size = 20))
dev.off()

png(file.path(xpath_nlp, "dairyFree.png"), width = 1280, height = 720) 
ggplot(dairyFree2d) +
  geom_text(aes(x = V1, y = V2, label = word), col = "blue", size = 5) +
  labs(title = "Dairy-Free Diet") +
  theme(plot.title = element_text(size = 20))
dev.off()

png(file.path(xpath_nlp, "reg.png"), width = 1280, height = 720)
ggplot(reg2d) +
  geom_text(aes(x = V1, y = V2, label = word), col = "blue", size = 5) +
  labs(title = "Unrestricted Diet") +
  theme(plot.title = element_text(size = 20))
dev.off()
