
options(timeout = 1000000)
load(url("https://cbail.github.io/Elected_Official_Tweets.Rdata"))

# word-embedding model without using neural networks
library(tidytext)
    # unnest_tokens
library(dplyr)

elected_no_retweets <- elected_official_tweets %>%
  filter(is_retweet == F) %>%
  select(c("text"))
#create tweet id
elected_no_retweets$postID<-row.names(elected_no_retweets)

library(widyr)
  # pairwise_count()
#create context window with length 8
tidy_skipgrams <- elected_no_retweets %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, postID, ngramID) %>%
  unnest_tokens(word, ngram)
    # post_numberedNgram by words
    # window size 8 is how far apart we are willing to consider context words from input word

#calculate unigram probabilities (used to normalize skipgram probabilities later)
unigram_probs <- elected_no_retweets %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))
  # how often a word appears across entire corpus
  # automatic token is "words," which count recognizes  

#calculate probabilities
skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))
  # 215233 ngrams exist
  # proportion out of total ngrams two letters appear 
    # how often two words appear next to each other

#normalize probabilities
  # ratio of probabilities between words appearing together and words occurring alone (joint)
normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

normalized_prob %>% 
  filter(word1 == "trump") %>%
  arrange(-p_together)

# vectorization of word
pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)
  # arranges into df with row word1 and column word2 
    # all combinations of word 1 and word2 with probability ratio value 
  # from perspective of column vector (word), ratio probabilities against entire corpus
    # (word) vector with interaction with other words as probability ratio value

library(irlba)

#remove missing data
pmi_matrix@x[is.na(pmi_matrix@x)] <- 0
#run SVD
pmi_svd <- irlba(pmi_matrix, 256, maxit = 500)
#next we output the word vectors:
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)
  # rows: 6019 word vectors 
  # columns: dimensionality of vector (6019 -> 256)


# single valued decomposition 
  # want to use fast SVD algorithm for sparse matrices
    # sparse matrices because some words don't interact with each other at all (zero values)

# find similarity between words in high-dimension
library(broom) 
  # part of widyr library
  
search_synonyms <- function(word_vectors, selected_vector) {
  # similar to find_similar_words for Python
  similarities <- word_vectors %*% selected_vector %>%
    data.frame(similarity = .) %>%
    arrange(-similarity) %>% head(10)
}
  # word_vectors %*% selected_vector is dot point -> similarity 
  # tibble looks better for print()

pres_synonym <- search_synonyms(word_vectors, word_vectors["president",])
pres_synonym

# graph words into 2-dimensions 
  # dot project/cosine similarity is a better measurement of similarity between words
    # creates ranking system of input word based on high dimensions (more information considered)
    # but does not create hard boundaries/clusters done in k-means clustering
pmi_svd <- irlba(pmi_matrix, 2, maxit = 500)

#next we output the word vectors:
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

#grab 100 words
forplot<-as.data.frame(word_vectors[200:300,])
forplot$word<-rownames(forplot)
  # need another column of input word for graphing

#now plot
library(ggplot2)
ggplot(forplot, aes(x=V1, y=V2, label=word))+
  geom_text(aes(label=word),hjust=0, vjust=0, color="blue")+
  theme_minimal()+
  xlab("First Dimension Created by SVD")+
  ylab("Second Dimension Created by SVD")
