
path_data <- file.path(Sys.getenv("PATH_MY_MAIN_DATA"), "analysis", "nlp", "_assets")

set.seed(2)
  # every time cluster_list() is used, will produce different cluster based on seed and number of times
library(factoextra)
library(cluster)
library(dplyr)

prep <- function(diet){
  rownames(diet) <- diet[,3]
  diet <- diet[,-3]
}

clust_list <- function(diet, cluster_num){
  diet <- prep(diet)
  # apply k-means clustering
  # fviz_nbclust(diet, kmeans, method = "wss")
    # pick 3 number range when graph goes flat and test individually
  km <- kmeans(diet, centers = cluster_num, nstart = 25)
  # create list
  diet_list <- km$cluster %>% as.data.frame %>% 
    rename(clust_group = colnames(.)[1]) %>%
    mutate(word = rownames(diet)) %>% split(., .$clust_group)
  return(list(km = km, diet_list = diet_list))
}

# vegan 

set.seed(2) # to maintain consistency of clusters (first-use) 
vegan <- read.csv(file.path(path_data, "vegan2d.csv"))
vegan_list <- clust_list(vegan, 6)

png(file.path(path_data, "vegan_clust.png"), width = 1080, height = 720)
fviz_cluster(vegan_list$km, data = prep(vegan), axes = c(10,50), main = "Vegan Diet", labelsize = 10)
dev.off()
  # calling on pre-made functions in arguments is useful
  # spooling does not work in a function/loop/Rmarkdown? 

# dairy-free

set.seed(2) # to maintain consistency of clusters 
dairyFree <- read.csv(file.path(path_data, "dairyFree2d.csv"))
dairyFree_list <- clust_list(dairyFree, 7)

png(file.path(path_data, "dairyFree_clust.png"), width = 1080, height = 720)
fviz_cluster(dairyFree_list$km, data = prep(dairyFree), axes = c(10,50), main = "Dairy-Free Diet",
             labelsize = 10)
dev.off()

# gluten-free

set.seed(2) # to maintain consistency of clusters 
glutenFree <- read.csv(file.path(path_data, "glutenFree2d.csv"))
glutenFree_list <- clust_list(glutenFree, 6)

png(file.path(path_data, "glutenFree_clust.png"), width = 1080, height = 720)
fviz_cluster(glutenFree_list$km, data = prep(glutenFree), axes = c(10,50), main = "Gluten-Free Diet", labelsize = 10)
dev.off()

# unrestricted

set.seed(2) # to maintain consistency of clusters 
reg <- read.csv(file.path(path_data, "reg2d.csv"))
reg_list <- clust_list(reg, 6)

png(file.path(path_data, "reg_clust.png"), width = 1080, height = 720)
fviz_cluster(reg_list$km, data = prep(reg), axes = c(10,50), main = "Unrestricted Diet", labelsize = 10)
dev.off()

# rum: unigram_prob$word[109100:109200]
# look at words within cluster, png for outliers and closeness, and sentences for recipe context
