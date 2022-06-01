
library(RMySQL)
drv <- dbDriver("MySQL")
xdbsock <- ""


xdbuser <- Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_USER")
xpw     <- Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_PW")
xdbname <- Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_DBNAME")
xdbhost <- Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_HOST")
xdbport <- as.integer( Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_PORT") )


con <- dbConnect(drv, user=xdbuser, password=xpw, dbname=xdbname, host=xdbhost, port=xdbport, unix.sock=xdbsock)

dbGetInfo(con)
dbListTables(con)

# Getting recipe table
recipe<-dbGetQuery(con, "SELECT * FROM spoonacularRecipe")
head(recipe)
dim(recipe)


# Getting taste table
taste<-dbGetQuery(con, "SELECT * FROM spoonacularTaste")
dim(taste)
head(taste)


# Getting instruction table
instruct<- dbGetQuery(con, "SELECT * FROM spoonacularInstruct ")
head(instruct)
dim(instruct)
library(dplyr)
instruct2<-instruct %>% filter(instructions != "")
head(instruct2)
dim(instruct2)






##### DATA WRANGLING ####
library(tidyverse)
library(dplyr)
# combine all 3 dataframes into 1 datafarme 
df_list<-list(recipe, taste, instruct2)
merge3<-df_list %>% reduce(inner_join, by = 'DT')
head(merge3)
dim(merge3)

# get red of prep mins and cooking mins 
merge4<-subset(merge3, select = -c(14,15))
dim(merge4)
head(merge4)


# NORMALIZE Spiciness using this fxn: 
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x)) *100
}

#apply Min-Max normalization; note col 25 is spiciness 
merge5_norm <- merge4 %>% mutate(spiciness=lapply(merge4[25], min_max_norm))


#merge5_norm is our spoonacular_clean table

for(i in 3:11){
  merge5_norm[,i]<- ifelse(merge5_norm[,i]=='TRUE',T,F)
}



##### Histograms by diet

par(mfrow=c(1,3))

vegan<- merge5_norm[(merge5_norm$vegan==TRUE),]
hist(vegan$healthScore)

glutenfree<- merge5_norm[merge5_norm$glutenFree==TRUE,]
hist(glutenfree$healthScore)

dairyfree<- merge5_norm[merge5_norm$dairyFree==TRUE,]
hist(dairyfree$healthScore)


# unrestricted diet histogram
par(mfrow=c(1,1))
unrestricted<- merge5_norm[which((apply(merge5_norm[,3:11], 1, function(x) length(unique(x)) == 1) == TRUE)),]
hist(unrestricted$healthScore)