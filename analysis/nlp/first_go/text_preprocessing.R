
# connect to database
library(RMySQL)
library(rjson)
library(dplyr)
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

xpath_main_data <- Sys.getenv("PATH_MY_MAIN_DATA")
(xpath_scrape <- file.path(xpath_main_data, "spoonacular"))

instruct_df <- dbGetQuery(con, "SELECT * FROM spoonacularInstruct")
instruct_clean <- instruct_df %>% filter(instructions != "")

library(SnowballC) # Corpus()
library(tm)
  # don't use stemDocument
library(textstem) 
  # lemmatization

# text preprocessing 
corp <- Corpus(VectorSource(instruct_clean$instructions))
corp_clean <- tm_map(corp, removeNumbers)
corp_clean <- tm_map(corp_clean, removePunctuation)
corp_clean <- tm_map(corp_clean, stripWhitespace)
corp_clean <- tm_map(corp_clean, content_transformer(tolower))
corp_clean <- tm_map(corp_clean, removeWords, stopwords('english'))
corp_clean <- tm_map(corp_clean, lemmatize_strings)

corp_vec <- unlist(unlist(corp_clean, recursive = FALSE))
  # SimpleCorpus does not follow regular R list syntax
    # try corp_clean$'1'
corp_vec <- corp_vec[-length(corp_vec)]
corp_df <- data.frame(recipeID = as.character(instruct_clean$DT), instructions = corp_vec)

# write out csv
xpath_nlp <- file.path(xpath_main_data, "analysis", "nlp")
if(!dir.exists(file.path(xpath_nlp, "_assets"))){
  dir.create(file.path(xpath_nlp, "_assets"))
}

write.csv(corp_df, file.path(xpath_nlp, "_assets", "corp_df.csv"), row.names = FALSE, quote = TRUE)

