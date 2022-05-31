
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

# acquire tables
recipe <- dbGetQuery(con, "SELECT * FROM spoonacularRecipe")
taste <- dbGetQuery(con, "SELECT * FROM spoonacularTaste")

instruct <- dbGetQuery(con, "SELECT * FROM spoonacularInstruct")
library(dplyr)
instruct <- instruct %>% filter(instructions != "")

#### DATA WRANGLING ####
library(tidyverse)

# combine into one df 
merge <- recipe %>% inner_join(taste, by = "DT") %>% inner_join(instruct, by = "DT") %>%
  select(-c(14,15)) %>% mutate(spiciness = c("none", "medium", "high")[findInterval(spiciness, c(0, 380051, 67500000), rightmost.closed = TRUE)])
  # %>% mutate(spiciness = round(min_max_norm(spiciness), 2))
    # do not combine mutate and lapply (work too similarly)
  # findInterval() has interval closed on left, open on right 

# write df to DB 
bool <- dbExistsTable(con, "spoonacular_clean")
if (bool){
  dbGetQuery(con, "DROP TABLE spoonacular_clean")
}

qstr <-
  paste(
    "CREATE TABLE spoonacular_clean", 
    "(DT VARCHAR(10) NOT NULL,",
    "recipeID INT(10),",
    "vegetarian VARCHAR(5),",
    "vegan VARCHAR(5),",
    "glutenFree VARCHAR(5),",
    "dairyFree VARCHAR(5),",
    "veryHealthy VARCHAR(5),",
    "cheap VARCHAR(5),",
    "veryPopular VARCHAR(5),",
    "sustainable VARCHAR(5),",
    "lowFodmap VARCHAR(5),",
    "weightWatcherSmartPoints INT(5),",
    "gaps VARCHAR(5),",
    "aggregateLikes INT(5),",
    "healthScore INT(5),",
    "sourceName TEXT,",
    "pricePerServing INT(5),",
    "readyInMinutes INT(5),",
    "sweetness INT(5),",
    "saltiness INT(5),",
    "sourness INT(5),",
    "bitterness INT(5),", 
    "savoriness INT(5),",
    "fattiness INT(5),",
    "spiciness VARCHAR(10),",
    "instructions TEXT,",
    "PRIMARY KEY (DT))"
  )

dbGetQuery(con, qstr)

dbWriteTable(con, "spoonacular_clean", merge, row.names = FALSE, append = TRUE)



