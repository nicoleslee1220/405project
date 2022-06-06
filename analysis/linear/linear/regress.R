
xpath_main <- Sys.getenv("PATH_MY_MAIN_DATA")
xpath_lin <- file.path(xpath_main, "analysis", "linear")

library(RMySQL)
drv <- dbDriver("MySQL")
xdbsock <- ""

xdbuser <- Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_USER")
xpw     <- Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_PW")
xdbname <- Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_DBNAME")
xdbhost <- Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_HOST")
xdbport <- as.integer( Sys.getenv("MAS405_AWS_PROJ_DB_ROUSER_PORT") )

con <- dbConnect(drv, user=xdbuser, password=xpw, dbname=xdbname, host=xdbhost, port=xdbport, unix.sock=xdbsock)

clean_df <- dbGetQuery(con, "SELECT * FROM spoonacular_clean2")

####
# prep clean_df for regression
names(clean_df)
library(dplyr)

regress_df <- clean_df %>% select(-c(DT, recipeID , sourceName, instructions)) 
regress_df[1:9] <- lapply(regress_df[1:9], as.logical)
regress_df[c("gaps", "spiciness")] <- lapply(regress_df[c("gaps", "spiciness")], as.factor)
regress_df <- regress_df %>% select(-c(cheap, sustainable)) %>% select(-veryHealthy)
  # cheap, sustainable are all FALSE 
  # veryHealthy is highly correlated and unhelpful
str(regress_df)

control_mod <- lm(healthScore ~ ., data = regress_df)

#### combine keyword_df from clusters 
keyword_df <- read.csv(file.path(xpath_lin, "linear", "_assets", "keyword_df.csv"), 
                        header = TRUE, sep = ",")
totRegress_df <- cbind(regress_df, keyword_df)

mod <- lm(healthScore ~ ., data = totRegress_df)
