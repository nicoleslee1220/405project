
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

con <- dbConnect(drv, user=xdbuser, password=xpw, dbname=xdbname, host=xdbhost, port=xdbport, unix.sock=xdbsock)
dbListTables(con)

# Getting recipe table

clean_df <-dbGetQuery(con, "SELECT * FROM spoonacular_clean")
dim(clean_df)


# salad 
cond1 <- grepl("toss", clean_df$instructions, ignore.case = TRUE)
cond2 <- grepl("combine", clean_df$instructions, ignore.case = TRUE)
cond3 <- grepl("bowl", clean_df$instructions, ignore.case = TRUE)

salad <- (cond1 | cond2) & cond3

# saucy 
cond1 <- grepl("coat", clean_df$instructions, ignore.case = TRUE)
cond2 <- grepl("sauce", clean_df$instructions, ignore.case = TRUE)
cond3 <- grepl("simmer", clean_df$instructions, ignore.case = TRUE)
cond4 <- grepl("reduce", clean_df$instructions, ignore.case = TRUE)

saucy <- cond1 | cond2 | cond3 | cond4

# oven 
cond1 <- grepl("bake", clean_df$instructions, ignore.case = TRUE)
cond2 <- grepl("oven", clean_df$instructions, ignore.case = TRUE)
cond3 <- grepl("sheet", clean_df$instructions, ignore.case = TRUE)

oven <- cond1 | cond2 | cond3

# boil
cond1 <- grepl("boil", clean_df$instructions, ignore.case = TRUE)

boil <- cond1

# golden brown 
cond1 <- grepl("golden", clean_df$instructions, ignore.case = TRUE)
cond2 <- grepl("brown", clean_df$instructions, ignore.case = TRUE)

goldbr <- cond1 | cond2

# meat
cond1 <- grepl("chicken", clean_df$instructions, ignore.case = TRUE)
cond2 <- grepl("fish", clean_df$instructions, ignore.case = TRUE)
cond3 <- grepl("shrimp", clean_df$instructions, ignore.case = TRUE)
cond4 <- grepl("tuna", clean_df$instructions, ignore.case = TRUE)

meat <- cond1 | cond2 | cond3 | cond4

# vegetables 
cond1 <- grepl("potato", clean_df$instructions, ignore.case = TRUE)
cond2 <- grepl("tomato", clean_df$instructions, ignore.case = TRUE)
cond3 <- grepl("arugula", clean_df$instructions, ignore.case = TRUE)
cond4 <- grepl("broccoli", clean_df$instructions, ignore.case = TRUE)
cond5 <- grepl("mushroom", clean_df$instructions, ignore.case = TRUE)
cond6 <- grepl("beet", clean_df$instructions, ignore.case = TRUE)
cond7 <- grepl("vegetable", clean_df$instructions, ignore.case = TRUE)

vegetable <- cond1 | cond2 | cond3 | cond4 | cond5 | cond6 | cond7

# spices
cond1 <- grepl("pepper", clean_df$instructions, ignore.case = TRUE)
cond2 <- grepl("salt", clean_df$instructions, ignore.case = TRUE)
cond3 <- grepl("onion", clean_df$instructions, ignore.case = TRUE)

spice <- cond1 | cond2 | cond3

# oil 
cond1 <- grepl("oil", clean_df$instructions, ignore.case = TRUE)
cond2 <- grepl("butter", clean_df$instructions, ignore.case = TRUE)

oil <- cond1 | cond2

# bread
cond1 <- grepl("bread", clean_df$instructions, ignore.case = TRUE)
cond2 <- grepl("flour", clean_df$instructions, ignore.case = TRUE)

bread <- cond1 | cond2

# cheese
cond1 <- grepl("cheese", clean_df$instructions, ignore.case = TRUE)

cheese <- cond1





### acquire numerics for meat, vegetable, bread, cheese, spice, oil 
xpath_lin <- file.path(xpath_main, "analysis", "linear")
dir.create(xpath_lin)
dir.create(file.path(xpath_lin, "_assets"))

numeric_df <- data.frame(meat = meat, vegetable = vegetable, bread = bread, 
                          cheese = cheese, spice = spice, oil = oil)
logic_df <- data.frame(salad = salad, saucy = saucy, oven = oven, boil = boil, 
                      goldbr = goldbr)

write.csv(numeric_df, file.path(xpath_lin, "_assets", "numeric_df.csv"), row.names = FALSE)
write.csv(logic_df, file.path(xpath_lin, "_assets", "logic_df.csv"), row.names = FALSE)

keyword_df <- cbind(numeric_df, logic_df)
write.csv(keyword_df, file.path(xpath_lin, "_assets", "keyword_df.csv"), row.names = FALSE)
