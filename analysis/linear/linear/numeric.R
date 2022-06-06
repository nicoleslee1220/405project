
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
dbListTables(con)

##### load in csv

numeric_df <- read.csv(file.path(xpath_lin, "_assets", "numeric_df.csv"), header = TRUE, sep = ",")

categoryName <- list(meat = c("chicken", "fish", "shrimp", "tuna"), 
                     vegetable = c("potato", "tomato", "arugula", "broccoli", "mushroom", 
                                   "beet", "vegetables"), 
                     bread = c("bread", "flour"), 
                     cheese = "cheese", 
                     spice = c("salt", "pepper", "onion"), 
                     oil = c("oil", "butter"))


categories <- list(definition = categoryName, df = numeric_df)

##### acquire numerics

recipe_path <- file.path(xpath_main, "spoonacular_recipe")
recipe <- list.files(recipe_path, pattern="^recipe")

library(rjson)
this_ls  <- fromJSON(file = file.path(recipe_path, recipe))

ingredient_df <- data.frame(ID = NULL, ingredients = NULL, amt = NULL, units = NULL)
for (i in 1:length(recipe)){
  this_ls  <- fromJSON(file = file.path(recipe_path, recipe[i]))
  
  ingredient_list <- unlist(this_ls$extendedIngredients)
  id <- this_ls$id
  ingredient <- ingredient_list[names(ingredient_list) == "name"] 
  amt <- ingredient_list[names(ingredient_list) == "amount"]
  unit <- ingredient_list[names(ingredient_list) == "unit"]
  
  if (length(ingredient_list) != 0){
    temp_df <- data.frame(id = id, ingredients = ingredient, amount = amt,
                                units = unit)
    ingredient_df <- rbind.data.frame(ingredient_df, temp_df)
  }
  cat(i, "\n")
}

test <- ingredient_df[grepl("chicken", ingredient_df$ingredients, ignore.case = TRUE),]

# chicken: chicken broth 
# fish: fish sauce 
  # biased dataset 
# shrimp: shrimp stock, shrimp paste 



##### 
