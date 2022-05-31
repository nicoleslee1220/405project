##Connecting to group project DB
library(RMySQL)
library(rjson)
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




##Get Recipe Information: creating table in DB
xpath_main_data <- Sys.getenv("PATH_MY_MAIN_DATA")
xtableName <- "spoonacularRecipe"
xbool.tableExists <- dbExistsTable(con, xtableName) ; xbool.tableExists

if(!xbool.tableExists) {
  qstr <-
    paste0(
      "CREATE TABLE ", xtableName, "  ",
      "(DT VARCHAR(15) NOT NULL, ",
      "recipeID INT(10), ",
      "vegetarian VARCHAR(5), ",
      "vegan VARCHAR(5), ",
      "glutenFree VARCHAR(5), ",
      "dairyFree VARCHAR(5), ",
      "veryHealthy VARCHAR(5), ",
      "cheap VARCHAR(5), ",
      "veryPopular VARCHAR(5), ",
      "sustainable VARCHAR(5), ",
      "lowFodmap VARCHAR(5), ",
      "weightWatcherSmartPoints INT(7), ",
      "gaps VARCHAR(5), ",
      "preparationMinutes INT(7), ",
      "cookingMinutes INT(7), ",
      "aggregateLikes INT(7), ",
      "healthScore  INT(7), ",
      "sourceName VARCHAR(20), ",
      "pricePerServing DECIMAL(10, 2), ",
      "PRIMARY KEY (DT))"
      
    )
  
  xx <- dbGetQuery(con, qstr)
}

##Get Recipe Information: Inserting json data into DB

xgfn <- list.files(file.path(xpath_main_data, "spoonacular_recipe"), pattern="^recipe")
xgfn_sub <- xgfn

ii <- 1
for(ii in 1:length(xgfn_sub)) {
  
  xthis_fn <- xgfn_sub[ii] 
  xxx <- strsplit(xthis_fn, "_")[[1]]
  DT <- xxx[2] #DT is the recipe number
  recipeID <- xxx[2]


  xthis_ls <- fromJSON( file=file.path(xpath_scrape, xthis_fn) )

  vegetarian <- xthis_ls[[c("vegetarian")]]
  vegan <- xthis_ls[[c("vegan")]]
  glutenFree <- xthis_ls[[c("glutenFree")]]
  dairyFree <- xthis_ls[[c("dairyFree")]]
  veryHealthy <- xthis_ls[[c("veryHealthy")]]
  cheap <- xthis_ls[[c("cheap")]]
  veryPopular <- xthis_ls[[c("veryPopular")]]
  sustainable <- xthis_ls[[c("sustainable")]]
  lowFodmap <- xthis_ls[[c("lowFodmap")]]
  weightWatcherSmartPoints <- xthis_ls[[c("weightWatcherSmartPoints")]]
  gaps <- xthis_ls[[c("gaps")]]
  preparationMinutes <- xthis_ls[[c("preparationMinutes")]]
  veryHealthy <- xthis_ls[[c("veryHealthy")]]
  cookingMinutes <- xthis_ls[[c("cookingMinutes")]]
  aggregateLikes <- xthis_ls[[c("aggregateLikes")]]
  healthScore <- xthis_ls[[c("healthScore")]]
  sourceName <- xthis_ls[[c("sourceName")]]
  pricePerServing <- xthis_ls[[c("pricePerServing")]]
 
  

      
  qstr <- paste0(
          "INSERT INTO ", xtableName, " (DT, recipeID, vegetarian, vegan, glutenFree, dairyFree, veryHealthy,
                                        cheap, veryPopular, sustainable, lowFodmap, weightWatcherSmartPoints, gaps, preparationMinutes,
                                        cookingMinutes, aggregateLikes, healthScore, sourceName, pricePerServing) ",
          # "INSERT INTO ", xtableName, " (DT, recipeID, vegetarian, pricePerServing) ",

          " VALUES ",
          "('",
          DT, "', '",
          as.integer(recipeID), "', '",
          vegetarian, "', '",
          vegan, "', '",
          glutenFree, "', '",
          dairyFree, "', '",
          veryHealthy, "', '",
          cheap, "', '",
          veryPopular, "', '",
          sustainable, "', '",
          lowFodmap, "', '",
          as.integer(weightWatcherSmartPoints), "', '",
          gaps, "', '",
          as.integer(preparationMinutes), "', '",
          as.integer(cookingMinutes), "', '",
          as.integer(aggregateLikes), "', '",
          as.integer(healthScore), "', '",
          sourceName,"', '",
          as.integer(pricePerServing), "')"
        )
      
      xx <- try(dbGetQuery(con, qstr), silent=TRUE)
      
      if( "try-error" %in% class(xx) ) {
        cat("SQL insert into Team Table failed for recipe# ", DT, "\n")
      } else {
        cat("Successfully inserted recipe# ", DT, "into Table", "\n")
      }
 
}




##Taste: creating table in DB
xtableName <- "spoonacularTaste"
xbool.tableExists <- dbExistsTable(con, xtableName) ; xbool.tableExists

if(!xbool.tableExists) {
  qstr <-
    paste0(
      "CREATE TABLE ", xtableName, "  ",
      "(DT VARCHAR(15) NOT NULL, ",
      "sweetness INT(10), ",
      "saltiness INT(10), ",
      "sourness INT(10), ",
      "bitterness INT(10), ",
      "savoriness INT(10), ",
      "fattiness INT(10), ",
      "spiciness INT(10), ",
      "PRIMARY KEY (DT))"
      
    )
  
  xx <- dbGetQuery(con, qstr)
}






##Taste: Inserting json data into DB

xgfn <- list.files(file.path(xpath_main_data, "spoonacular_taste"), pattern="^taste")
xgfn_sub <- xgfn

ii <- 1
for(ii in 1:length(xgfn_sub)) {
  
  xthis_fn <- xgfn_sub[ii] 
  xxx <- strsplit(xthis_fn, "_")[[1]]
  DT <- xxx[2] #DT is the recipe number
  
  
  
  xthis_ls <- fromJSON( file=file.path(xpath_scrape, xthis_fn) )
  
  sweetness <- xthis_ls[[c("sweetness")]]
  saltiness <- xthis_ls[[c("saltiness")]]
  sourness <- xthis_ls[[c("sourness")]]
  bitterness <- xthis_ls[[c("bitterness")]]
  savoriness <- xthis_ls[[c("savoriness")]]
  fattiness <- xthis_ls[[c("fattiness")]]
  spiciness <- xthis_ls[[c("spiciness")]]
  
  
  
  qstr <- paste0(
    "INSERT INTO ", xtableName, " (DT, sweetness, saltiness, sourness, bitterness, savoriness, fattiness,
                                        spiciness) ",

    " VALUES ",
    "('",
    DT, "', '",
    as.integer(sweetness), "', '",
    as.integer(saltiness), "', '",
    as.integer(sourness), "', '",
    as.integer(bitterness), "', '",
    as.integer(savoriness), "', '",
    as.integer(fattiness), "', '",
    as.integer(spiciness), "')"
  )
  
  xx <- try(dbGetQuery(con, qstr), silent=TRUE)
  
  if( "try-error" %in% class(xx) ) {
    cat("SQL insert into Team Table failed for recipe# ", DT, "\n")
  } else {
    cat("Successfully inserted recipe# ", DT, "into Table", "\n")
  }
  
}

## Instructions 
# create table 
qstr <- paste("CREATE TABLE spoonacularInstruct",
              "(DT VARCHAR (10) NOT NULL,", 
              "instructions VARCHAR(65535),", 
              "PRIMARY KEY (DT))")
instruct_bool <- dbExistsTable(con, "spoonacularInstruct")
if (!instruct_bool){
  dbGetQuery(con, qstr)
}
dbListTables(con)

instruct_fl <- list.files(file.path(xpath_main_data, "spoonacular_instructions"), pattern = "^instructions")
  # note: id 10000 comes before 5000
  # file.size(file.path(xpath_scrape, fileName))
    # some files contain nothing; need to be filtered out during data wrangling
for (ii in 1:length(instruct_fl)){
  fileName <- instruct_fl[ii]
  id <- strsplit(fileName, "_")[[1]][[2]]
  rList <- fromJSON(file = file.path(xpath_scrape, fileName))
  allSteps <- ""
  if (length(rList) != 0){
    for (jj in 1:length(rList)){
      stepList <- rList[[jj]]$steps
      # gather steps 
      if (length(stepList) != 0){
        for (kk in 1:length(stepList)){
          newStep <- stepList[[kk]]$step
          allSteps <- paste(allSteps, newStep)
        }
      }
    }
  }
  qstr <- paste0("INSERT INTO spoonacularInstruct ", 
                 "(DT, instructions) VALUES (", 
                 "'", id, "', ", 
                 "'", allSteps, "')")
  cmd <- try(dbGetQuery(con, qstr), silent = TRUE)
  # error handling
  if("try-error" %in% class(cmd)){
    cat("ID", id, "failed to insert successfully. \n")
  } else {
    cat("Successfully inserted ID", id, "in database. \n")
  }
}



