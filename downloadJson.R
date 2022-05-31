options(width=90, xtable.comment = FALSE)

##Load relevant libraries

library(SSsimple)
library(rjson)


## Setting directory for .json download. xpath_scrape is where .json will download into
xpath_main_data <- Sys.getenv("PATH_MY_MAIN_DATA")

xpath_scrape <-
  file.path(
    xpath_main_data,
    "spoonacular"
  )


if(!dir.exists(xpath_scrape)) {
  dir.create(xpath_scrape, recursive=TRUE)
}


## Add xAPI_KEY to .Renviron
xAPIkey <- Sys.getenv("MY_SPOONACULAR_APIKEY")
xAPIkey



## Get Recipe Information: Spoonacular has about 1.15 million recipes, downloading a subset
# ii_vec <- sample(1:1500000, 7000, replace = FALSE)
ii_vec <- 5600:13000 # assuming recipe ids are not correlated; ~7400 recipes
for(ii in ii_vec) {

    xthis_url <-
      paste0(
        "https://api.spoonacular.com/recipes/",
        ii,
        "/information",
        "?apiKey=",
        xAPIkey
      )
  
    xthisJSON <- try(suppressWarnings(readLines(con=xthis_url)))
    
    if("try-error" %in% class(xthisJSON)){
      cat("Could not read recipe", ii, "\n")
    }else{
      xtn_out <- paste0("recipe_", ii, "_spoonacular.json")
      writeLines( xthisJSON, file.path(xpath_scrape, xtn_out) ) # overwrites file if same id exists
      cat("Done getting recipe", ii, "\n" )
    }
                     
    Sys.sleep(sample(c(1,3,5), 1))
    
}

## Taste: Spoonacular has about 1.15 million recipes, downloading a subset
for(ii in ii_vec) {
  
  xthis_url <-
    paste0(
      "https://api.spoonacular.com/recipes/",
      ii,
      "/tasteWidget.json",
      "?apiKey=",
      xAPIkey
    )
  
  xthisJSON <- try(suppressWarnings(readLines(con=xthis_url)))
  
  if("try-error" %in% class(xthisJSON)){
    cat("Could not read recipe", ii, "\n")
  }else{
    xtn_out <- paste0("taste_", ii, "_spoonacular.json")
    writeLines( xthisJSON, file.path(xpath_scrape, xtn_out) )
    cat("Done getting recipe", ii, "\n" )
  }
  
  Sys.sleep(sample(c(1,3,5), 1))
  
}

## Instructions
for(ii in ii_vec) {
  
  xthis_url <-
    paste0(
      "https://api.spoonacular.com/recipes/",
      ii,
      "/analyzedInstructions",
      "?apiKey=",
      xAPIkey
    )
  
  xthisJSON <- try(suppressWarnings(readLines(con=xthis_url)))
  
  if("try-error" %in% class(xthisJSON)){
    cat("Could not read recipe", ii, "\n")
  }else{
    xtn_out <- paste0("instructions", ii, "_spoonacular.json")
    writeLines( xthisJSON, file.path(xpath_scrape, xtn_out) )
    cat("Done getting recipe", ii, "\n" )
  }
  
  Sys.sleep(sample(c(1,2,3), 1))
  
}

# fl <- list.files(xpath_scrape, pattern = "^instructions")
# file.rename(fl, sub("(.{12})(.*)", "\\1_\\2", fl))


