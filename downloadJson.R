options(width=90, xtable.comment = FALSE)

##Load relevant libraries

library(SSsimple)
library(rjson)


## Setting directory for .json downlad. xpath_scrape is where .json will download into
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



## Spoonacular has about 1.7 million recipes, downloading a subset
ii <- 5600 
for(ii in 5600:6000) {

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
      writeLines( xthisJSON, file.path(xpath_scrape, xtn_out) )
      cat("Done getting recipe", ii, "\n" )
    }
                     
    Sys.sleep(1)
    
}




