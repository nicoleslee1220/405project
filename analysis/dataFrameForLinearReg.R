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
recipe<-dbGetQuery(con, "SELECT * FROM spoonacular_clean")
dim(recipe)

#Unrestricted
##Pasta w/ meat: checking for pasta + bacon, pasta + chicken, pasta + shrimp, pasta + fish, pasta + beef, pasta + meat
combination1<- grepl("pasta", recipe$instructions,ignore.case = TRUE) & grepl("chicken", recipe$instructions,ignore.case = TRUE)
combination2<- grepl("pasta", recipe$instructions,ignore.case = TRUE) & grepl("beef", recipe$instructions,ignore.case = TRUE)
combination3<- grepl("pasta", recipe$instructions,ignore.case = TRUE) & grepl("shrimp", recipe$instructions,ignore.case = TRUE)
combination4<- grepl("pasta", recipe$instructions,ignore.case = TRUE) & grepl("fish", recipe$instructions,ignore.case = TRUE)
combination5<- grepl("pasta", recipe$instructions,ignore.case = TRUE) & grepl("meat", recipe$instructions,ignore.case = TRUE)
combination6<- grepl("pasta", recipe$instructions,ignore.case = TRUE) & grepl("bacon", recipe$instructions,ignore.case = TRUE)
pastaWMeat<- combination1 | combination2 | combination3 | combination4 | combination5 | combination6

##Crumb-Crusted: checking for egg + coat
combination1 <- grepl("egg", recipe$instructions,ignore.case = TRUE) & grepl("coat", recipe$instructions,ignore.case = TRUE)
crumbCrusted <- combination1


##Saucy: checking for simmer
combination1 <- grepl("simmer", recipe$instructions,ignore.case = TRUE)
saucy <- combination1

##Cake: checking for cake
combination1 <- grepl("cake", recipe$instructions,ignore.case = TRUE)
cake <- combination1

##Unrestricted_dataframe
unrestricted_df<- data.frame(pastaWMeat, crumbCrusted, saucy, cake)






#Gluten Free
##Crumb-Crusted: shows up already on Unrestricted therefore skip

##Tossed Salad: checking for toss + salad
combination_2<-grepl("toss", recipe$instructions,ignore.case = TRUE) & grepl("salad", recipe$instructions,ignore.case = TRUE)
tossedSalad <- combination_2

##Saucy: shows up already on Unrestricted therefore skip

##GlutenFree_dataframe
glutenFree_df<- data.frame(tossedSalad)





#Dairy Free
##Roasted: check for oven + golden
roasted <- grepl("oven",recipe$instructions,ignore.case = TRUE) & grepl("golden",recipe$instructions,ignore.case = TRUE)

##Bake: checking for dough, cheese + oven
combination1<- grepl("dough", recipe$instructions,ignore.case = TRUE)
combination2<- grepl("cheese", recipe$instructions,ignore.case = TRUE) & grepl("oven", recipe$instructions,ignore.case = TRUE)
bake <- combination1 | combination2 

##Pasta w/ meat: shows up already on Unrestricted therefore skip

##DairyFree_dataframe
dairyFree_df<- data.frame(bake, roasted)





#Vegan
##Bake: shows up already on dairyFree therefore skip

##Roasted Vegetables: checking for golden + oven + vegetable, golden + oven + broccoli
combination1 <- grepl("golden", recipe$instructions,ignore.case = TRUE) & grepl("oven", recipe$instructions,ignore.case = TRUE) & grepl("vegetable", recipe$instructions,ignore.case = TRUE)
combination2<- grepl("golden", recipe$instructions,ignore.case = TRUE) & grepl("oven", recipe$instructions,ignore.case = TRUE) & grepl("broccoli", recipe$instructions,ignore.case = TRUE)
roastedVegetables <- combination1 | combination2 

##Tossed salad no meat: checking for toss + salad + !chicken + !shrimp + !bacon + !meat + !beef
combination1 <- grepl("toss", recipe$instructions,ignore.case = TRUE) & grepl("salad", recipe$instructions,ignore.case = TRUE) & !grepl("chicken", recipe$instructions,ignore.case = TRUE) & !grepl("shrimp", recipe$instructions,ignore.case = TRUE) & !grepl("bacon", recipe$instructions,ignore.case = TRUE) & !grepl("meat", recipe$instructions,ignore.case = TRUE) & !grepl("beef", recipe$instructions,ignore.case = TRUE)
tossedSaladNoMeat <- combination1

##Pasta no meat: checking for pasta + !chicken + !shrimp + !bacon + !meat + !beef
combination1 <- grepl("pasta", recipe$instructions,ignore.case = TRUE) & !grepl("chicken", recipe$instructions,ignore.case = TRUE) & !grepl("shrimp", recipe$instructions,ignore.case = TRUE) & !grepl("bacon", recipe$instructions,ignore.case = TRUE) & !grepl("meat", recipe$instructions,ignore.case = TRUE) & !grepl("beef", recipe$instructions,ignore.case = TRUE)
pastaNoMeat <- combination1

##Vegan_dataframe
vegan_df<- data.frame(roastedVegetables, tossedSaladNoMeat, pastaNoMeat)



#all dataframe combined
totalDf <- data.frame(unrestricted_df, glutenFree_df, dairyFree_df, vegan_df)

