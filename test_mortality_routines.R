options(encoding="UTF-8")

# source("test_mortality_routines.R")
library(httr)
# rjstat is used for converting SSB JSON -> Data frame
library(rjstat)
# jsonlite is used mainly for converting metadata 
library(jsonlite)
# Reshape is used for filtering/transforming/grouping 
library(reshape)

# JSON handling functions and utility 

source("ssb_mortality_table_testing.R")
source("ssb-json-functions.R")
#graphics.off()



# Some testing etc


testAllYearsTables <- function(){ 
    survTable <- getSurvivalTable()
    numDeaths <- getDeathsTable()
    eLTTable <- getExpectedLifeTimeTable()
    probDeath <- getProbabilityOfDeathTable()

}

testAllVarsTables <- function(){ 

    mort2016 <- getMortalityYearsDataTable(yearsPicked=c(51))
    mort20142016 <- getMortalityYearsDataTable(yearsPicked=c(49,50,51))

}










