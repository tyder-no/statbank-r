options(encoding="UTF-8")

# source("ssb_mortality_table_testing.R")
library(httr)
# rjstat is used for converting SSB JSON -> Data frame
library(rjstat)
# jsonlite is used mainly for converting metadata 
library(jsonlite)
# Reshape is used for filtering/transforming/grouping 
library(reshape)


#library(mosaic)
source("ssb-json-functions.R")
#graphics.off()






getQueryData07902 <- function() {
'{
  "query": [
  {
    "code":"Kjonn",
     "selection": {
    "filter": "all",
    "values":["*"] }
  },{
    "code":"AlderX",
     "selection": {
    "filter": "all",
    "values":["*"] }
  },{
    "code":"ContentsCode",
     "selection": {
    "filter": "all",
    "values":["*"] }
  },{
    "code":"Tid",
    "selection": {
    "filter": "all",
    "values":["*"] }
   }   ],
  "response": {
  "format": "json-stat"
  } 
}'
}




getMDValuesLabels07902 <- function(){

    getValuesAndLabels("07902")
    
}


getAllMortalityData07902 <- function(){

    getJSONData("07902",getQueryData07902())
    
}




#
#
#

testMetaDataAndData07902 <- function(){
    metaData <- getMDValuesLabels07902() ; 
    tableData <- getAllMortalityData07902()
    list(metaData=metaData,tableData=tableData)
}

#
# Example of "manual" setup for data extraction 
#

pickMortalityYears <- function(mdVL,yearsPicked=c(47,48,49,50,51)){
    mdVL$Kjonn[1,3] <- 10 ; # All
    mdVL$AlderX[1,3] <- 10 ; # All
    mdVL$ContentsCode[1,3] <- 10 ; # All
    for (i in 1:length(yearsPicked) mdVL$Tid[yearsPicked[i],3] <- 1 ; # Selected
    
    mdVL
}

#
# Fetching expected life time data
#

getMortalityYearsData <- function(yearsPicked){
    
    metaData <- getMDValuesLabels07902() ;     
    mdVL <- pickMortalityYears(metaData,yearsPicked=yearsPicked) ;
    eQuery <- createSearchFromDF(mdVL) ;
    eData <- getJSONData("07902",eQuery)
   # eData$ContentsCode <- NULL # Drop trivial column
    eData
}


#
#  Using melt&cast from reshape to transform to columns
#

transformToColumnsYearsData <- function(eData){
    eData$expLT <- eData$value # To avoid default naming collision on "value"
    eData$value <- NULL ;  # Drop value column
    meltYears <- melt(eData,id=c("Kjonn","AlderX","Tid")) ;
  #  meltExpectation$expLT <- NULL ;  # Drop trivial column
    colExp <- cast(meltYears,Kjonn+AlderX~Tid)

    colExp 
}


#
# Putting it all together - this function will give expected life times in columns
# Grouped by sex in first column  
#

getMortalityDataTable <- function(contCode){

    eData <- getMortalityData(contCode)
    transformToColumnsData(eData)

}

#
# Example of "manual" setup for data extraction 
#

pickMortalityVars <- function(mdVL,contCode=3){
    mdVL$Kjonn[1,3] <- 10 ; # All
    mdVL$AlderX[1,3] <- 10 ; # All
    mdVL$ContentsCode[contCode,3] <- 1 ; # Expected life years
    mdVL$Tid[1,3] <- 10 ; # All
    
    mdVL
}


#
# Fetching expected life time data
#

getMortalityData <- function(contCode){
    
    metaData <- getMDValuesLabels07902() ;     
    mdVL <- pickMortalityVars(metaData,contCode=contCode) ;
    eQuery <- createSearchFromDF(mdVL) ;
    eData <- getJSONData("07902",eQuery)
    eData$ContentsCode <- NULL # Drop trivial column
    eData
}


#
#  Using melt&cast from reshape to transform to columns
#

transformToColumnsData <- function(eData){
    eData$expLT <- eData$value # To avoid default naming collision on "value"
    eData$value <- NULL ;  # Drop value column
    meltExpectation <- melt(eData,id=c("Kjonn","AlderX","Tid")) ;
    meltExpectation$expLT <- NULL ;  # Drop trivial column
    colExp <- cast(meltExpectation,Kjonn+AlderX~Tid)

    colExp 
}


#
# Putting it all together - this function will give expected life times in columns
# Grouped by sex in first column  
#

getMortalityDataTable <- function(contCode){

    eData <- getMortalityData(contCode)
    transformToColumnsData(eData)

}



getSurvivalTable <- function(){

    getMortalityDataTable(1)
  
}

getDeathsTable <- function(){

    getMortalityDataTable(2)
  
}



getExpectedLifeTimeTable <- function(){

    getMortalityDataTable(3)
  
}


getProbabilityOfDeathTable <- function(){

    getMortalityDataTable(4)
  
}



#eLTTable <- getExpectedLifeTimeTable()






