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

pickExpectedLifeTime <- function(mdVL){
    mdVL$Kjonn[1,3] <- 10 ; # All
    mdVL$AlderX[1,3] <- 10 ; # All
    mdVL$ContentsCode[3,3] <- 1 ; # Expected life years
    mdVL$Tid[1,3] <- 10 ; # All
    mdVL
}


#
# Fetching expected life time data
#

getExpectedLifeTime <- function(){
    
    metaData <- getMDValuesLabels07902() ;     
    mdVL <- pickExpectedLifeTime(metaData) ;
    eQuery <- createSearchFromDF(mdVL) ;
    eData <- getJSONData("07902",eQuery)
    eData$ContentsCode <- NULL # Drop trivial column
    eData
}


#
#  Using melt&cast from reshape to transform to columns
#

transformToColumnsExpectations <- function(eData){
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

getExpectedLifeTimeTable <- function(){

    eData <- getExpectedLifeTime()
    transformToColumnsExpectations(eData)

}



#eLTTable <- getExpectedLifeTimeTable()






