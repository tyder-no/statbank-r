options(encoding="UTF-8")
#
# source("ssb-json-tests-0.R")
#
#
#
#

library(httr)
# rjstat is used for converting SSB JSON -> Data frame
library(rjstat)
# jsonlite is used mainly for converting metadata 
library(jsonlite)
# Reshape is used for filtering/transforming/grouping 
library(reshape)
#

source("ssb-json-functions.R")




# Query for the data-containing variables of 05375
getQueryData05375 <- function() {
'{
  "query": [
  {
    "code":"Kjonn",
     "selection": {
    "filter": "all",
    "values":["*"] }
  },{
    "code":"Alder",
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

# Query for the data-containing variables of 05862
getQueryData05862 <- function() {
'{
  "query": [
  {
    "code":"Kjonn",
     "selection": {
    "filter": "all",
    "values":["*"] }
  },{
    "code":"Alder2",
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

md05375 <- getValuesAndLabels("05375")
md05862 <- getValuesAndLabels("05862")
md03013 <- getValuesAndLabels("03013")


query2DF <- function(jsQuery){

    fromJSON(jsQuery)


}

df2Query0 <- function(df) {

    toJSON(df)

}


df2Query <- function(df) {

    toJSON(df)

}





