options(encoding="UTF-8")

library(httr)
# rjstat is used for converting SSB JSON -> Data frame
library(rjstat)
# jsonlite is used mainly for converting metadata 
library(jsonlite)
# Reshape is used for filtering/transforming/grouping 
library(reshape)


#library(mosaic)
#source("ssb-json-functions.R")
#graphics.off()

#
##########################################################
########## Fetching data/metadata from SSB ###############
########################################################## 
#
#
#

# Fetches the meta data for a table, with number tableId, as a JSON structure

getMetaData <- function(tableId) {
  getUrl <- paste("http://data.ssb.no/api/v0/no/table/",tableId,sep="")
  d.tmp<-GET(getUrl)
  content(d.tmp,"text")
}

# getRawJSONData - Fetches SB-data by POST-request
# Parameters 
#  tableId: Number  SB-table
#  queryData: JSON-formatted query 
#

getRawJSONData <- function(tableId,queryData) {
#  POST query request
   d.tmp <- POST(paste("http://data.ssb.no/api/v0/no/table/",tableId,sep=""), body = queryData, encode = "json", verbose())
# Returns contents of d.tmp as JSON-formatted text 
   content(d.tmp, "text")
}


# getJSONData - Fetches SB-data by POST-request. Returns a data table for further processing
# Parameters 
#  tableId: Number  SB-table
#  queryData: JSON-formatted query 
#  naming: Two relevant values: id - coding and variable names, label - the label-texts
#

getJSONData <- function(tableId,queryData,naming="id") {
# Fetches the content processed by fromJSONstat
   sbtable <- fromJSONstat(getRawJSONData(tableId,queryData),naming=naming)
# Only dataset is used from sbtable and returned
   ds <- sbtable[[1]]
   ds
}


#
##########################################################
####### Handling metadata, creating queries  #############
########################################################## 




# JSON metadata table structure w/no subtable converted to dataframe by jsonlite
# Suitable for further processing, may e.g. be stored
# Parameter tableId: Number SB-table

getRMetaDataFrame <- function(tableId) {
    mDF <- fromJSON(getMetaData(tableId))
    mDF
}



# From metadata data frame
# Adds column "Slct" for selection marking
# Parameter tableId: Number SB-table

getDFValuesAndLabels <- function(mDF) {
    varNms <- mDF[[2]][[1]]  ; varLbls <- mDF[[2]][[1]] ; varNmb <- length(varNms) ;
    valAndLbl <- list()
    for (i in 1:varNmb) {
        xdfi <- data.frame(mDF[[2]][3][[1]][i],  mDF[[2]][4][[1]][i],0)
        names(xdfi) <- c(varNms[i],paste(varNms[i],"Label",sep=""),"Slct")
        valAndLbl[[varNms[i]]] <- xdfi
    }
    valAndLbl
}


# From JSON metadata table structure w/no subtable converted to dataframe by jsonlite
# Adds column "Slct" for selection marking
# Parameter tableId: Number SB-table

getValuesAndLabels <- function(tableId) {
    mDF <- fromJSON(getMetaData(tableId))
    varNms <- mDF[[2]][[1]]  ; varLbls <- mDF[[2]][[1]] ; varNmb <- length(varNms) ;
    valAndLbl <- list()
    for (i in 1:varNmb) {
        xdfi <- data.frame(mDF[[2]][3][[1]][i],  mDF[[2]][4][[1]][i],0)
        names(xdfi) <- c(varNms[i],paste(varNms[i],"Label",sep=""),"Slct")
        valAndLbl[[varNms[i]]] <- xdfi
    }
    valAndLbl
}


# 
#  Creates search data frame from marked-up meta data frame metaDF 
#
#

createSearchDF <- function(metaDF){

    srchDF <- fromJSON(getQueryData99999())
    srchDFq  <-  srchDF$query
    nQueryRows <- length(srchDFq$code) ;
    
    nVar <- length(metaDF) ; nSrchVar <- 0 ;
    
    for (i in 1:nVar){

        nValues <-  length(metaDF[[i]][,3]) ;
        allSlct <- metaDF[[i]][1,3] ; nmbSlct <-  sum(metaDF[[i]][,3]) ;
        topSlct <- metaDF[[i]][nValues,3]
        
        if (allSlct==10 || nmbSlct>0) {   # Variable included in search
            nSrchVar <-  nSrchVar + 1 ;
            varNm <- names(metaDF[[i]])[1] ;
            srchDFq$code[nSrchVar] <- varNm ;
        }
        if (allSlct==10) {  # All
            srchDFq$selection[nSrchVar,1] <- "all" ;
            srchDFq$selection[nSrchVar,2] <- "*" ;
        }
        else if (nmbSlct>0&topSlct<2) { # Some values, put them in a list
            
            mDi <- metaDF[[i]] ; mDiSlct <- mDi[mDi[,3]==1,1] ;
            mDiSlct <- paste("\"",mDiSlct,"\"",sep="")
            srchDFq$selection[nSrchVar,1] <- "item" ;
            srchDFq$selection[nSrchVar,2] <- paste(mDiSlct,collapse=",") ;
         }
        else if (topSlct>1) { # Newest topSlct values
            srchDFq$selection[nSrchVar,1] <- "top" ;
            srchDFq$selection[nSrchVar,2] <- as.character(topSlct) ;
        }
    }

    srchDF$query <- srchDFq[1:nSrchVar,]
    # print(length(srchDF$query$code))
    fjL <- list(format="json-stat")
    srchDF$response<-fjL
    
    srchDF
     
}


# Patch the generated query by removing the last [] pair
# This way, we can create a correct query from a data frame (really list/frame hybrid) we generate

queryFromDF <- function(df){

    jfJ0 <- toJSON(df)
    jfJ0 <- gsub("\\\\","",jfJ0)
    jfJ0 <- gsub("\\[\"\"","\\[\"",jfJ0)
    jfJ0 <- gsub("\"\"\\]","\"\\]",jfJ0)
    jfJ0 <- gsub("\\]([^]]*)$","\\1",jfJ0)
    jfJ0 <- gsub("\\[([^[]*)$","\\1",jfJ0)
    jfJ0
    
}    

#  createSearchFromDF - Creates  json search query from marked-up metadata 
#  Parameter: markedValueLabelsDF - list of data frames marked for selection

createSearchFromDF <- function(markedValueLabelsDF){

   sDF <- createSearchDF(markedValueLabelsDF)
   queryFromDF(sDF)

}



# Queries for SSB statbank - structure
# Dummy query for new query generation - just  to have enough variables
# This gets rewritten

getQueryData99999 <- function() {
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
   "code":"ContentsCode0",
     "selection": {
    "filter": "all",
    "values":["*"] }
  },{
   "code":"ContentsCode1",
     "selection": {
    "filter": "all",
    "values":["*"] }
  },{
   "code":"ContentsCode2",
     "selection": {
    "filter": "all",
    "values":["*"] }
  },{
   "code":"ContentsCode3",
     "selection": {
    "filter": "all",
    "values":["*"] }
  },{
    "code":"ContentsCode4",
     "selection": {
    "filter": "all",
    "values":["*"] }
  },{
    "code":"ContentsCode5",
     "selection": {
    "filter": "all",
    "values":["*"] }
  },{
   
    "code":"ContentsCode6",
     "selection": {
    "filter": "all",
    "values":["*"] }
  },{
    "code":"ContentsCode7",
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



