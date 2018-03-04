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


getMetaData <- function(tableId) {
  getUrl <- paste("http://data.ssb.no/api/v0/no/table/",tableId,sep="")
  d.tmp<-GET(getUrl)
  content(d.tmp,"text")
}

# getRawJSONData - Henter SB-data vha POST-kall
# Parametre 
#  tableId: Nummer  SB-tabellen
#  queryData: JSON-formatert query 
#

getRawJSONData <- function(tableId,queryData) {
# query-oppslaget med POST
   d.tmp <- POST(paste("http://data.ssb.no/api/v0/no/table/",tableId,sep=""), body = queryData, encode = "json", verbose())
# Henter ut innholdet fra d.tmp som tekst
   content(d.tmp, "text")
}


# getJSONData - Henter SB-data vha POST-kall tjenesten. Returnerer en tabell for videre bearbeiding
# Parametre 
#  tableId: Nummer SB-tabellen
#  queryData: JSON-formatert query 
#  naming: To aktuelle verdier: id - gir koding og variabelnavn, label - gir label-tekstene
#

getJSONData <- function(tableId,queryData,naming="id") {
# Henter ut innholdet  bearbeides av fromJSONstat
   sbtabell <- fromJSONstat(getRawJSONData(tableId,queryData),naming=naming)
# Henter ut kun datasettet fra sbtabell
   ds <- sbtabell[[1]]
# Returnerer datasettet
   ds
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




# From JSON metadata structure converted to dataframe by jsonlite

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
# This way, we can create a correct query from a data frame (really list/fram hybrid) we generate

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






#
##########################################################
########## Transformations on SSB table data #############
########################################################## 

# Dummy query for new query generation - just  to have enough variables

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



