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

#
##########################################################
########## Transformations on SSB table data #############
########################################################## 






