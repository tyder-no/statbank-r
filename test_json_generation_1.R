options(encoding="UTF-8")


library(httr)
# henter rjstat bibliotek for behandling av JSON-stat
library(rjstat)
library(jsonlite)

#library(mosaic)
#source("test_json_generation_1.R")
source("ssb-json-functions.R")

graphics.off()

#
####################################################
########## Fetching data from SSB    ###############
#################################################### 
#
#
#
getRawMetaData <- function(tableId) {
  getUrl <- paste("http://data.ssb.no/api/v0/no/table/",tableId,sep="")
  GET(getUrl)
 
}


getMetaData <- function(tableId) {
   content(getRawMetaData(tableId),"text")
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
#  naming: To aktuelle verdier: id - gir koding og variabelnavn label - gir label-tekstene
#

getJSONData <- function(tableId,queryData,naming="id") {
# Henter ut innholdet  bearbeides av fromJSONstat
   sbtabell <- fromJSONstat(getRawJSONData(tableId,queryData),naming=naming)
# Henter ut kun datasettet fra sbtabell
   ds <- sbtabell[[1]]
# Returnerer datasettet
   ds
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
############################################
######### Reading data       ###############
############################################ 
#

# The query below is created (manually, for now) from the metadata returned by
# d1 <- getMetaData("05375")

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

getQueryData07902_0 <- function() {
'{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "0",
          "1",
          "2"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat"
  }
}'
}



# Should ideally return input modulo whitespace etc.  Does not for response element in Statbank queries 

invertFromJSON <- function(jsonDat){
   toJSON(fromJSON(jsonDat))
}

# # Should ideally return input data frame

invertToJSON <- function(jsDFrame){
   fromJSON(toJSON(jsDFrame))
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


# 

createSearchDF <- function(metaDF){

    srchDF <- fromJSON(getQueryData07902())
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



# Development version of create-search function

createSearchDF_0 <- function(metaDF){

    srchDF <- fromJSON(getQueryData07902())
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



createSearchFromDF <- function(pmVLDF){

   sDF <- createSearchDF_0(pmVLDF)
   queryFromDF(sDF)

}


patchValLabDF <- function(metaDF) {
# Just a try, works for 05375...    
    metaDF[[1]][1,3] <- 10 ; metaDF[[2]][1,3] <- 1 ;  metaDF[[2]][51,3] <- 1 ;  metaDF[[2]][81,3] <- 1 ;
    metaDF[[4]][31,3] <- 8 ;

    metaDF ; 

}


testCreateSearchDF <- function(){

   mVLDF <- getValuesAndLabels("05375") 
   pmVLDF <- patchValLabDF(mVLDF)
   createSearchFromDF(pmVLDF)

}



# toJSON(fromJSON(getQueryData05375())) - Last section is wrong: ["json-stat"]


cnvFromDF05375 <- '{"query":[{"code":"Kjonn","selection":{"filter":"all","values":["*"]}},{"code":"Alder","selection":{"filter":"all","values":["*"]}},{"code":"Tid","selection":{"filter":"all","values":["*"]}}],"response":{"format":["json-stat"]}}'

# The patched version works

cnvFromDFPatched05375 <- '{"query":[{"code":"Kjonn","selection":{"filter":"all","values":["*"]}},{"code":"Alder","selection":{"filter":"all","values":["*"]}},{"code":"Tid","selection":{"filter":"all","values":["*"]}}],"response":{"format":"json-stat"}}'

# We therefore need to be able to add the response element separately to 



#srchDF05375 <- fromJSON(getQueryData05375())
#metaDF03013 <- fromJSON(getMetaData("03013"))






lagNyLevAlderData <- function(levAld){

   tid <- as.numeric(levAld[levAld$Kjonn=="2"&levAld$Alder=="090",]$Tid)
   k90 <- levAld[levAld$Kjonn=="2"&levAld$Alder=="090",]$value
   k80 <- levAld[levAld$Kjonn=="2"&levAld$Alder=="080",]$value
   k50 <- levAld[levAld$Kjonn=="2"&levAld$Alder=="050",]$value
   m90 <- levAld[levAld$Kjonn=="1"&levAld$Alder=="090",]$value
   m80 <- levAld[levAld$Kjonn=="1"&levAld$Alder=="080",]$value
   m50 <- levAld[levAld$Kjonn=="1"&levAld$Alder=="050",]$value

   data.frame(tid,k50,k80,k90,m50,m80,m90)
   
} 


hentOgLagNyLevAlderData <- function(){

   nyLevAld <- getJSONData("05375",getQueryData05375())
   lagNyLevAlderData(nyLevAld)

}


leveAlderForventingNyereTid <- function(levAld){

    X11(width=8,height=10) ;
    tid <- levAld[,1] 
    plot(tid,50+levAld[,5],ylim=c(60,100),xlim=c(min(tid)-1,max(tid)),type="l",lwd=3,xlab="Tid",ylab="Forventa levealder",col=4)
    axis(2,at=seq(60,100,by=5)) ; grid() ;
    points(tid,80+levAld[,6],type="l",lwd=3,lty=2,col=4)
    points(tid,90+levAld[,7],type="l",lwd=2,lty=3,col=4)
    points(tid,50+levAld[,2],type="l",lwd=3,lty=1,col=2)   
    points(tid,80+levAld[,3],type="l",lwd=3,lty=2,col=2)
    points(tid,90+levAld[,4],type="l",lwd=2,lty=3,col=2)
   
   legend(1990,73,c("Forventa levealder v/50, kvinner","Forventa levealder v/80, kvinner","Forventa levealder v/90, kvinner",
                    "Forventa levealder v/50, menn", "Forventa levealder v/80, menn", "Forventa levealder v/90, menn"),
         lwd=c(3,3,2,3,3,2),lty=c(1,2,3,1,2,3),col=c(2,2,2,4,4,4))  

}

oppDaterNyereTidLeveAlderFigur <- function(){

    levAld <- hentOgLagNyLevAlderData()
    leveAlderForventingNyereTid(levAld) 

}



lagHistLevAlderData <- function(levAld){

   tid <- (levAld[levAld$Kjonn=="2"&levAld$Alder=="00",]$Tid)
   k00 <- levAld[levAld$Kjonn=="2"&levAld$Alder2=="00",]$value
   k80 <- levAld[levAld$Kjonn=="2"&levAld$Alder2=="80",]$value
   k50 <- levAld[levAld$Kjonn=="2"&levAld$Alder2=="50",]$value
   m00 <- levAld[levAld$Kjonn=="1"&levAld$Alder2=="00",]$value
   m80 <- levAld[levAld$Kjonn=="1"&levAld$Alder2=="80",]$value
   m50 <- levAld[levAld$Kjonn=="1"&levAld$Alder2=="50",]$value

   data.frame(tid,k00,k50,k80,m00,m50,m80)
   
} 


hentOgLagHistLevAlderData <- function(){

   histLevAld <- getJSONData("05862",getQueryData05862())
   lagHistLevAlderData(histLevAld)

}


leveAlderForventingHistorisk <- function(levAld){

    X11(width=8,height=10) ;
    tid <- seq(1821,2011,by=5) 
    plot(tid[8:37],0+levAld[8:37,5],ylim=c(40,90),xlim=c(tid[8],tid[37]),type="l",lwd=3,xlab="Tid",ylab="Forventa levealder",col=4)
    axis(2,at=seq(40,90,by=5)) ; grid() ;
    points(tid[8:37],50+levAld[8:37,6],type="l",lwd=3,lty=2,col=4)
    points(tid[8:37],80+levAld[8:37,7],type="l",lwd=2,lty=3,col=4)
    points(tid[8:37],0+levAld[8:37,2],type="l",lwd=3,lty=1,col=2)   
    points(tid[8:37],50+levAld[8:37,3],type="l",lwd=3,lty=2,col=2)
    points(tid[8:37],80+levAld[8:37,4],type="l",lwd=2,lty=3,col=2)
   
   legend(1920,55,c("Forventa levealder v/0, kvinner","Forventa levealder v/50, kvinner","Forventa levealder v/80, kvinner",
                    "Forventa levealder v/0, menn", "Forventa levealder v/50, menn", "Forventa levealder v/80, menn"),
         lwd=c(3,3,2,3,3,2),lty=c(1,2,3,1,2,3),col=c(2,2,2,4,4,4))  

}

oppDaterHistoriskLeveAlderFigur <- function(){

    levAld <- hentOgLagHistLevAlderData()
    leveAlderForventingHistorisk(levAld) 

}




####

# nyLevAld <- getJSONData("05375",getQueryData05375())
# histLevAld <- getJSONData("05862",getQueryData05862())


# Doesn't work well
filterAndData <- function(dataF,dataFStr,condList,dataVar) {

    modCond <- paste(paste(dataFStr,condList,sep="$"),collapse="&")
    print(modCond)
    print(dataF[modCond,])
   dataF[modCond,]$dataVar

}


# condL <- c('Kjonn=="2"','Alder=="090"')
# filterAndData(levAld,"levAld",condL,value)
# condL <- c('Kjonn==2','Alder==090')

# condL <- c('Kjonn=="2"','Alder2=="80"')
# filterAndData(levAld,"levAld",condL,value)
# condL <- c('Kjonn==2','Alder2==80')
# condL <- c('Kjonn==|2|','Alder2==|80|')



#> source("test_json_generation_1.R")
#> md03013 <- getMetaData("03013")
#> mdDF03013 <- fromJSON(md03013)
#> md05375 <- getMetaData("05375")
#> mdDF05375 <- fromJSON(md05375)
#> 
#> mdLV05375 <- getValuesAndLabels("05375")
#> mdLV05375x <- mdLV05375
#> mdLV05375x[[1]]

#dfQ05375x <- fromJSON(getQueryData05375())
#dfQueryPart <- dfQ05375x$query
#dfQueryPart$selection[1,1]
#dfQueryPart$selection[1,1] <- "none"
#dfQueryPart$selection[1,2] <- "**"











