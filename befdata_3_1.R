options(encoding="UTF-8")


library(httr)
# henter rjstat bibliotek for behandling av JSON-stat
library(rjstat)
library(jsonlite)

#library(mosaic)
#source("befdata_3_1.R")
graphics.off()

#
####################################################
########## Fetching data from SSB    ###############
#################################################### 
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

















