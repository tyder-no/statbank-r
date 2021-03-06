options(encoding="UTF-8")
suppressPackageStartupMessages(require(pacman))
#p_load(httr, rjstat, mosaic, dplyr,stringr,Quandl,quantmod)
p_load(httr, rjstat, stringr,Quandl,quantmod)
#
#
# source("json-sb-03687-03013.R")
#

library(httr)
# henter rjstat bibliotek for behandling av JSON-stat
library(rjstat)
#library(mosaic)
source("ssb-json-functions.R")

graphics.off()

queryData03687 <- function(){
'{
  "query": [
  {
  "code": "Region",
  "selection": {
  "filter": "item",
  "values": ["0"]  } 
  },  {
  "code": "Kjopegrupper",
  "selection": {
  "filter": "item",
  "values": ["00"]  }
  },  {
  "code": "PetroleumProd",
  "selection": {
  "filter": "item",
  "values": ["03","04"]  }
  },  {
  "code": "ContentsCode",
  "selection": {
  "filter": "item",
  "values": ["Petroleum"]  }
  },  {
  "code": "Tid",
  "selection": {
  "filter": "all",
  "values": ["*"]
  }  }  ],
  "response": {
  "format": "json-stat"
  }
}'
}



petrVolData <- function() {
    
    petrVol <- getJSONData("03687",queryData03687())

    q.bensin <- petrVol[petrVol$PetroleumProd=="03",]
    q.diesel <- petrVol[petrVol$PetroleumProd=="04",]
    
    qbensin <- ts(q.bensin$value, start=1995, frequency = 12)
    qdiesel <- ts(q.diesel$value, start=1995, frequency = 12)

    list(qdiesel=qdiesel,qbensin=qbensin)

}


queryData09654 <- function(){
'{ "query": [   {
      "code": "PetroleumProd",
      "selection": {
        "filter": "item",
        "values": [ "031", "035" ]  }
    },  {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [ "Priser"] }
    },{
      "code": "Tid",
      "selection": {
        "filter": "all",
        "values": ["*"]
      } } ],
  "response": {
    "format": "json-stat" }
}'
}


petrPrisData <- function() {
  petrPris <- getJSONData("09654",queryData09654())

  p.bensin <- petrPris[petrPris$PetroleumProd=="031"&petrPris$Tid>"1994M12",]
  p.diesel <- petrPris[petrPris$PetroleumProd=="035"&petrPris$Tid>"1994M12",]

  pbensin <- ts(p.bensin$value, start=1995, frequency = 12)
  pdiesel <- ts(p.diesel$value, start=1995, frequency = 12)

  list(pdiesel=pdiesel,pbensin=pbensin)

}


oljePrisData <- function(){

    brent = Quandl("FRED/DCOILBRENTEU", collapse="monthly")
    brent = brent[rev(rownames(brent)), , drop=F] 
    min.year=as.numeric(format(min(brent$DATE),"%Y"))
    min.month=as.numeric(format(min(brent$DATE),"%m"))
    pbrent = ts(brent$VALUE, start=c(min.year,min.month), frequency = 12)
    rm(brent,min.month,min.year)

}

dollarKursData <- function() {
    
    options("getSymbols.warning4.0"=FALSE)
    suppressWarnings(getSymbols("USD/NOK", src="oanda", from="1995-01-01", to="1999-12-31"))
    c1 = apply.monthly(USDNOK,mean)
    suppressWarnings(getSymbols("USD/NOK", src="oanda", from="2000-01-01", to="2004-12-31"))
    c2 = apply.monthly(USDNOK,mean)
    suppressWarnings(getSymbols("USD/NOK", src="oanda", from="2005-01-01", to="2009-12-31"))
    c3 = apply.monthly(USDNOK,mean)
    suppressWarnings(getSymbols("USD/NOK", src="oanda", from="2010-01-01", to="2014-12-31"))
    c4 = apply.monthly(USDNOK,mean)
    suppressWarnings(getSymbols("USD/NOK", src="oanda", from="2015-01-01", to="2016-12-31"))
    c5 = apply.monthly(USDNOK,mean)
    min.year=as.numeric(format(min(index(c1)),"%Y"))
    min.month=as.numeric(format(min(index(c1)),"%m"))
    usdnok = ts(rbind(c1,c2,c3,c4,c5), start=c(min.year,min.month), frequency = 12)
    rm(c1,c2,c3,c4,c5,USDNOK,min.month,min.year)

}

avgiftsData <- function() {
    
    taxbensin=c(rep(440,12), rep(449,12), rep(489,12), rep(500,12), rep(517,12), rep(528,12),
                rep(478,6), rep(446,6), rep(454,12), rep(464,12), rep(472,12), rep(481,12),
                rep(489,12), rep(497,12), rep(510,6), rep(515,6), rep(530,12), rep(540,12),
                rep(550,12), rep(558,12), rep(569,12), rep(580,12), rep(582,12), rep(596,6))
    taxdiesel=c(rep(328.5,12), rep(335.5,12), rep(378.5,12), rep(387.5,12), rep(400,12), rep(421,6),
                rep(401,6), rep(352,6), rep(320,6), rep(326,12), rep(333,12),
                rep(339,12), rep(344,12), rep(350,12), rep(356,12), rep(385,6), rep(395,6),
                rep(407,12), rep(414,12), rep(421,12), rep(428,12), rep(436,12), rep(444,12),
                rep(445,12), rep(456,6))
    taxbensin = ts(taxbensin, start=1995, frequency=12)
    taxdiesel = ts(taxdiesel, start=1995, frequency=12)

}

petroPlots1 <- function(pVD,pPD){

    qbensin <- pVD$qbensin ; qdiesel <- pVD$qdiesel ;   
    pbensin <- pPD$pbensin ; pdiesel <- pPD$pdiesel ;   
    
    X11(width=9,height=6)
    par(mfrow=c(1,2))
    plot(qbensin, ylab="Salg av Bilbensin (mill. liter)")
    plot(qdiesel, ylab="Salg av Diesel (mill. liter)")
    
    X11(width=9,height=6)
    par(mfrow=c(1,2))
    plot(pbensin, ylab="Kr/liter Bensin, blyfri 95 oktan")
    plot(pdiesel, ylab="Kr/liter Avgiftspliktig diesel")

# Manglende/feil håndtering av tittel-parametre til plot (main, sub) illustrerer
# svakheter i R sin objektmodell

    par(mfrow=c(1,1))
    X11(width=9,height=6)
#plot(decompose(qbensin),main="Bensin, dekomponering" )
    plot(decompose(qbensin),sub="Bensin, dekomponering" )

    X11(width=9,height=6)
#plot(decompose(qdiesel),main="Diesel, dekomponering") 
    plot(decompose(qdiesel),sub="Diesel, dekomponering") 

    X11(width=9,height=6)
#plot(decompose(qbensin+qdiesel),main="Bensin+Diesel, dekomponering")
    plot(decompose(qbensin+qdiesel),sub="Bensin+Diesel, dekomponering")

    X11()
#plot(decompose(qbensin)$seasonal,main="Bensin+Diesel, dekomp. sesong")
    plot(decompose(qbensin)$seasonal,sub="Bensin+Diesel, dekomp. sesong")
    lines(decompose(qdiesel)$seasonal, col="red")

}



queryData03013 <- function(){
'{ "query": [   {
      "code": "Konsumgrp",
      "selection": {
        "filter": "item",
        "values": [ "TOTAL" ]  }
    },  {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [ "KpiIndMnd"] }
    },{
      "code": "Tid",
      "selection": {
        "filter": "all",
        "values": ["*"]
      } } ],
  "response": {
    "format": "json-stat" }
}'
}


konsumPrisData <- function() {
    konsPris <- getJSONData("03013",queryData03013())

                                        # p.bensin <- petrPris[petrPris$PetroleumProd=="031"&petrPris$Tid>"1994M12",]
                                        # p.diesel <- petrPris[petrPris$PetroleumProd=="035"&petrPris$Tid>"1994M12",]

                                        # pbensin <- ts(p.bensin$value, start=1995, frequency = 12)
                                        # pdiesel <- ts(p.diesel$value, start=1995, frequency = 12)

                                        # list(pdiesel=pdiesel,pbensin=pbensin)

    cpi = read.table("http://data.ssb.no/api/v0/dataset/1086.csv?lang=en", header=TRUE, sep=",", 
                     na.strings="NA", dec=".", strip.white=TRUE, stringsAsFactors = FALSE)
    cpi = filter(cpi, contents=="Consumer Price Index (1998=100)")
    names(cpi) = c("group","time","contents","cpi")
    ym = str_split_fixed(cpi$time, "M", 2)
    cpi$year = as.numeric(ym[,1])
    cpi$month = as.numeric(ym[,2])
    cpi$date = as.Date(paste(cpi$year, cpi$month, 1, sep="-"), "%Y-%m-%d")
    min.year=as.numeric(format(min(cpi$date),"%Y"))
    min.month=as.numeric(format(min(cpi$date),"%m"))
    cpi = ts(cpi$cpi, start = c(min.year,min.month), frequency = 12) 
    rm(ym,min.month,min.year)
    
}




lagTidsSerie <- function() {

    fuel = ts.intersect(pbensin,qbensin,pdiesel,qdiesel,pbrent,usdnok,cpi,taxbensin,taxdiesel)
    plot(fuel)


    date=time(fuel)
    month = as.data.frame(cycle(date)) # trekker ut måneder
    year = as.data.frame(floor(time(date))) # trekker ut år  
    fuel = as.data.frame(fuel)
    fuel$date = as.Date(paste(year$x, month$x, 1, sep="-"), "%Y-%m-%d")
    fuel$year = year$x
    fuel$month = month$x
    fuel = mutate(fuel, t=1:length(fuel$year)) # lager en trendvariabel
    rm(month,year,cpi,date,pbensin,pbrent,pdiesel,qbensin,qdiesel,taxbensin,taxdiesel,usdnok)

    ggplot(fuel, aes(x=date)) + geom_line(aes(y=100*pdiesel/pdiesel[181], col="Dieselpris")) +
        geom_line(aes(y=100*pbensin/pbensin[181], col="Bensinpris")) +
        geom_line(aes(y=100*pbrent/pbrent[181], col="Brentpris")) +
        xlab("") + ylab("Indeks (Januar 2010=100)")

}


# d0 <- testMetaData("03687")
metaData03687 <- function(){
'
 "{\"title\":\"Salg av petroleumsprodukter (mill. liter), etter region, kjøpegruppe, petroleumsprodukt, statistikkvariabel og måned\",\"variables\":[{\"code\":\"Region\",\"text\":\"region\",\"values\":[\"0\",\"01\",\"02\",\"03\",\"04\",\"05\",\"06\",\"07\",\"08\",\"09\",\"10\",\"11\",\"12\",\"14\",\"15\",\"50\",\"16\",\"17\",\"18\",\"19\",\"20\",\"99\"],\"valueTexts\":[\"Hele landet\",\"Østfold\",\"Akershus\",\"Oslo\",\"Hedmark\",\"Oppland\",\"Buskerud\",\"Vestfold\",\"Telemark\",\"Aust-Agder\",\"Vest-Agder\",\"Rogaland\",\"Hordaland\",\"Sogn og Fjordane\",\"Møre og Romsdal\",\"Trøndelag\",\"Sør-Trøndelag (-2017)\",\"Nord-Trøndelag (-2017)\",\"Nordland\",\"Troms - Romsa\",\"Finnmark - Finnmárku\",\"Uoppgitt fylke\"],\"elimination\":true},{\"code\":\"Kjopegrupper\",\"text\":\"kjøpegruppe\",\"values\":[\"00\",\"01\",\"02\",\"03\",\"04\",\"05\",\"06\",\"07\",\"94\",\"93\"],\"valueTexts\":[\"Alle kjøpegrupper\",\"Jordbruk og skogbruk\",\"Fiske og fangst\",\"Industri i alt\",\"Bygg og anlegg\",\"Boliger og næringsbygg\",\"Transport\",\"Offentlig virksomhet\",\"Netto direkte import\",\"Andre\"],\"elimination\":true},{\"code\":\"PetroleumProd\",\"text\":\"petroleumsprodukt\",\"values\":[\"00\",\"03\",\"04\",\"05a\",\"06\",\"10\",\"12\",\"150\",\"120\"],\"valueTexts\":[\"Petroleumsprodukter i alt\",\"Bilbensin\",\"Diesel\",\"Andre mellomdestillat\",\"Parafinprodukter\",\"Tungolje\",\"Smøremiddel\",\"Bitumen\",\"Andre petroleumsprodukt\"],\"elimination\":true},{\"code\":\"ContentsCode\",\"text\":\"statistikkvariabel\",\"values\":[\"Petroleum\"],\"valueTexts\":[\"Salg\"]},{\"code\":\"Tid\",\"text\":\"måned\",\"values\":[\"1995M01\",\"1995M02\",\"1995M03\",\"1995M04\",\"1995M05\",\"1995M06\",\"1995M07\",\"1995M08\",\"1995M09\",\"1995M10\",\"1995M11\",\"1995M12\",\"1996M01\",\"1996M02\",\"1996M03\",\"1996M04\",\"1996M05\",\"1996M06\",\"1996M07\",\"1996M08\",\"1996M09\",\"1996M10\",\"1996M11\",\"1996M12\",\"1997M01\",\"1997M02\",\"1997M03\",\"1997M04\",\"1997M05\",\"1997M06\",\"1997M07\",\"1997M08\",\"1997M09\",\"1997M10\",\"1997M11\",\"1997M12\",\"1998M01\",\"1998M02\",\"1998M03\",\"1998M04\",\"1998M05\",\"1998M06\",\"1998M07\",\"1998M08\",\"1998M09\",\"1998M10\",\"1998M11\",\"1998M12\",\"1999M01\",\"1999M02\",\"1999M03\",\"1999M04\",\"1999M05\",\"1999M06\",\"1999M07\",\"1999M08\",\"1999M09\",\"1999M10\",\"1999M11\",\"1999M12\",\"2000M01\",\"2000M02\",\"2000M03\",\"2000M04\",\"2000M05\",\"2000M06\",\"2000M07\",\"2000M08\",\"2000M09\",\"2000M10\",\"2000M11\",\"2000M12\",\"2001M01\",\"2001M02\",\"2001M03\",\"2001M04\",\"2001M05\",\"2001M06\",\"2001M07\",\"2001M08\",\"2001M09\",\"2001M10\",\"2001M11\",\"2001M12\",\"2002M01\",\"2002M02\",\"2002M03\",\"2002M04\",\"2002M05\",\"2002M06\",\"2002M07\",\"2002M08\",\"2002M09\",\"2002M10\",\"2002M11\",\"2002M12\",\"2003M01\",\"2003M02\",\"2003M03\",\"2003M04\",\"2003M05\",\"2003M06\",\"2003M07\",\"2003M08\",\"2003M09\",\"2003M10\",\"2003M11\",\"2003M12\",\"2004M01\",\"2004M02\",\"2004M03\",\"2004M04\",\"2004M05\",\"2004M06\",\"2004M07\",\"2004M08\",\"2004M09\",\"2004M10\",\"2004M11\",\"2004M12\",\"2005M01\",\"2005M02\",\"2005M03\",\"2005M04\",\"2005M05\",\"2005M06\",\"2005M07\",\"2005M08\",\"2005M09\",\"2005M10\",\"2005M11\",\"2005M12\",\"2006M01\",\"2006M02\",\"2006M03\",\"2006M04\",\"2006M05\",\"2006M06\",\"2006M07\",\"2006M08\",\"2006M09\",\"2006M10\",\"2006M11\",\"2006M12\",\"2007M01\",\"2007M02\",\"2007M03\",\"2007M04\",\"2007M05\",\"2007M06\",\"2007M07\",\"2007M08\",\"2007M09\",\"2007M10\",\"2007M11\",\"2007M12\",\"2008M01\",\"2008M02\",\"2008M03\",\"2008M04\",\"2008M05\",\"2008M06\",\"2008M07\",\"2008M08\",\"2008M09\",\"2008M10\",\"2008M11\",\"2008M12\",\"2009M01\",\"2009M02\",\"2009M03\",\"2009M04\",\"2009M05\",\"2009M06\",\"2009M07\",\"2009M08\",\"2009M09\",\"2009M10\",\"2009M11\",\"2009M12\",\"2010M01\",\"2010M02\",\"2010M03\",\"2010M04\",\"2010M05\",\"2010M06\",\"2010M07\",\"2010M08\",\"2010M09\",\"2010M10\",\"2010M11\",\"2010M12\",\"2011M01\",\"2011M02\",\"2011M03\",\"2011M04\",\"2011M05\",\"2011M06\",\"2011M07\",\"2011M08\",\"2011M09\",\"2011M10\",\"2011M11\",\"2011M12\",\"2012M01\",\"2012M02\",\"2012M03\",\"2012M04\",\"2012M05\",\"2012M06\",\"2012M07\",\"2012M08\",\"2012M09\",\"2012M10\",\"2012M11\",\"2012M12\",\"2013M01\",\"2013M02\",\"2013M03\",\"2013M04\",\"2013M05\",\"2013M06\",\"2013M07\",\"2013M08\",\"2013M09\",\"2013M10\",\"2013M11\",\"2013M12\",\"2014M01\",\"2014M02\",\"2014M03\",\"2014M04\",\"2014M05\",\"2014M06\",\"2014M07\",\"2014M08\",\"2014M09\",\"2014M10\",\"2014M11\",\"2014M12\",\"2015M01\",\"2015M02\",\"2015M03\",\"2015M04\",\"2015M05\",\"2015M06\",\"2015M07\",\"2015M08\",\"2015M09\",\"2015M10\",\"2015M11\",\"2015M12\",\"2016M01\",\"2016M02\",\"2016M03\",\"2016M04\",\"2016M05\",\"2016M06\",\"2016M07\"],\"valueTexts\":[\"1995M01\",\"1995M02\",\"1995M03\",\"1995M04\",\"1995M05\",\"1995M06\",\"1995M07\",\"1995M08\",\"1995M09\",\"1995M10\",\"1995M11\",\"1995M12\",\"1996M01\",\"1996M02\",\"1996M03\",\"1996M04\",\"1996M05\",\"1996M06\",\"1996M07\",\"1996M08\",\"1996M09\",\"1996M10\",\"1996M11\",\"1996M12\",\"1997M01\",\"1997M02\",\"1997M03\",\"1997M04\",\"1997M05\",\"1997M06\",\"1997M07\",\"1997M08\",\"1997M09\",\"1997M10\",\"1997M11\",\"1997M12\",\"1998M01\",\"1998M02\",\"1998M03\",\"1998M04\",\"1998M05\",\"1998M06\",\"1998M07\",\"1998M08\",\"1998M09\",\"1998M10\",\"1998M11\",\"1998M12\",\"1999M01\",\"1999M02\",\"1999M03\",\"1999M04\",\"1999M05\",\"1999M06\",\"1999M07\",\"1999M08\",\"1999M09\",\"1999M10\",\"1999M11\",\"1999M12\",\"2000M01\",\"2000M02\",\"2000M03\",\"2000M04\",\"2000M05\",\"2000M06\",\"2000M07\",\"2000M08\",\"2000M09\",\"2000M10\",\"2000M11\",\"2000M12\",\"2001M01\",\"2001M02\",\"2001M03\",\"2001M04\",\"2001M05\",\"2001M06\",\"2001M07\",\"2001M08\",\"2001M09\",\"2001M10\",\"2001M11\",\"2001M12\",\"2002M01\",\"2002M02\",\"2002M03\",\"2002M04\",\"2002M05\",\"2002M06\",\"2002M07\",\"2002M08\",\"2002M09\",\"2002M10\",\"2002M11\",\"2002M12\",\"2003M01\",\"2003M02\",\"2003M03\",\"2003M04\",\"2003M05\",\"2003M06\",\"2003M07\",\"2003M08\",\"2003M09\",\"2003M10\",\"2003M11\",\"2003M12\",\"2004M01\",\"2004M02\",\"2004M03\",\"2004M04\",\"2004M05\",\"2004M06\",\"2004M07\",\"2004M08\",\"2004M09\",\"2004M10\",\"2004M11\",\"2004M12\",\"2005M01\",\"2005M02\",\"2005M03\",\"2005M04\",\"2005M05\",\"2005M06\",\"2005M07\",\"2005M08\",\"2005M09\",\"2005M10\",\"2005M11\",\"2005M12\",\"2006M01\",\"2006M02\",\"2006M03\",\"2006M04\",\"2006M05\",\"2006M06\",\"2006M07\",\"2006M08\",\"2006M09\",\"2006M10\",\"2006M11\",\"2006M12\",\"2007M01\",\"2007M02\",\"2007M03\",\"2007M04\",\"2007M05\",\"2007M06\",\"2007M07\",\"2007M08\",\"2007M09\",\"2007M10\",\"2007M11\",\"2007M12\",\"2008M01\",\"2008M02\",\"2008M03\",\"2008M04\",\"2008M05\",\"2008M06\",\"2008M07\",\"2008M08\",\"2008M09\",\"2008M10\",\"2008M11\",\"2008M12\",\"2009M01\",\"2009M02\",\"2009M03\",\"2009M04\",\"2009M05\",\"2009M06\",\"2009M07\",\"2009M08\",\"2009M09\",\"2009M10\",\"2009M11\",\"2009M12\",\"2010M01\",\"2010M02\",\"2010M03\",\"2010M04\",\"2010M05\",\"2010M06\",\"2010M07\",\"2010M08\",\"2010M09\",\"2010M10\",\"2010M11\",\"2010M12\",\"2011M01\",\"2011M02\",\"2011M03\",\"2011M04\",\"2011M05\",\"2011M06\",\"2011M07\",\"2011M08\",\"2011M09\",\"2011M10\",\"2011M11\",\"2011M12\",\"2012M01\",\"2012M02\",\"2012M03\",\"2012M04\",\"2012M05\",\"2012M06\",\"2012M07\",\"2012M08\",\"2012M09\",\"2012M10\",\"2012M11\",\"2012M12\",\"2013M01\",\"2013M02\",\"2013M03\",\"2013M04\",\"2013M05\",\"2013M06\",\"2013M07\",\"2013M08\",\"2013M09\",\"2013M10\",\"2013M11\",\"2013M12\",\"2014M01\",\"2014M02\",\"2014M03\",\"2014M04\",\"2014M05\",\"2014M06\",\"2014M07\",\"2014M08\",\"2014M09\",\"2014M10\",\"2014M11\",\"2014M12\",\"2015M01\",\"2015M02\",\"2015M03\",\"2015M04\",\"2015M05\",\"2015M06\",\"2015M07\",\"2015M08\",\"2015M09\",\"2015M10\",\"2015M11\",\"2015M12\",\"2016M01\",\"2016M02\",\"2016M03\",\"2016M04\",\"2016M05\",\"2016M06\",\"2016M07\"],\"time\":true}]}"
'}
    


