options(encoding="UTF-8")


library(httr)
# henter rjstat bibliotek for behandling av JSON-stat
library(rjstat)

#source("NRdata_1_1.R")
#library(mosaic)
source("ssb-json-functions.R")


graphics.off()



#
####################################################
########## Fetching data from SSB    ###############
#################################################### 
#
#
#


queryData09190<- function(){
'{
  "query":
     [
      {"code":"Makrost",
         "selection":{"filter":"item","values":["koh.nrpriv","koh.nr61_","koh.nr61vare","koh.nr61tjen","koh.nr61L8","koh.nr61L9","koi.nr66_","koo.nroff","koo.nr64_","koo.nr64sivil","koo.nr6401","koo.nr65_","bif.nr83_6","bif.nr83oljroer","bif.pub8X50A","bif.nr83_6fn","bif.nr83_6fnxof","bif.nr83naer","bif.nr8307","bif.nr83rest","bif.nr83indberg","bif.nr83vare","bif.nr8368","bif.nr84_5","biv.nr0890","bil.nr8lu_","bif.nr8_","makrok.nrianv","makrok.nrefn","makrok.nreoff","eks.nrtot","eks.nrtradvare","eks.puboljegs","eks.nrskiplfly","eks.nrtjen","makrok.nrsanv","imp.nrtot","imp.nrtradvare","imp.puboljegs","imp.nrskiplfly","imp.nrtjen","bnpb.nr23_9","bnpb.nr23_9fn","bnpb.nr23oljsj","bnpb.nr23_6fn","bnpb.nr23_6fnxof","bnpb.nr23indberg","bnpb.nr23vare","bnpb.nr23ptjfn","bnpb.nr24_5","bnpb.nr29korr" ]}},
      {"code":"ContentsCode","selection":{"filter":"item","values":["Faste", "Volum","Endringer"]}},
      {"code":"Tid","selection":{"filter":"all","values":["*"]}}
     ],
    "response":{"format":"json-stat"}
}'
}


lagNRData_0<- function(nrData){
  
    tid <- nrData[nrData$Makrost=="koh.nr61_"&nrData$ContentsCode=="Faste",]$Tid
    koh.nr61P <- nrData[nrData$Makrost=="koh.nr61_"&nrData$ContentsCode=="Faste",]$value
    koo.nroffP <- nrData[nrData$Makrost=="koo.nroff"&nrData$ContentsCode=="Faste",]$value
    koh.nr61V <- nrData[nrData$Makrost=="koh.nr61_"&nrData$ContentsCode=="Volum",]$value
    koo.nroffV <- nrData[nrData$Makrost=="koo.nroff"&nrData$ContentsCode=="Volum",]$value

    data.frame(tid, koh.nr61P, koo.nroffP,koh.nr61V,koo.nroffV)
   
} 


hentOgLagNRData_0<- function(){

   nyNRData <- getJSONData("09190",queryData09190())
   lagNRData_0(nyNRData)

}




#
# Alternative queries
#


queryData09190_1 <- function(){
'{
  "query":
     [
      {"code":"Makrost",
         "selection":{"filter":"item","values":["koh.nrpriv","koo.nroff","bif.nr83_6","makrok.nrianv","eks.nrtot","imp.nrtot","bnpb.nr23_9","bnpb.nr23_9fn","bnpb.nr23oljsj"]}},
      {"code":"ContentsCode","selection":{"filter":"item","values":["Faste", "Volum","Endringer"]}},
      {"code":"Tid","selection":{"filter":"all","values":["*"]}}
     ],
    "response":{"format":"json-stat"}
}'
}


queryData09190_2 <- function() {
 '{
  "query": [
    {
      "code": "Makrost",
      "selection": {
        "filter": "item",
        "values": [
          "koh.nr61_",
          "koi.nr66_",
          "koo.nroff",
          "bif.nr83_6",
          "bif.nr83oljroer",
          "bif.pub8X50A",
          "bif.nr83_6fn",
          "bif.nr8307",
          "bif.nr83indberg",
          "bif.nr83vare",
          "bif.nr8368",
          "bif.nr84_5",
          "eks.nrtradvare",
          "eks.puboljegs",
          "eks.nrskiplfly",
          "eks.nrtjen",
          "imp.nrtradvare",
          "imp.puboljegs",
          "imp.nrskiplfly",
          "imp.nrtjen",
          "bnpb.nr23_9",
          "bnpb.nr23_9fn"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Faste",
          "Volum"
          ]
           }
    }
  ],
  "response": {
    "format": "json-stat"
  }
}'
} 



##
#
#
# Metadata-utdrag
#

q0 <-
 '{

"query":
  [
   {"code":"Makrost","values":["koh.nrpriv","koh.nr61_","koh.nr61vare","koh.nr61tjen","koh.nr61L8","koh.nr61L9","koi.nr66_","koo.nroff","koo.nr64_","koo.nr64sivil","koo.nr6401","koo.nr65_","bif.nr83_6","bif.nr83oljroer","bif.pub8X50A","bif.nr83_6fn","bif.nr83_6fnxof","bif.nr83naer","bif.nr8307","bif.nr83rest","bif.nr83indberg","bif.nr83vare","bif.nr8368","bif.nr84_5","biv.nr0890","bil.nr8lu_","bif.nr8_","makrok.nrianv","makrok.nrefn","makrok.nreoff","eks.nrtot","eks.nrtradvare","eks.puboljegs","eks.nrskiplfly","eks.nrtjen","makrok.nrsanv","imp.nrtot","imp.nrtradvare","imp.puboljegs","imp.nrskiplfly","imp.nrtjen","bnpb.nr23_9","bnpb.nr23_9fn","bnpb.nr23oljsj","bnpb.nr23_6fn","bnpb.nr23_6fnxof","bnpb.nr23indberg","bnpb.nr23vare","bnpb.nr23ptjfn","bnpb.nr24_5","bnpb.nr29korr"]},

   {"code":"ContentsCode","values":["Priser","Faste","PriserSesJust","FastePriserSesJust","PrisIndex","Volum","PriserSesJustPros","VolumEndrSesJust","Endringer"]},

   {"code":"Tid","values":["1978K1","1978K2","1978K3","1978K4","1979K1","1979K2","1979K3","1979K4","1980K1","1980K2","1980K3","1980K4","1981K1","1981K2","1981K3","1981K4","1982K1","1982K2","1982K3","1982K4","1983K1","1983K2","1983K3","1983K4","1984K1","1984K2","1984K3","1984K4","1985K1","1985K2","1985K3","1985K4","1986K1","1986K2","1986K3","1986K4","1987K1","1987K2","1987K3","1987K4","1988K1","1988K2","1988K3","1988K4","1989K1","1989K2","1989K3","1989K4","1990K1","1990K2","1990K3","1990K4","1991K1","1991K2","1991K3","1991K4","1992K1","1992K2","1992K3","1992K4","1993K1","1993K2","1993K3","1993K4","1994K1","1994K2","1994K3","1994K4","1995K1","1995K2","1995K3","1995K4","1996K1","1996K2","1996K3","1996K4","1997K1","1997K2","1997K3","1997K4","1998K1","1998K2","1998K3","1998K4","1999K1","1999K2","1999K3","1999K4","2000K1","2000K2","2000K3","2000K4","2001K1","2001K2","2001K3","2001K4","2002K1","2002K2","2002K3","2002K4","2003K1","2003K2","2003K3","2003K4","2004K1","2004K2","2004K3","2004K4","2005K1","2005K2","2005K3","2005K4","2006K1","2006K2","2006K3","2006K4","2007K1","2007K2","2007K3","2007K4","2008K1","2008K2","2008K3","2008K4","2009K1","2009K2","2009K3","2009K4","2010K1","2010K2","2010K3","2010K4","2011K1","2011K2","2011K3","2011K4","2012K1","2012K2","2012K3","2012K4","2013K1","2013K2","2013K3","2013K4","2014K1","2014K2","2014K3","2014K4","2015K1","2015K2","2015K3","2015K4","2016K1","2016K2","2016K3","2016K4","2017K1","2017K2","2017K3","2017K4"],"valueTexts":["1978K1","1978K2","1978K3","1978K4","1979K1","1979K2","1979K3","1979K4","1980K1","1980K2","1980K3","1980K4","1981K1","1981K2","1981K3","1981K4","1982K1","1982K2","1982K3","1982K4","1983K1","1983K2","1983K3","1983K4","1984K1","1984K2","1984K3","1984K4","1985K1","1985K2","1985K3","1985K4","1986K1","1986K2","1986K3","1986K4","1987K1","1987K2","1987K3","1987K4","1988K1","1988K2","1988K3","1988K4","1989K1","1989K2","1989K3","1989K4","1990K1","1990K2","1990K3","1990K4","1991K1","1991K2","1991K3","1991K4","1992K1","1992K2","1992K3","1992K4","1993K1","1993K2","1993K3","1993K4","1994K1","1994K2","1994K3","1994K4","1995K1","1995K2","1995K3","1995K4","1996K1","1996K2","1996K3","1996K4","1997K1","1997K2","1997K3","1997K4","1998K1","1998K2","1998K3","1998K4","1999K1","1999K2","1999K3","1999K4","2000K1","2000K2","2000K3","2000K4","2001K1","2001K2","2001K3","2001K4","2002K1","2002K2","2002K3","2002K4","2003K1","2003K2","2003K3","2003K4","2004K1","2004K2","2004K3","2004K4","2005K1","2005K2","2005K3","2005K4","2006K1","2006K2","2006K3","2006K4","2007K1","2007K2","2007K3","2007K4","2008K1","2008K2","2008K3","2008K4","2009K1","2009K2","2009K3","2009K4","2010K1","2010K2","2010K3","2010K4","2011K1","2011K2","2011K3","2011K4","2012K1","2012K2","2012K3","2012K4","2013K1","2013K2","2013K3","2013K4","2014K1","2014K2","2014K3","2014K4","2015K1","2015K2","2015K3","2015K4","2016K1","2016K2","2016K3","2016K4","2017K1","2017K2","2017K3","2017K4"],"time":true}
 ],
 "response":{"format":"json-stat"}

}'

valueTexts <-
 '   "valueTexts":["Løpende priser (mill. kr)","Faste 2015-priser (mill. kr)","Løpende priser, sesongjustert (mill. kr)","Faste 2015-priser, sesongjustert (mill. kr)","Prisindekser, sesongjustert (2015=100)","Volumendring fra samme periode året før (prosent)","Verdiendring fra foregående kvartal, sesongjustert (prosent)","Volumendring fra foregående kvartal, sesongjustert (prosent)","Prisendring fra samme periode året før (prosent)"]

"valueTexts":["Konsum i husholdninger og ideelle organisasjoner","¬ Konsum i husholdninger","¬¬ Varekonsum","¬¬ Tjenestekonsum","¬¬ Husholdningenes kjøp i utlandet","¬¬ Utlendingers kjøp i Norge","¬ Konsum i ideelle organisasjoner","Konsum i offentlig forvaltning","¬ Konsum i statsforvaltningen","¬¬¬ Konsum i statsforvaltningen, sivilt","¬¬¬ Konsum i statsforvaltningen, forsvar","¬ Konsum i kommuneforvaltningen","Bruttoinvestering i fast realkapital","¬ Utvinning og rørtransport (bruttoinvestering)","¬ Utenriks sjøfart (bruttoinvestering)","¬ Fastlands-Norge (bruttoinvestering)","¬¬ Fastlands-Norge utenom offentlig forvaltning (bruttoinvestering)","¬¬¬ Næringer (bruttoinvestering)","¬¬¬¬ Tjenester tilknyttet utvinning (bruttoinvestering)","¬¬¬¬ Andre tjenester (bruttoinvestering)","¬¬¬¬ Industri og bergverk (bruttoinvestering)","¬¬¬¬ Annen vareproduksjon (bruttoinvestering)","¬¬¬ Boliger (husholdninger) (bruttoinvestering)","¬¬¬ Offentlig forvaltning (bruttoinvestering)","Anskaffelser minus avhendelse av verdigjenstander","Lagerendring og statistiske avvik","Bruttoinvestering i alt","Innenlandsk sluttanvendelse","Etterspørsel fra Fastlands-Norge (ekskl. lagerendring)","Etterspørsel fra offentlig forvaltning","Eksport i alt","¬ Tradisjonelle varer (eksport)","¬ Råolje og naturgass (eksport)","¬ Skip, plattformer og fly (eksport)","¬ Tjenester (eksport)","Samlet sluttanvendelse","Import i alt","¬ Tradisjonelle varer (import)","¬ Råolje og naturgass (import)","¬ Skip, plattformer og fly (import)","¬ Tjenester (import)","Bruttonasjonalprodukt, markedsverdi","Bruttonasjonalprodukt Fastlands-Norge, markedsverdi","Oljevirksomhet og utenriks sjøfart","Bruttoprodukt Fastlands-Norge, basisverdi","¬ Fastlands-Norge utenom offentlig forvaltning (bruttoprodukt)","¬¬ Industri og bergverk (bruttoprodukt)","¬¬ Annen vareproduksjon (bruttoprodukt)","¬¬ Tjenester inkl. boligtjenester (bruttoprodukt)","¬ Offentlig forvaltning (bruttoprodukt)","Produktavgifter og -subsidier"] '



