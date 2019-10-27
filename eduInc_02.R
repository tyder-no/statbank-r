#
#
# source("eduInc_02.R")
#
library(sas7bdat)
library(sqldf) 
library(shiny) 



#> names(basDset)
# [1] "IO_nummer"    "IOs_Komm"     "Antpers"      "fylke"        "ioyrke"      
# [6] "nace3s"       "Ald"          "Ald2"         "Ald3"         "Ald4"        
#[11] "Ald5"         "Ald6"         "Ald7"         "Ald8"         "Kjonn"       
#[16] "Kjonn2"       "Kjonn3"       "Kjonn4"       "Kjonn5"       "Kjonn6"      
#[21] "Kjonn7"       "Kjonn8"       "bu_nus2000_1" "bu_nus2000_2" "bu_nus2000_3"
#[26] "bu_nus2000_4" "bu_nus2000_5" "bu_nus2000_6" "bu_nus2000_7" "bu_nus2000_8"
#[31] "saminnt_1"    "saminnt_2"    "saminnt_3"    "saminnt_4"    "saminnt_5"   
#[36] "saminnt_6"    "saminnt_7"    "saminnt_8"   
#> 


#
# Income, education,age, sex,fylke
prepareData0 <- function() {

    basDset <- read.sas7bdat("utd_innt_utdr_2016.sas7bdat") ;

    ddf1 <- sqldf("SELECT saminnt_1 AS inntekt, Ald AS alder, fylke, Kjonn AS kjonn, bu_nus2000_1 AS utdkode FROM basDset WHERE (bu_nus2000_1>0) ") ;
    ddf2 <- sqldf("SELECT saminnt_2 AS inntekt, Ald2 AS alder, fylke, Kjonn2 AS kjonn, bu_nus2000_2 AS utdkode FROM basDset WHERE (bu_nus2000_2>0) ") ;
    ddf3 <- sqldf("SELECT saminnt_3 AS inntekt, Ald3 AS alder, fylke, Kjonn3 AS kjonn, bu_nus2000_3 AS utdkode FROM basDset WHERE (bu_nus2000_3>0) ") ;
    ddf4 <- sqldf("SELECT saminnt_4 AS inntekt, Ald4 AS alder, fylke, Kjonn4 AS kjonn, bu_nus2000_4 AS utdkode FROM basDset WHERE (bu_nus2000_4>0) ") ;
    ddf5 <- sqldf("SELECT saminnt_5 AS inntekt, Ald5 AS alder, fylke, Kjonn5 AS kjonn, bu_nus2000_5 AS utdkode FROM basDset WHERE (bu_nus2000_5>0) ") ;

    ddf <- rbind(ddf1,ddf2,ddf3,ddf4,ddf5)

    
    ddf
    
}



# Vs 2: Income, education,age, sex,fylke,yrke,naering
 
prepareData <- function(saveRdata=1) {

    basDset <- read.sas7bdat("utd_innt_utdr_2016.sas7bdat") ;

    ddf1 <- sqldf("SELECT saminnt_1 AS inntekt, Ald AS alder, fylke, Kjonn AS kjonn, bu_nus2000_1 AS utdkode, ioyrke AS yrkeskode, nace3s AS naering3s FROM basDset WHERE (bu_nus2000_1>0) ") ;
 
    ddf <- ddf1
    if (saveRdata==1)  save(ddf,file="ddf.Rdata")   
    ddf
    
}


inflateData <- function(toInflate=ddf,iFact=100) {

    infDf <- ddf ;  if (iFact>500) iFact <- 500 ;
    for (i in 1:iFact) infDf <- rbind(infDf,ddf) ;
    infDf

}



#table(basDset$bu_nus2000_1)

#> attach(basDset)
#> sqldf("SELECT saminnt_1 FROM basDset WHERE bu_nus2000_1 = 455234 ")
#ddf <- sqldf("SELECT saminnt_1 FROM basDset WHERE (bu_nus2000_1 = 455234 AND fylke='03') ")
#> hist(ddf$saminnt_1)
#> hist(ddf$saminnt_1,breaks=20)

#ddf <- prepareData() ;
#save(ddf,file="ddf.Rdata")
#load("ddf.Rdata") 


getEduCategory <- function(eduCode,basD=ddf) {

  eSlct <-  fn$sqldf("SELECT * FROM basD WHERE utdkode = $eduCode ")
  eSlct
    
}

plotCategoryInfo <- function(dfrm,eduC,brks=10) {
    
    nObs <- length(dfrm[,1]) ; maxInnt <- max(dfrm$inntekt) ;
    minAld <- max(min(dfrm$alder,na.rm=T),19) ;  maxAld <- max(dfrm$alder,na.rm=T) ;# minAld <- 25 ;
    minInnt <- min(dfrm$inntekt,na.rm=T) ;  maxInnt <- max(dfrm$inntekt,na.rm=T) ;
    hdr <- paste("Utdanningskode: ",eduC," N=",nObs) ;
    eMale <- dfrm[dfrm$kjonn==1,] ;  eFemale <- dfrm[dfrm$kjonn==2,] ; 
    par(mfrow=c(3,2)) ;
    hist(dfrm$inntekt,breaks=brks,col=3,xlab="Inntekt",ylab="Antall",main=hdr)  ;
    plot( eMale$alder,eMale$inntekt,col=4,xlim=c(minAld,1.03*maxAld),ylim=c(minInnt,1.02*maxInnt), xlab="Alder",ylab="Inntekt") ;
    points( eFemale$alder,eFemale$inntekt,col=2) ;
    legend(minAld+1,0.95*maxInnt,col=c(4,2),pch=c(1,1),legend=c("Menn","Kvinner")) ;
   
    boxplot(dfrm$inntekt~dfrm$fylke,col=3,xlab="Fylke",ylab="Inntekt") ;
    boxplot(dfrm$inntekt~dfrm$kjonn,col=c(4,2),xlab="Kjønn",ylab="Inntekt",names=c("Menn","Kvinner")) ;

    boxplot(dfrm$inntekt~dfrm$yrkeskode,col=3,xlab="Yrke",ylab="Inntekt") ;
    boxplot(dfrm$inntekt~dfrm$naering3s,col=4,xlab="Næring",ylab="Inntekt") ;

}

plotEduCategory <- function(eduCode,brks=10,basD=ddf) {

  eSlct <-  fn$sqldf("SELECT * FROM basD WHERE utdkode = $eduCode ")
  plotCategoryInfo(eSlct,eduCode,brks=brks)
    
}  

#slido.com #H586
# 763101 749999 682199 699999 659999 641104 649999
# plotEduCategory(649999,brks=10,basD=ddf)
# png("studie_1.png",width=900,height=1000)
# plotEduCategory(649999,brks=20,basD=ddf)
# dev.off()
# source("eduInc_02.R")
# infDf <- inflateData()
# save(infDf,file="infDf.Rdata")
# plotEduCategory(649999,brks=20,basD=infDf)
