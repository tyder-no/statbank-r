#
#
# source("gompertz_fitting_1.R")
#
#library(ggplot)

library(dplyr)
library(ggplot2)
library(flexsurv)

source("ssb-json-functions.R")
source("ssb_mortality_table_testing.R")

graphics.off()

midpoints <- function(x) (x[-1] + x[-length(x)])/2

usDataHandling <- function() {

    plotMalesFemales <- function() {
        X11()
        ggplot(ushm, aes(x, hx)) + geom_line() + scale_y_log10() +  geom_line(data=ushmf, aes(x, hf), color = "blue")
        X11()
        ggplot(ushf, aes(x, hx)) + geom_line() + scale_y_log10() +  geom_line(data=ushff, aes(x, hf), color = "red")

    }

    us.m <- read.table("http://data.princeton.edu/eco572/datasets/us2002m.dat",header = FALSE, col.names = c("age", "lx"))
    us.f <- read.table("http://data.princeton.edu/eco572/datasets/us2002f.dat",header = FALSE, col.names = c("age", "lx"))

    usm <- mutate(us.m, lx = lx/100000, Hx = -log(lx))
    usf <- mutate(us.f, lx = lx/100000, Hx = -log(lx))


    ushm <- data.frame( hx = diff(usm$Hx), x = midpoints(usm$age))
    lfm <- lm(log(hx) ~ I(x - 30), data = filter(ushm, x > 30))
    ushmf <- filter(ushm, x > 30) %>%  mutate(hf = exp(fitted(lfm)))

    ushf <- data.frame( hx = diff(usf$Hx), x = midpoints(usm$age))
    lff <- lm(log(hx) ~ I(x - 30), data = filter(ushf, x > 30))
    ushff <- filter(ushf, x > 30) %>%  mutate(hf = exp(fitted(lff)))

    plotMalesFemales()
}


transformAndRegress <- function(lxData) {
    lxm <- mutate(lxData, lx = lx/100000, Hx = -log(lx))
    lxhm <- data.frame( hx = diff(lxm$Hx), x = midpoints(lxm$age))
    lfm <- lm(log(hx) ~ I(x - 30), data = filter(lxhm, x > 30))
    lxhmf <- filter(lxhm, x > 30) %>%  mutate(hf = exp(fitted(lfm)))

    lfm$coef
}


estimateCoeffSeries <- function(lxDF) {

    lengthSeries <- 50 ;
    acoeff <- numeric(lengthSeries) ; bcoeff <- numeric(lengthSeries) ;
    
    for (i in 1:lengthSeries) {
        age <- seq(1:110) ;
        lxData <- cbind(lxDF[,i],age)
        gCoefs <- transformAndRegress(lxData)
        acoeff[i]<- gCoefs[1] ; bcoeff[i]<- gCoefs[2] ;


    }
    
    list(acoeff=acoeff,bcoeff=bcoeff)
    
}   

#sT <- getSurvivalTable()
# y1967 <- prepareYear(sT,"1967")
# pE <- paramEstimYear(y1967,mkPlot=1) ;

prepareYear <- function(sT,yearStr) {

    femYear <- sT[sT$Kjonn==2,yearStr] ;
    maleYear <- sT[sT$Kjonn==1,yearStr] ; 
    age <- 0:106 ;

    data.frame(age,femYear,maleYear)

}

paramEstimYear <- function(dF,mkPlot=0,yr=1967,regrStart=50) {
    
    prepareHazard <- function(hdF,plotcol=4,yr,regrStart=50,plotNum=1) {
        hdF$lx <- ifelse(hdF$lx>0,hdF$lx,2) 
        lxm <- mutate(hdF, lx = lx/100000, Hx = -log(lx)) ;
        lxhm <- data.frame(hx = diff(lxm$Hx), x = midpoints(lxm$age))
        lfm <- lm(log(hx) ~ I(x - regrStart), data = filter(lxhm, (x >=regrStart)&(x<=90)))
        lxhmf <- filter(lxhm, (x >= regrStart)&(x<=90)) %>%  mutate(hf = exp(fitted(lfm)))
      
        if (mkPlot==1) {
           if (plotNum==1) mainTitle <- paste("Year: ",yr) else mainTitle <- " " ;
           plot(lxhm$x,log(lxhm$hx),ylim=c(-11,0),col=plotcol,xlab="Age x",ylab="log(h(x)",main=mainTitle)
           abline(lfm$coef[1]-regrStart*lfm$coef[2],lfm$coef[2],col=plotcol,lty=4)
          # print(length(lfm$resid))
           plot((regrStart+1):90,lfm$resid,ylim=c(-0.7,0.7),col=plotcol,xlab="Age x",ylab="Residuals of log(h(x)")
        }
        list(lxhm=lxhm,lfcoef=lfm$coef,lfres=lfm$resid)  
    }
    
    
    yrM <- data.frame(dF$age,dF$maleYear) ;  yrF <- data.frame(dF$age,dF$femYear) ;
    names(yrM) <- c("age","lx") ;  names(yrF) <- c("age","lx") ;
    yrHM <- prepareHazard(yrM,plotcol=4,yr,regrStart,plotNum=1) ;
    yrHF <- prepareHazard(yrF,plotcol=2,yr,regrStart,plotNum=2) ;
    
    list(yrHM=yrHM,yrHF=yrHF)
}


paramEstimSeriesOfYears <- function(sT,years=1967:2017,mkPlot=0,regrStart=50) {

    #years <- 1967:2016 ;
    resM <- matrix(0,nrow=length(years),ncol=5) ; resM[,1] <- years ;
    

    for (yr in 1:length(years)) {
        yearData <- prepareYear(sT,as.character(yr+years[1]-1)) ;
        pE <- paramEstimYear(yearData,regrStart=regrStart) ;
        resM[yr,2] <- pE$yrHM$lfcoef[1] ;  resM[yr,3] <- pE$yrHM$lfcoef[2] ;
        resM[yr,4] <- pE$yrHF$lfcoef[1] ;  resM[yr,5] <- pE$yrHF$lfcoef[2] ;
        
    }
    resM
}



plotMortalityParameters <- function(resM,regrStart=50,lastPerStart=2007) {

    yrs <- resM[,1] ; a0M <- resM[,2] - resM[,3]*regrStart ;  a0F <- resM[,4] - resM[,5]*regrStart ;
    aFitM <- lm(a0M~yrs) ;    bFitM <- lm(resM[,3]~yrs) ;
    aFitF <- lm(a0F~yrs) ;    bFitF <- lm(resM[,5]~yrs)
    aCoefM <- aFitM$coef ; bCoefM <- bFitM$coef ;
    aCoefF <- aFitF$coef ; bCoefF <- bFitF$coef ;

    
    aFitM25<- lm(a0M[yrs>lastPerStart]~resM[yrs>lastPerStart,1]) ;    bFitM25 <- lm(resM[yrs>lastPerStart,3]~resM[yrs>lastPerStart,1]) ;
    aFitF25 <- lm(a0F[yrs>lastPerStart]~resM[yrs>lastPerStart,1]) ;    bFitF25 <- lm(resM[yrs>lastPerStart,5]~resM[yrs>lastPerStart,1])
    aCoefM25 <- aFitM25$coef ; bCoefM25 <- bFitM25$coef ;
    aCoefF25 <- aFitF25$coef ; bCoefF25 <- bFitF25$coef ;    

  
    X11(width=12,height=7)
    par(mfrow=c(1,2))
    #png(filename='gomp_f1.png') ;
    plot(yrs,a0M,col=4,ylim=c(-12.5,-9.5),xlab="Year",ylab="Basic mortality",main="Parameter a in y=a+bx")
    points(yrs,a0F,col=2)
    abline(aCoefM[1],aCoefM[2],col=4)
    abline(aCoefF[1],aCoefF[2],col=2)
    abline(aCoefM25[1],aCoefM25[2],col=4,lty=3)
    abline(aCoefF25[1],aCoefF25[2],col=2,lty=3)  
    legend(1990,-4.75,pch=c(1,1),col=c(4,2),legend=c("Males","Females"))
    
    plot(resM[,1],resM[,3],col=4,ylim=c(0.09,0.12),xlab="Year",ylab="Aging mortality component",main="Parameter b in y=a+bx")
    points(resM[,1],resM[,5],col=2)
    abline(bCoefM[1],bCoefM[2],col=4)
    abline(bCoefF[1],bCoefF[2],col=2)
    abline(bCoefM25[1],bCoefM25[2],col=4,lty=3)
    abline(bCoefF25[1],bCoefF25[2],col=2,lty=3)

    dev.copy2eps(device=x11,file='gomp_f1.eps') ;
                                        #dev.off()
    c(aCoefM25[1],aCoefM25[2],aCoefF25[1],aCoefF25[2],bCoefM25[1],bCoefM25[2],bCoefF25[1],bCoefF25[2])
    
}



processingGompertz <- function(mkPlotSlct=1,regrStart=50) {

   # sT <- getSurvivalTable()
    resM <- paramEstimSeriesOfYears(sT,years=1967:2017,mkPlot=0,regrStart=regrStart) 
    regrC <- plotMortalityParameters(resM)

    if (mkPlotSlct==1) {
    
        X11(height=12,width=12) ; par(mfrow=c(4,4)) ;
        #png(filename='gomp_f2.png') ;  # Doesn't work well with png here
        y1967 <- prepareYear(sT,"1967") ;  pE <- paramEstimYear(y1967,mkPlot=1,yr=1967,regrStart=regrStart) ;
        y1997 <- prepareYear(sT,"1997") ;  pE <- paramEstimYear(y1997,mkPlot=1,yr=1997,regrStart=regrStart) ;
        y2007 <- prepareYear(sT,"2007") ;  pE <- paramEstimYear(y2007,mkPlot=1,yr=2007,regrStart=regrStart) ;
        y2017 <- prepareYear(sT,"2017") ;  pE <- paramEstimYear(y2017,mkPlot=1,yr=2017,regrStart=regrStart) ;

        dev.copy2eps(device=x11,file='gomp_f2.eps') ;
        #dev.off()
    }
    list(regrC=regrC,resM=resM)
}


extraPolDraw <- function(rC,yr,N=100000,regrStart=50)  {

    aM <- rC[1] + rC[2]*yr ;  bM <- rC[5] + rC[6]*yr ;
    rgM <- rgompertz(N,bM,exp(aM))
    aF <- rC[3] + rC[4]*yr ;  bF <- rC[7] + rC[8]*yr ;
    rgF <- rgompertz(N,bF,exp(aF))
 
    EM0 <- mean(rgM) ; EF0 <- mean(rgF) ; 
    EM62 <- mean(rgM[rgM>62]) ;  EM67 <- mean(rgM[rgM>67]) ;
    EF62 <- mean(rgF[rgF>62]) ;  EF67 <- mean(rgF[rgF>67]) ;
   
    EM80 <- mean(rgM[rgM>80]) ;  EM90 <- mean(rgM[rgM>90]) ;
    EF80 <- mean(rgF[rgF>80]) ;  EF90 <- mean(rgF[rgF>90]) ;
    
    list(parm=c(aM,bM,aF,bF), rgM=rgM,rgF=rgF,expects=c(EM0,EM62,EM67,EM80,EM90,EF0,EF62,EF67,EF80,EF90))
}

parmBasedDraw <- function(resM,yr,N=100000,regrStart=50,fYr=1967)  {
    
    aM <- resM[yr-fYr+1,2] ;  bM <- resM[yr-fYr+1,3] ;
    rgM <- rgompertz(N,bM,exp(aM-regrStart*bM))
    aF <- resM[yr-fYr+1,4]  ;  bF <- resM[yr-fYr+1,5] ;
    rgF <- rgompertz(N,bF,exp(aF-regrStart*bF))

    EM0 <- mean(rgM) ; EF0 <- mean(rgF) ; 
    EM62 <- mean(rgM[rgM>62]) ;  EM67 <- mean(rgM[rgM>67]) ;
    EF62 <- mean(rgF[rgF>62]) ;  EF67 <- mean(rgF[rgF>67]) ;
   
    EM80 <- mean(rgM[rgM>80]) ;  EM90 <- mean(rgM[rgM>90]) ;
    EF80 <- mean(rgF[rgF>80]) ;  EF90 <- mean(rgF[rgF>90]) ;
    
    list(parm=c(aM,bM,aF,bF), rgM=rgM,rgF=rgF,expects=c(EM0,EM62,EM67,EM80,EM90,EF0,EF62,EF67,EF80,EF90))
    
}


plotPopHistograms <- function(rC) {

    histPlotM <- function(sYr,year     ){
         mText <- paste("Males",year, "Expect 0/67/90: ")
         mainTitle <- paste(mText,round(sYr$expects[1],1),round(sYr$expects[3],1),round(sYr$expects[5],1),sep=" ") ;
         hist(sYr$rgM,breaks=50,col=4,main=mainTitle,ylab="Deaths",xlab="Age",ylim=c(0,9000)) ;
    }

    histPlotF <- function(sYr,year     ){
         mText <- paste("Females",year, "Expect 0/67/90: ")
         mainTitle <- paste(mText,round(sYr$expects[6],1),round(sYr$expects[8],1),round(sYr$expects[10],1),sep=" ") ;
         hist(sYr$rgM,breaks=50,col=2,main=mainTitle,ylab="Deaths",xlab="Age",ylim=c(0,9000)) ;
    }
    
    X11(height=8,width=12); par(mfrow=c(2,3)) ;
    surv2017 <- extraPolDraw( rC,2017) ;  surv2040 <- extraPolDraw( rC,2040) ;  surv2060 <- extraPolDraw( rC,2060) ;
    histPlotM(surv2017,2017)  ;   histPlotM(surv2040,2040)  ; histPlotM(surv2060,2060)  ;
    histPlotF(surv2017,2017)  ;   histPlotF(surv2040,2040)  ; histPlotF(surv2060,2060)  ;
     
}

plotHistHistograms <- function(resM) {

    histPlotM <- function(sYr,year     ){
         mText <- paste("Males",year, "Expect 0/67/90: ")
         mainTitle <- paste(mText,round(sYr$expects[1],1),round(sYr$expects[3],1),round(sYr$expects[5],1),sep=" ") ;
         hist(sYr$rgM,breaks=50,col=4,main=mainTitle,ylab="Deaths",xlab="Age",ylim=c(0,8000)) ;
    }

    histPlotF <- function(sYr,year     ){
         mText <- paste("Females",year, "Expect 0/67/90: ")
         mainTitle <- paste(mText,round(sYr$expects[6],1),round(sYr$expects[8],1),round(sYr$expects[10],1),sep=" ") ;
         hist(sYr$rgM,breaks=50,col=2,main=mainTitle,ylab="Deaths",xlab="Age",ylim=c(0,8000)) ;
    }
    
    X11(height=8,width=12); par(mfrow=c(2,3)) ;
    surv2017 <- parmBasedDraw( resM,2017) ;   surv1997 <- parmBasedDraw( resM,1997) ;  surv1977 <- parmBasedDraw( resM,1977) ;  
    histPlotM(surv1977,1977)  ; histPlotM(surv1997,1997)  ; histPlotM(surv2017,2017)  ;  
    histPlotF(surv1977,1977)  ; histPlotF(surv1997,1997)  ; histPlotF(surv2017,2017)  ;  
  
     
}



#> pG <- processingGompertz()
#> pG <- processingGompertz()
#> rC <- pG$regrC
#> surv2017 <- extraPolDraw( rC,2017) ; surv2040 <- extraPolDraw( rC,2040) ; surv2060 <- extraPolDraw( rC,2060) ;
#> par(mfrow=c(1,3))
#> hist(surv2017$rgM,breaks=50)
#> hist(surv2040$rgM,breaks=50)
#> hist(surv2060$rgM,breaks=50)
#> surv2017 <- extraPolDraw( rC,2017)
#>  c( mean(surv2017$rgM), mean(surv2017$rgF))
#[1] 82.30068 85.74297  vs expected 80.9  84.3
#> c(mean(surv2040$rgM), mean(surv2040$rgF))
#[1] 87.09236 89.24088
#> surv2060 <- extraPolDraw( rC,2060)
#> c( mean(surv2060$rgM), mean(surv2060$rgF))
#[1] 91.11062 92.34628
#> pb2017 <- parmBasedDraw(resM,2017,N=100000,regrStart=50,fYr=1967)
#> pb2017$expects
#[1] 88.82454 94.35750 90.63094 95.41299
#> pb1997 <- parmBasedDraw(resM,1997,N=100000,regrStart=50,fYr=1967)
#> pb1997$expects
#[1] 86.55767 93.09283 89.10194 94.60010
 
# pb1997 <- parmBasedDraw(resM,1997,N=100000,regrStart=50,fYr=1967)
#> pb1997$expects-62
#[1] 17.78744 19.07724 24.61017 31.04673 21.90984 22.82995 27.05983 32.55004
#> pb2017 <- parmBasedDraw(resM,2017,N=100000,regrStart=50,fYr=1967)
#> pb2017$expects-62
#[1] 21.70059 22.63029 26.84820 32.37957 24.52203 25.22189 28.61228 33.43216
