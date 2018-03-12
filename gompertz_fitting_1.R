#
#
# source("gompertz_fitting_1.R")
#
#library(ggplot)
library(dplyr)
library(ggplot2)

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

paramEstimYear <- function(dF,mkPlot=0) {
    
    prepareHazard <- function(hdF,plotcol=4) {
        hdF$lx <- ifelse(hdF$lx>0,hdF$lx,2) 
        lxm <- mutate(hdF, lx = lx/100000, Hx = -log(lx)) ;
        lxhm <- data.frame(hx = diff(lxm$Hx), x = midpoints(lxm$age))
        lfm <- lm(log(hx) ~ I(x - 30), data = filter(lxhm, (x >=30)&(x<=90)))
        lxhmf <- filter(lxhm, (x >= 30)&(x<=90)) %>%  mutate(hf = exp(fitted(lfm)))
      
        if (mkPlot==1) {
           plot(lxhm$x,log(lxhm$hx),ylim=c(-11,0),col=plotcol,xlab="Age x",ylab="log(h(x)")
           abline(lfm$coef[1]-30*lfm$coef[2],lfm$coef[2],col=plotcol,lty=4)
           print(length(lfm$resid))
           plot(31:90,lfm$resid,ylim=c(-0.7,0.7),col=plotcol,xlab="Age x",ylab="Residuals of log(h(x)")
        }
        list(lxhm=lxhm,lfcoef=lfm$coef,lfres=lfm$resid)  
    }
    
    
    yrM <- data.frame(dF$age,dF$maleYear) ;  yrF <- data.frame(dF$age,dF$femYear) ;
    names(yrM) <- c("age","lx") ;  names(yrF) <- c("age","lx") ;
    if (mkPlot==1) {
        X11(height=10,width=12)
        par(mfrow=c(2,2))

    }
    yrHM <- prepareHazard(yrM,plotcol=4) ;
    yrHF <- prepareHazard(yrF,plotcol=2) ;
    
    list(yrHM=yrHM,yrHF=yrHF)
}


paramEstimSeriesOfYears <- function(sT,years=1967:2016,mkPlot=0) {

    #years <- 1967:2016 ;
    resM <- matrix(0,nrow=length(years),ncol=5) ; resM[,1] <- years ;
    

    for (yr in 1:length(years)) {
        yearData <- prepareYear(sT,as.character(yr+years[1]-1)) ;
        pE <- paramEstimYear(yearData) ;
        resM[yr,2] <- pE$yrHM$lfcoef[1] ;  resM[yr,3] <- pE$yrHM$lfcoef[2] ;
        resM[yr,4] <- pE$yrHF$lfcoef[1] ;  resM[yr,5] <- pE$yrHF$lfcoef[2] ;
        
    }
    resM
}


plotMortalityParameters <- function(resM) {

    yrs <- resM[,1] 
    aFitM <- lm(resM[,2]~resM[,1]) ;    bFitM <- lm(resM[,3]~resM[,1]) ;
    aFitF <- lm(resM[,4]~resM[,1]) ;    bFitF <- lm(resM[,5]~resM[,1])
    aCoefM <- aFitM$coef ; bCoefM <- bFitM$coef ;
    aCoefF <- aFitF$coef ; bCoefF <- bFitF$coef ;

    aFitM25<- lm(resM[yrs>1991,2]~resM[yrs>1991,1]) ;    bFitM25 <- lm(resM[yrs>1991,3]~resM[yrs>1991,1]) ;
    aFitF25 <- lm(resM[yrs>1991,4]~resM[yrs>1991,1]) ;    bFitF25 <- lm(resM[yrs>1991,5]~resM[yrs>1991,1])
    aCoefM25 <- aFitM25$coef ; bCoefM25 <- bFitM25$coef ;
    aCoefF25 <- aFitF25$coef ; bCoefF25 <- bFitF25$coef ;    

    X11(width=12,height=7)
    par(mfrow=c(1,2))
   # X11()
    plot(resM[,1],resM[,2],col=4,ylim=c(-9,-6.5),xlab="Year",ylab="Basic mortality")
    points(resM[,1],resM[,4],col=2)
    abline(aCoefM[1],aCoefM[2],col=4)
    abline(aCoefF[1],aCoefF[2],col=2)
    abline(aCoefM25[1],aCoefM25[2],col=4,lty=3)
    abline(aCoefF25[1],aCoefF25[2],col=2,lty=3)  
   # X11()
    plot(resM[,1],resM[,3],col=4,ylim=c(0.09,0.107),xlab="Year",ylab="Aging mortality component")
    points(resM[,1],resM[,5],col=2)
    abline(bCoefM[1],bCoefM[2],col=4)
    abline(bCoefF[1],bCoefF[2],col=2)
    abline(bCoefM25[1],bCoefM25[2],col=4,lty=3)
    abline(bCoefF25[1],bCoefF25[2],col=2,lty=3)  
}
