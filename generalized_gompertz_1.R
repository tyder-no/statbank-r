options(encoding="UTF-8")
                                        #
#
# source("generalized_gompertz_1.R")
#
#library(ggplot)


library(dplyr)
library(ggplot2)

source("ssb-json-functions.R")
source("ssb_mortality_table_testing.R")

graphics.off()


saveAsRExpression <- function(dFrame,fileName,saveDir="../data/") {
    dput(dFrame,paste(saveDir,fileName,".txt",sep=""))
}

loadRExpression <- function(fileName,saveDir="../data/") {
    dget(paste(saveDir,fileName,".txt",sep=""))
}



midpoints <- function(x) (x[-1] + x[-length(x)])/2


#sT <- getSurvivalTable()
# y1967 <- prepareYear(sT,"1967")
# pE <- paramEstimYear(y1967,mkPlot=1) ;

prepareYear <- function(sT,yearStr) {

    femYear <- sT[sT$Kjonn==2,yearStr] ;
    maleYear <- sT[sT$Kjonn==1,yearStr] ; 
    age <- 0:106 ;

    data.frame(age,femYear,maleYear)

}

prepareYearMF <- function(sT,yearStr) {

    femYear <- sT[sT$Kjonn==2,yearStr] ;
    maleYear <- sT[sT$Kjonn==1,yearStr] ; 
    age <- 0:106 ;

    yrM <- data.frame(age,maleYear) ;  yrF <- data.frame(age,femYear) ;
    names(yrM) <- c("age","lx") ;  names(yrF) <- c("age","lx") ;
    list(yrM=yrM,yrF=yrF)

}


prepareHx <- function(hdF) {
    hdF$lx <- ifelse(hdF$lx>0,hdF$lx,2)
    lxm <- mutate(hdF, lx = lx/100000, Hx = -log(lx)) ;
    data.frame(hx = diff(lxm$Hx), x = midpoints(lxm$age))
}

prepareYearMFHx <- function(sT,yearStr) {

    yearMF <- prepareYearMF(sT,yearStr)
    hxF <- prepareHx(yearMF$yrF) ;
    hxM <- prepareHx(yearMF$yrM) ;
   
    list(hxM=hxM,hxF=hxF)

}


estimGompertz3Parm <- function(hxMF) {

    fitted <- function(x0,para) { para[2]*x0^para[1] + (para[1]-1)*log(x0) + para[3] }
    
    sSG3 <- function(para){sqrSumGomp3(para)}

    sqrSumGomp3 <- function(para,loghx=loghx0,x=x0) {
        a <- para[1] ; b <- para[2] ; c <- para[3] ;
        sum((loghx - b*x^a - (a-1)*log(x) - c)^2)
    }

    
    hxhm0  = filter(hxMF$hxM, (x >= regrStart) & (x<=90))
    x0 <- hxhm0$x ; loghx0 <- log(hxhm0$hx) ;
    hfm <-nlm(sSG3,c(1.2,0.07,-7),hessian=T)
    residm <- loghx0 - fitted(x0,hfm$estimate)

    
    hxhf0  = filter(hxMF$hxF, (x >= regrStart) & (x<=90))
    x0 <- hxhf0$x ; loghx0 <- log(hxhf0$hx) ;
    hff <-nlm(sSG3,c(1.2,0.07,-7),hessian=T)
    residf <- loghx0 - fitted(x0,hff$estimate)
  
    list(hfm=hfm,residm=residm,hff=hff,residf=residf) 
}



paramEstimYear2 <- function(dF,mkPlot=0,yr=1967,regrStart=50) {
    
    prepareHazard <- function(hdF,plotcol=4,yr,regrStart=50,plotNum=1) {

        fitted <- function(x0,para) { para[2]*x0^para[1] + (para[1]-1)*log(x0) + para[3] }
        
        sSG3 <- function(para){sqrSumGomp3(para)}

        sqrSumGomp3 <- function(para,loghx=loghx0,x=x0) {
            a <- para[1] ; b <- para[2] ; c <- para[3] ;
            sum((loghx - b*x^a - (a-1)*log(x) - c)^2)
        }

        hdF$lx <- ifelse(hdF$lx>0,hdF$lx,2)
        lxm <- mutate(hdF, lx = lx/100000, Hx = -log(lx)) ;
        lxhm <- data.frame(hx = diff(lxm$Hx), x = midpoints(lxm$age))
        lxhm0  = filter(lxhm, (x >= regrStart) & (x<=90))
        
        x0 <- lxhm0$x ; loghx0 <- log(lxhm0$hx) ;
        
        lfm <-nlm(sSG3,c(1.2,0.07,-7),hessian=T)

        resid <- loghx0 - fitted(x0,lfm$estimate)
          
        if (mkPlot==1) {
            if (plotNum==1) mainTitle <- paste("Year: ",yr) else mainTitle <- " " ;
            plot(lxhm$x,log(lxhm$hx),ylim=c(-11,0),col=plotcol,xlab="Age x",ylab="log(h(x)",main=mainTitle)
                                        # abline(lfm$coef[1]-regrStart*lfm$coef[2],lfm$coef[2],col=plotcol,lty=4)
                                        # print(length(lfm$resid))
            plot((regrStart+1):90,lfm$resid,ylim=c(-0.7,0.7),col=plotcol,xlab="Age x",ylab="Residuals of log(h(x)")
        }
        list(lxhm=lxhm,lfcoef=lfm$estimate,lfres=resid)  
    }
    
    
    yrM <- data.frame(dF$age,dF$maleYear) ;  yrF <- data.frame(dF$age,dF$femYear) ;
    names(yrM) <- c("age","lx") ;  names(yrF) <- c("age","lx") ;
                                        # if (mkPlot==1) {
                                        #     X11(height=10,width=12)
                                        #     par(mfrow=c(2,2))
                                        # }
    yrHM <- prepareHazard(yrM,plotcol=4,yr,regrStart,plotNum=1) ;
    yrHF <- prepareHazard(yrF,plotcol=2,yr,regrStart,plotNum=2) ;
    
    list(yrHM=yrHM,yrHF=yrHF)
}



plotMortalityParameters2 <- function(resM) {

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
    #png(filename='gomp_f1.png') ;
    plot(resM[,1],resM[,2],col=4,ylim=c(-7,-4.5),xlab="Year",ylab="Basic mortality",main="Parameter a in y=a+bx")
    points(resM[,1],resM[,4],col=2)
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

    #dev.copy2eps(device=x11,file='gomp_f1.eps') ;
    #dev.off()
}



processingGompertz2 <- function(sT,mkPlotSlct=1,regrStart=50) {

      if (mkPlotSlct==1) {
    
        X11(height=12,width=12) ; par(mfrow=c(4,4)) ;
        #png(filename='gomp_f2.png') ;  # Doesn't work well with png here
        y1967 <- prepareYear(sT,"1967") ;  pE <- paramEstimYear2(y1967,mkPlot=1,yr=1967,regrStart=regrStart) ;
        y1996 <- prepareYear(sT,"1996") ;  pE <- paramEstimYear2(y1996,mkPlot=1,yr=1996,regrStart=regrStart) ;
        y2006 <- prepareYear(sT,"2006") ;  pE <- paramEstimYear2(y2006,mkPlot=1,yr=2006,regrStart=regrStart) ;
        y2016 <- prepareYear(sT,"2016") ;  pE <- paramEstimYear2(y2016,mkPlot=1,yr=2016,regrStart=regrStart) ;

        #dev.copy2eps(device=x11,file='gomp_f2.eps') ;
        #dev.off()
    }
    
}

#> x <- 30:105
#> p3 <- g3p2017$hff$estimate
#> y <- p3[2]*x^p3[1]+(p3[1]-1)*log(x)+p3[3]
#> plot(x,y)
#> q3 <- g3p2017$hfm$estimate
#> y2 <- q3[2]*x^q3[1]+(q3[1]-1)*log(x)+q3[3]
#> points(x,y2,col=4)
#> hxMF1967 <- prepareYearMFHx(sT,"1967")
#> g3p1967 <- estimGompertz3Parm(hxMF1967)
#> p3 <- g3p1967$hff$estimate
#>  y3 <- p3[2]*x^p3[1]+(p3[1]-1)*log(x)+p3[3]
#>  points(x,y3,pch=2,col=2)
#> q3 <- g3p1967$hfm$estimate
#>  y4 <- q3[2]*x^q3[1]+(q3[1]-1)*log(x)+q3[3]
#>  points(x,y4,pch=2,col=4)


