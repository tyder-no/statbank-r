#
# Plotting og framskriving befolkning
# Baserer seg på minimale sett offentlig publiserte data
#
#  20050501-03 Trond Arild Ydersbond (tyder)
#  Oppdatert 20070115-17 tyder
#  Oppdatert 20100109- tyder
#  Omskrevet 20180225 tyder



#source("framskr_4_1.R")
#source("befdata_2_1.R")




# levealder.plot()
# dodsr.plot()
#> fodtkohort.plot()
#> source("framskr_1.R")
#> expect.plot()
#> dodsr.plot()

#
#
#
#
  graphics.off()
#

# Needs the following data
# menn.dodsr
# kvinn.dodsr
# 
fodt.kohort40 <- rbind(c(1935,1940,1945,1950,1955,1960,1965),c(2.42,2.35,2.19,2.08,2.04,2.1,2.085))



############################################
######### Interpolating functions ###############
############################################ 

# dodsr.interpolate
# Constructs 1-year rates from 5 yr averages by interpolation
# Assumes i>=2 - meaningless otherwise
# Returns array[1:maxage] with interpolated rates, given array avgrat
# of averages
#
dodsr.interpolate <- function(avgrat,maxavg=90,maxage=110) {

    nclass <- length(avgrat) + 1 ;
    interp1yr <- numeric(maxage) ; avgratA <- c(avgrat,avgrat[nclass-1])
    #print(avgratA)
   
   # Starting, takes end point as average
    x10 <- (avgratA[1] + avgratA[2])/2 ; d1 <- (x10-avgratA[1])/5 ; 
            x0 <-  x10 - 10*d1 ; d2 <- d1 ;
           
    for (j in 1:3) { 
       interp1yr[j] <- x0 + 2*(j-1)*d1 ;  interp1yr[j+3] <- x0 + 5*d1 + 2*(j-1)*d2 ;     
    }
    for (i in 2:(nclass-1)) {
       xAvg <- avgratA[i]
       print(i) ; print(xAvg) ;
       if ((xAvg>=avgratA[i-1])&&(xAvg<=avgratA[i+1])||
                (xAvg<=avgratA[i-1])&&(xAvg>=avgratA[i+1])) { # Monotone
          x0 <- x10 ; d1 <- (xAvg - x0)/5 ; d2 <- d1 ; x10 <- x0 + 10*d1 ;  
       }
       else { # Max- or min-point
          x0 <- x10 ;    x10 <- (xAvg + avgratA[i+1])/2 ; x5 <- 3*xAvg - x0 - x10 ;
          d1 <- (x5 - x0)/5 ; d2 <- (x10 - x5)/2 ;
       }
       for (j in 1:3) { 
          interp1yr[5*(i-1)+j] <- x0 + 2*(j-1)*d1 ;  
          interp1yr[5*(i-1)+j+3] <- x0 + 5*d1 + 2*(j-1)*d2 ;     
       }
    }   
   interp1yr
}

# dodsr.quadinterpolate
# Constructs 1-year rates from 5 yr averages by quadratic interpolation
# Conditions: Values, derivative and integral on intervals
# Assumes i>=2 - meaningless otherwise
# Returns array interp1yr[maxage] of interpolated values, given array avgrat

dodsr.quadinterpolate <- function(avgrat,maxavg=90,maxage=111,initval=320,initchng=-200) {

    nclass <- length(avgrat) + 1 ; 
    interp1yr <- numeric(maxage) ; avgratA <- c(avgrat,avgrat[nclass-1])
    #print(avgratA)

    B <- c(avgrat,32000,40000) - c(0,avgrat,32000) ; C <- rep(5/6,nclass) ; A <- C ; D <- rep(10/3,nclass) ;
    H <- rep(5,nclass) ;
  #  print(list(B,A,D,C))
   
  # Solving a tridiagonal system with A,D,C,B for Xi = bi. 
  #   
   b <- numeric(nclass) ;     a <- numeric(nclass) ;     c <- numeric(nclass) ;  
  # Treat first category separately, to get right initial conditions 
   b[1] <- initchng ; c[1] <- initval ; a[1] <- (B[1] - b[1]*5 - c[1])/25 ; 
   B[2] <- B[2] - A[2]*b[1]

 # Make it upper diagonal 
   for (i in 3:(nclass-1)) {
       m <- A[i]/D[i-1] ; D[i] <- D[i] - m*C[i-1] ; B[i] <- B[i] - m*B[i-1] ;
   }
  # Solve, b first
    b[nclass-1] <- B[nclass-1]/D[nclass-1] ;
    for (i in (nclass-2):2) {
       b[i] <- (B[i] - C[i]*b[i+1])/D[i] ;
    }

   b[nclass] <- 2000 ;
  # Then a and c
    for (i in (nclass-1):2) {
       a[i] <- (b[i+1] - b[i])/(2*H[i]) ;
       c[i] <- avgratA[i] - a[i]*H[i]*H[i]/3 - b[i]*H[i]/2 ;
    }

  #print(list(a,b,c))  ;

  # Make the intrapolations 
    for (i in 1:(nclass-1)) {
       for (j in 0:4) {
         iptmp <- a[i]*j*j + b[i]*j + c[i]  ;
         if (iptmp < 0) iptmp <- 10 ;
         interp1yr[5*(i-1)+j+1] <- iptmp ;      
       }  
    }   
 
    for (i in (5*nclass-4):(maxage)) {  interp1yr[i] <- 32000  }  
  #  interp1yr[maxage] <- 100000 ; 
   # plot(c)
   interp1yr
}


# Testing the interpolations
makeinterp <- function() { 
   kvinn.interp71 <- dodsr.quadinterpolate(kvinn.dodsr[,1])/100000
   kvinn.interp05 <- dodsr.quadinterpolate(kvinn.dodsr[,10])/100000
   menn.interp71 <- dodsr.quadinterpolate(menn.dodsr[,1])/100000
   menn.interp05 <- dodsr.quadinterpolate(menn.dodsr[,10])/100000
}


# Just for-loop....
for.test <- function(n1=2,n2=6) {
  ssum <- 0
  for (i in n2:n1) { print(i) ; ssum <- ssum + i ; }
  ssum
}


longtimelimits <- function(inprates,limitfactor=0.33 ) {
  inprates*limitfactor
}

#
#
computeKn <- function(t0,t1,rt0,rt1,rL) {
 
  if (rt0<rt1) rt0 <- rt1 ; # Makes it constant if increasing 
  if (rt0<rL) rt0 <- rt1 ; # Makes it constant  if too low
  Kn <- 1/(t1-t0)*log((rt1-rL)/(rt0-rL))
  Kn
}
 
#
#
computerates <- function(t1,t0,inpr0,inpr1,limfact=0.33) {

   len <- length(inpr0) ;  rates <- numeric(len) ;
   ltL <- longtimelimits(inpr1,limitfactor=limfact)
   for (i in 1:len) {
      rates[i] <- computeKn(t1,t0,inpr0[i],inpr1[i],ltL[i]) 
   }

   dnE <- inpr0 - ltL ; 
   list(rates,dnE,ltL)

}

# makeallrates - returns a list of male and female rate lists
# Computes with default years 1971 and 2005, and asymptotes 0.2 of 2005 rates
#
makeallrates <- function(yr1=1971,yr2=2008,col1=1,col2=10,limfact=0.1) {
   kvinn.interp71 <- dodsr.quadinterpolate(kvinn.dodsr[,col1])/100000
   kvinn.interp05 <- dodsr.quadinterpolate(kvinn.dodsr[,col2])/100000
   menn.interp71 <- dodsr.quadinterpolate(menn.dodsr[,col1])/100000
   menn.interp05 <- dodsr.quadinterpolate(menn.dodsr[,col2])/100000

   kRates <- computerates(yr1,yr2,kvinn.interp71,kvinn.interp05,limfact=limfact)
   mRates <- computerates(yr1,yr2,menn.interp71,menn.interp05,limfact=limfact)
   list(mRates,kRates)
}

# compdead - computes the dead  given a year, and rate constants 
# pN : population t1: Actual time t0: Start time  kN: exponential coeff 
# D0: Diff long time and start time rates DL: Long time rates
#
compdead <- function(pN,t2,kN,D0,DL,t0=1971,t1=2008,k=0) {
   tD1 <- (D0*exp(kN*(t1-t0))+DL)   
   tD2 <- (D0*exp(kN*(t2-t0))+DL)
   pN*(k*tD1 + (1-k)*tD2)
}



#
# rates.estim - Estimates the rate  given a year, and rate constants 
# t: Actual time t0: Start time  kN: exponential coeff 
# D0: Diff long time and start time rates DL: Long time rates
#
rates.estim <- function(t,kN,D0,DL,t0=1971) {
 
   tD <- D0*exp(kN*(t-t0))+DL
   tD
}


#
#
#
makeexpect <- function(startyr=2009,endyr=2050,yr1=1971,yr2=2009,yrinterv=10,minald=1,col1=1,col2=10,limfact=0.1,k=0,fdgM=0.2,fdgK=0.0) {
 
   if (startyr>endyr) startyr <- endyr ; # To get it going right
   allrates <- makeallrates(yr1=yr1,yr2=yr2,col1=col1,col2=col2,limfact=limfact) ;
   kDt <- allrates[[2]][1][[1]] ;    kD0 <- allrates[[2]][2][[1]] ;  
   kDL <- allrates[[2]][3][[1]] ;  mDL <- allrates[[1]][3][[1]] ;            
   mDt <- allrates[[1]][1][[1]] ;    mD0 <- allrates[[1]][2][[1]] ;            
   maxald <- length(kDt) ; if (minald>maxald) minald <- maxald ;
   
   #i <- 1 ;
   yr <- startyr ;  
   yrArr <- 0 ; xpctKA <- 0 ; xpctMA <- 0 ;

   while (yr <=endyr) {

     pop0 <- 100000  ; deadArrK <- numeric(maxald) ;   popK <- pop0 ;  sumLK <- 0 ;
     pop0 <- 100000  ; deadArrM <- numeric(maxald) ;   popM <- pop0 ;  sumLM <- 0 ;
 
     for (i in minald:maxald) {
        dI <- compdead(popK,yr,kDt[i],kD0[i],kDL[i],t0=yr1,t1=startyr,k=k)
        popK <- popK - dI ; deadArrK[i] <- dI  ;
        sumLK <- sumLK + dI*(i-1)  
     }
     expctLK <- sumLK/pop0+0.5-fdgK ;

     for (i in minald:maxald) {
        dI <- compdead(popM,yr,mDt[i],mD0[i],mDL[i],t0=yr1,t1=startyr,k=k)
        popM <- popM - dI ; deadArrM[i] <- dI  ;
        sumLM <- sumLM + dI*(i-1)  
     }
     expctLM <- sumLM/pop0+0.5-fdgM ;
         
     yrArr <- c(yrArr,yr) ; xpctKA <- c(xpctKA,expctLK) ; xpctMA <- c(xpctMA,expctLM) ; 
     yr <- yr + yrinterv ;
   }
   # Behead
   lY <- length(yrArr) ;
   yrArr <- yrArr[2:lY] ;  xpctKA <- xpctKA[2:lY] ;  xpctMA <- xpctMA[2:lY] ; 
 
   rbind(yrArr,xpctKA,xpctMA)

}


#
#
ssb.m02trend <- function() {

  yr <- c(2004,2050,2100)
  lK <- c(82.0,88.1,94.8)
  lM <- c(77.3,84.2,92.1)
  rbind(yr,lK,lM)

}

ssb.m09expect <- function(yr) {

  eK <- 82.95 + (yr-2008)/42*(90.3 - 82.95)
  eM <- 78.31 + (yr-2008)/42*(87.1 - 78.31)


  c(eK,eM)
}

ssb.m09trend <- function(alt=2) {
  
  yr <- c(2008,2060,2100)
  if (alt==1) {  # low 
    lK <- c(82.95,87.1,92.8)
    lM <- c(78.3,84,90)
  } 
  else if (alt==3) { # high 
    lK <- c(82.95,93.4,98)
    lM <- c(78.3,90.2,95.7)
  }
  else {
    lK <- c(82.95,90.3,95.8)
    lM <- c(78.3,87.1,93.1)
  }

  rbind(yr,lK,lM)

}

ssb.m80ptrend <- function() {

  yr <- c(2007,2050,2100)
  lK <- c(9.24,13.5,17.36)
  lM <- c(7.39,10.7,13.6)
  rbind(yr,lK,lM)

}

 f2002rates <- function(ald=70,kj=1,td=c(2002,2003,2004,2005,2006,2007,2008,2010,2020,2030,2049)) {
 
     v1 <- ifelse(ald<90,5,10) ;  v2 <- ifelse(ald<90,5,9) ; 
     lf2002 <- numeric(length(td)) ;
     for (i in 1:length(td)) {
        if (kj==1) {
          tm1 <- menn.f2002[(ald+1):(ald+v1),td[i]-2001] ; p1 <- sum(tm1[1:v1]) ;
          tm2 <- menn.f2002[(ald+2):(ald+v2+1),td[i]-2000] ; p2 <- sum(tm2[1:v2])
        }
        else  {
          tm1 <- kvinn.f2002[(ald+1):(ald+v1),td[i]-2001] ; p1 <- sum(tm1[1:v1]) ;
          tm2 <- kvinn.f2002[(ald+2):(ald+v2+1),td[i]-2000] ; p2 <- sum(tm2[1:v2])
        } 

        lf2002[i] <- log(1-p2/p1)

      }   
      lf2002
   }


 f2009rates <- function(ald=70,kj=1,td=c(2010,2012,2015,2020,2025,2030,2045,2059)) {
 
     v1 <- ifelse(ald<90,5,15) ;  v2 <- ifelse(ald<90,5,15) ; 
     lf2009 <- numeric(length(td)) ;
     for (i in 1:length(td)) {
        if (kj==1) {
          tm1 <- menn.f2009[(ald+1):(ald+v1),td[i]-2008] ; p1 <- sum(tm1[1:v1]) ;
          tm2 <- menn.f2009[(ald+2):(ald+v2+1),td[i]-2007] ; p2 <- sum(tm2[1:v2])
        }
        else  {
          tm1 <- kvinn.f2009[(ald+1):(ald+v1),td[i]-2008] ; p1 <- sum(tm1[1:v1]) ;
          tm2 <- kvinn.f2009[(ald+2):(ald+v2+1),td[i]-2007] ; p2 <- sum(tm2[1:v2])
        } 

        lf2009[i] <- log(1-p2/p1)

      }   
      lf2009
   }

f2009rate <- function(ald=70,kj=1,td=2010) {
 
     v1 <- ifelse(ald<90,5,15) ;  v2 <- ifelse(ald<90,5,15) ; 
     
     if (kj==1) {
          tm1 <- menn.f2009[(ald+1):(ald+v1),td-2008] ; p1 <- sum(tm1[1:v1]) ;
          tm2 <- menn.f2009[(ald+2):(ald+v2+1),td-2007] ; p2 <- sum(tm2[1:v2])
        }
        else  {
          tm1 <- kvinn.f2009[(ald+1):(ald+v1),td-2008] ; p1 <- sum(tm1[1:v1]) ;
          tm2 <- kvinn.f2009[(ald+2):(ald+v2+1),td-2007] ; p2 <- sum(tm2[1:v2])
        } 

        lf2009 <- log(1-p2/p1)

         
      lf2009
   }



 #  kvinn.interp71 <- dodsr.quadinterpolate(kvinn.dodsr[,1])/100000
 #  kvinn.interp05 <- dodsr.quadinterpolate(kvinn.dodsr[,10])/100000
 #  menn.interp71 <- dodsr.quadinterpolate(menn.dodsr[,1])/100000
 #  menn.interp05 <- dodsr.quadinterpolate(menn.dodsr[,10])/100000


############################################
######### Plotting functions ###############
############################################ 


#
#
#
#
hist.expect.plot <- function(){

   #X11(width=9,height=9) ;

 #  plot(tidsser.2015,menn.leveald.2015[1,],ylim=c(40,95),xlim=c(1860,2020),type="l",lwd=3,xlab="Ar",ylab="Forventa levealder",col=4)
   plot(tidsser.2015,menn.leveald.2015[1,],ylim=c(40,95),xlim=c(1860,2020),type="l",lwd=3,xlab="Tid",ylab="Forventa levealder",col=4)
   
   points(tidsser.2015,50+menn.leveald.2015[8,],type="l",lwd=3,lty=2,col=4)
   points(tidsser.2015,80+menn.leveald.2015[11,],type="l",lwd=2,lty=3,col=4)
   points(tidsser.2015,kvinn.leveald.2015[1,],type="l",lwd=3,lty=1,col=2)
   points(tidsser.2015,50+kvinn.leveald.2015[8,],type="l",lwd=3,lty=2,col=2)
   points(tidsser.2015,80+kvinn.leveald.2015[11,],type="l",lwd=2,lty=3,col=2)
    
   legend(1925,58,c("Forventa levealder, kvinner","Forventa levealder v/50, kvinner","Forventa levealder v/80, kvinner",
                    "Forventa levealder, menn", "Forventa levealder v/50, menn", "Forventa levealder v/80, menn"),
 
         lwd=c(3,3,2,3,3,2),lty=c(1,2,3,1,2,3),col=c(2,2,2,4,4,4))  

   

}

newhist.expect.plot <- function(){

  # X11(width=8,height=10) ;

 #  plot(tidsser.2015,menn.leveald.2015[1,],ylim=c(40,95),xlim=c(1860,2020),type="l",lwd=3,xlab="Ar",ylab="Forventet levealder",col=4)
   plot(tidsser.2016,50+menn.leveald.2016[4,],ylim=c(60,100),xlim=c(1985,2016),type="l",lwd=3,xlab="Tid",ylab="Forventa levealder",col=4)
    points(tidsser.2016,80+menn.leveald.2016[7,],type="l",lwd=3,lty=2,col=4)
    points(tidsser.2016,90+menn.leveald.2016[8,],type="l",lwd=2,lty=3,col=4)
    points(tidsser.2016,50+kvinn.leveald.2016[4,],type="l",lwd=3,lty=1,col=2)   
    points(tidsser.2016,80+kvinn.leveald.2016[7,],type="l",lwd=3,lty=2,col=2)
    points(tidsser.2016,90+kvinn.leveald.2016[8,],type="l",lwd=2,lty=3,col=2)
    
 #  points(tidsser.2015,50+menn.leveald.2015[8,],type="l",lwd=3,lty=2,col=4)
 #  points(tidsser.2015,80+menn.leveald.2015[11,],type="l",lwd=2,lty=3,col=4)
 #  points(tidsser.2015,kvinn.leveald.2015[1,],type="l",lwd=3,lty=1,col=2)
 #  points(tidsser.2015,50+kvinn.leveald.2015[8,],type="l",lwd=3,lty=2,col=2)
 #  points(tidsser.2015,80+kvinn.leveald.2015[11,],type="l",lwd=2,lty=3,col=2)
    
   legend(1990,73,c("Forventa levealder v/50, kvinner","Forventa levealder v/80, kvinner","Forventa levealder v/90, kvinner",
                    "Forventa levealder v/50, menn", "Forventa levealder v/80, menn", "Forventa levealder v/90, menn"),
 
         lwd=c(3,3,2,3,3,2),lty=c(1,2,3,1,2,3),col=c(2,2,2,4,4,4))  

   

}


hist.expect.pngplot <- function(){
    
    X11(width=9,height=9) ;
    hist.expect.plot() ;
  #  jpeg(filename ='historisk_forventa_levealder_1.jpg', width = 1000, height = 1000, quality=85, pointsize = 12, bg = "white")
    png(filename='historisk_forventa_levealder_1.png') ;
    hist.expect.plot() ; 
    dev.off() ;

}


newhist.expect.pngplot <- function(){
    
    X11(width=9,height=10) ;
    newhist.expect.plot() ;
    png(filename='de_eldste_forventa_levealder_1.png') ;
    newhist.expect.plot() ; 
    dev.off() ;

}

#
#
#
#
expect.plot <- function(yr1=1973) {

   #dev.set(3) ; 
   X11(width=9,height=7) ;
   eK <-  makeexpect(startyr=2009,endyr=2100,yr1=yr1,yr2=2008,yrinterv=10,minald=1,col1=1,col2=10,limfact=0.1) 
 
   eK80p <-  makeexpect(startyr=2009,endyr=2100,yr1=yr1,yr2=2008,yrinterv=10,minald=81,col1=1,col2=10,limfact=0.1) 

   eSSB <- ssb.m09trend()
   e80pSSB <- ssb.m80ptrend()

   plot(eK[1,],eK[2,],ylim=c(40,100),xlim=c(1870,2100),type="l",lwd=3,xlab="Ar",ylab="Forventet levealder",col=3)
   points(eK[1,],eK[3,],type="l",lty=2,lwd=3,col=3)
   points(eSSB[1,],eSSB[2,],type="l",lty=1,col=2,lwd=2)
   points(eSSB[1,],eSSB[3,],type="l",lty=2,col=2,lwd=2)
   #points(seq(1996,2008),menn.leveald[29:41,2],type="l",lwd=1,lty=1,col=3)
   #points(seq(1996,2008),kvinn.leveald[29:41,2],type="l",lwd=1,lty=2,col=3)
   #points(seq(1996,2008),menn.leveald[29:41,2],type="l",lwd=1,lty=2,col=3)
   #points(seq(1996,2008),kvinn.leveald[29:41,2],type="l",lwd=1,lty=1,col=3)
   #points(alle.leveser[23:27,1],alle.leveser[23:27,2],type="l",lwd=1,lty=2,col=3)
   #points(alle.leveser[23:27,1],alle.leveser[23:27,13],type="l",lwd=1,lty=1,col=3)
   points(alle.leveser[,1],alle.leveser[,13],type="l",lwd=2,lty=1,col=1)
   points(alle.leveser[,1],alle.leveser[,2],type="l",lwd=2,lty=2,col=1)
   points(alle.leveser[,1],alle.leveser[,23]+80,type="l",lty=1,col=1)
   points(alle.leveser[,1],alle.leveser[,12]+80,type="l",lty=2,col=1)
   points(e80pSSB[1,],e80pSSB[2,]+80,type="l",lty=1,col=2,lwd=1)
   points(e80pSSB[1,],e80pSSB[3,]+80,type="l",lty=2,col=2,lwd=1)
   points(eK80p[1,],eK80p[2,],type="l",lty=1,lwd=1,col=3)
   points(eK80p[1,],eK80p[3,],type="l",lty=2,lwd=1,col=3)
 


   legend(1925,55,c("Framskrivning fra observerte dodsrater, kvinner","Framskrivning fra observerte dodsrater, menn",
         "SSB framskriving 09, kvinner","SSB framskriving 09, menn"),
         lwd=c(3,3,2,2),lty=c(1,2,1,2),col=c(3,3,2,2))  

}

#
#
#
fodtkohort.plot <- function() {

  #X11() ; 
  dev.set(4) ;
  plot(fodt.kohort40[1,],fodt.kohort40[2,],ylim=c(1.4,2.5),
       xlab="Fodselsaar",ylab="Barnetall", type="b",lwd=3)
  abline(h=1.8,col=3)
  abline(h=1.4,col=3)
  abline(h=2.2,col=3)
  abline(h=2.1,col=4,lty=3)
}

#
#
#
levealder.plot <- function() {
  #X11() ; dev.set(4) ;
  dev.set(2) ; 
  plot(alle.leveser[,1],alle.leveser[,13],type="l",ylim=c(40,90),xlim=c(1870,2010),lwd=2,lty=1,col=1,
       xlab="Ar",ylab="Forventet levealder" )
  points(alle.leveser[,1],alle.leveser[,2],type="l",lwd=2,lty=2,col=2)
  points(alle.leveser[,1],alle.leveser[,23]+80,type="l")
  points(alle.leveser[,1],alle.leveser[,12]+80,type="l",lty=2,col=2)

  legend(1950,60,c("Kvinner","Menn","Kvinner over 80","Menn over 80"),
                lwd=c(2,2,1,1),lty=c(1,2,1,2),col=c(1,2,1,2))

}

friskframskr.plot <- function() {
  #X11() ; dev.set(4) ;
  dev.set(2) ; 
  plot(alle.leveser[1:28,1],alle.leveser[1:28,13],type="l",ylim=c(50,100),xlim=c(1940,2100),lwd=2,lty=1,col=1,
       xlab="Ar",ylab="Forventet levealder" )
  points(alle.leveser[1:28,1],alle.leveser[1:28,2],type="l",lwd=2,lty=2,col=1)
  #points(alle.leveser[,1],alle.leveser[,23]+80,type="l")
  #points(alle.leveser[,1],alle.leveser[,12]+80,type="l",lty=2,col=2)

   eSSB <- ssb.m02trend()
   points(eSSB[1,],eSSB[2,],type="l",lty=1,col=4,lwd=2)
   points(eSSB[1,],eSSB[3,],type="l",lty=2,col=4,lwd=2)
 

  legend(1950,60,c("Kvinner","Menn","Pensjonskomm, kvinner","Pensjonskomm, menn"),
                lwd=c(2,2,2,2),lty=c(1,2,1,2),col=c(1,1,4,4))

}


#
#
#
dodsr.plot <- function() {
  #X11() ; dev.set(3) ;
  dev.set(3) ;
# Menn 2004
  plot(alle.dodsr[,1],log(alle.dodsr[,11]/100000),type="l",xlim=c(0,100),ylim=c(-10,0),lwd=2,lty=1,col=4,
       xlab="Ar",ylab="log(Dode pr 100000/100000)" )
# Kvinner 2004
  points(alle.dodsr[,1],log(alle.dodsr[,21]/100000),type="l",lwd=2,lty=1,col=2)
# Menn 1971
  points(alle.dodsr[,1],log(alle.dodsr[,2]/100000),type="l",lty=2,col=4)
# Kvinner 1971

  points(alle.dodsr[,1],log(alle.dodsr[,12]/100000),type="l",lty=2,col=2)

  legend(30,15000,c("Kvinner 2004","Menn 2004","Kvinner 1971","Menn 1971"),
                lwd=c(2,2,1,1),lty=c(1,2,1,2),col=c(1,2,1,2))
}

#
#
#
dodelutv.plot <- function(tidsfr=c(2010,2030,2050,2070,2100)) {
  
  logklippserie <- function(row,kj=1) {
    if (kj==1) { st=2 ; endd=11 }
    else {  st=12 ; endd=21 }
  #  log((alle.dodsr[row,st:endd]-alle.dodsr[row,endd]/5)/100000) 
    log(alle.dodsr[row,st:endd]/100000) 
  }



  klograte <- function(aldgr=19) {

   logre <- log(rates.estim(tidsfr,kDt[aldgr],kD0[aldgr],kDL[aldgr],t0=1971))
   logre

  }

  mlograte <- function(aldgr=19) {

   logre <- log(rates.estim(tidsfr,mDt[aldgr],mD0[aldgr],mDL[aldgr],t0=1971))
   logre

  }

   mrates <- computerates(1973,2008,menn.dodsr[,1]/100000,menn.dodsr[,10]/100000,limfact=0.1)
   krates <- computerates(1973,2008,kvinn.dodsr[,1]/100000,kvinn.dodsr[,10]/100000,limfact=0.1)

   
   kDt <- krates[[1]] ;    kD0 <- krates[[2]] ;  kDL <- krates[[3]] ;
   mDt <- mrates[[1]] ;    mD0 <- mrates[[2]] ;  mDL <- mrates[[3]] ;
       
   maxald <- length(kDt)
 
   #td<-c(2003,2004,2005,2007,2010,2020,2030,2049)
   td=c(2002,2003,2004,2005,2006,2007,2008,2010,2015,2020,2025,2030,2040,2049)
   td2<-c(2009,2010,2011,2012,2013,2015,2020,2025,2030,2045,2059)

  #X11() ; #  dev.set(2) ; 
   X11(width=8,height=12) ;
  tidspktr <- c(1971,1981,1986,1991,1996,2001,2005,2006,2007,2008) 
  xmax <- max(tidspktr,tidsfr) ;
 #90+
  plot(tidspktr,logklippserie(19),type="l",ylim=c(-7,-1), xlim=c(1970,xmax),lwd=2,lty=2,col=1,
       xlab="Ar",ylab="log(dodelighet)" )
  #points(tidspktr,logklippserie(18),type="l",lwd=1,lty=2,col=1)
  points(tidsfr,mlograte(19),type="l",lwd=1,lty=2,col=3)
  points(td,f2002rates(ald=90,kj=1,td=td),type="l",lwd=1,lty=2,col=4)
  points(td2,f2009rates(ald=90,kj=1,td=td2),type="l",lwd=1,lty=2,col=2)
 #85-89
  #points(tidspktr,logklippserie(18),type="l",lwd=2,lty=2,col=1)
  #points(tidsfr,mlograte(18),type="l",lwd=1,lty=2,col=3)
  #points(td,f2002rates(ald=85,kj=1,td=td),type="l",lwd=1,lty=2,col=4)
  #points(td2,f2009rates(ald=85,kj=1,td=td2),type="l",lwd=1,lty=2,col=2)
 #80-84
  points(tidspktr,logklippserie(17),type="l",lwd=2,lty=2,col=1)
  points(tidsfr,mlograte(17),type="l",lwd=1,lty=2,col=3)
  points(td,f2002rates(ald=80,kj=1,td=td),type="l",lwd=1,lty=2,col=4)
  points(td2,f2009rates(ald=80,kj=1,td=td2),type="l",lwd=1,lty=2,col=2)
 #70-74
  #points(tidspktr,logklippserie(16),type="l",lwd=2,lty=2,col=1)
  points(tidspktr,logklippserie(15),type="l",lwd=2,lty=2,col=1)
  points(tidsfr,mlograte(15),type="l",lwd=1,lty=2,col=3)
  points(td,f2002rates(ald=70,kj=1,td=td),type="l",lwd=1,lty=2,col=4)
  points(td2,f2009rates(ald=70,kj=1,td=td2),type="l",lwd=1,lty=2,col=2)

  #points(tidspktr,logklippserie(14),type="l",lwd=1,lty=2,col=1)
  #points(tidspktr,logklippserie(13),type="l",lwd=1,lty=2,col=1)

  points(tidspktr,logklippserie(19,kj=2),type="l",lwd=2,lty=1,col=1)
  points(tidsfr,klograte(19),type="l",lwd=1,lty=1,col=3)
  points(td,f2002rates(ald=90,kj=2,td=td),type="l",lwd=1,lty=1,col=4)
  points(td2,f2009rates(ald=90,kj=2,td=td2),type="l",lwd=1,lty=1,col=2)

  #points(tidspktr,logklippserie(18,kj=2),type="l",lwd=2,lty=1,col=1)
  #points(tidsfr,klograte(18),type="l",lwd=1,lty=1,col=3)
  #points(td,f2002rates(ald=85,kj=2,td=td),type="l",lwd=1,lty=1,col=4)
  #points(td2,f2009rates(ald=85,kj=2,td=td2),type="l",lwd=1,lty=1,col=2)
 

  points(tidspktr,logklippserie(17,kj=2),type="l",lwd=2,lty=1,col=1)
  points(tidsfr,klograte(17),type="l",lwd=1,lty=1,col=3)
  points(td,f2002rates(ald=80,kj=2,td=td),type="l",lwd=1,lty=1,col=4)
  points(td2,f2009rates(ald=80,kj=2,td=td2),type="l",lwd=1,lty=1,col=2)

  #points(tidspktr,logklippserie(16,kj=2),type="l",lwd=2,lty=1,col=1)
  points(tidspktr,logklippserie(15,kj=2),type="l",lwd=2,lty=1,col=1)
  points(tidsfr,klograte(15),type="l",lwd=1,lty=1,col=3)
  points(td,f2002rates(ald=70,kj=2,td=td),type="l",lwd=1,lty=1,col=4)
  points(td2,f2009rates(ald=70,kj=2,td=td2),type="l",lwd=1,lty=1,col=2)

  #points(tidspktr,logklippserie(14,kj=2),type="l",lwd=1,lty=1,col=1)
  #points(tidspktr,logklippserie(13,kj=2),type="l",lwd=1,lty=1,col=1)


  #legend(1950,85,c("Kvinner","Menn","Kvinner over 80","Menn over 80"),
  #              lwd=c(2,2,1,1),lty=c(1,2,1,2),col=c(1,2,1,2))
  legend(1970,-5.3,c("Observert 1970-2008, kvinner","Observert 1970-2008, menn","Fra observert, kvinner","Fra observert, menn",
         "Pensjonskomm, kvinner","Pensjonskomm, menn","Framskr 2009,kvinner","Framskr 2009,menn" ),
         lwd=c(2,2,1,1,1,1),lty=c(1,2,1,2,1,2,1,2),col=c(1,1,3,3,4,4,2,2),cex=1)  

  text(c(2080,2080,2080),c(-1.2,-2.9,-4.4),c("90+","80-84","70-74"),cex=1.5)
  X11(width=4,height=8) ;
 
  par(mfrow=c(3,1))
 
   plot(td2,exp(f2009rates(ald=90,td=td2,kj=1)),ylim=c(0.1,0.35),xlim=c(1970,2100),lty=2,col=2,type="l", xlab="Ar",ylab="90+ dodelighet" )
   points(td,exp(f2002rates(ald=90,td=td,kj=1)),lty=2,col=4,type="l")
   points(tidsfr,exp(mlograte(19)),type="l",lwd=1,lty=2,col=3)
   points(tidspktr,exp(logklippserie(19)),type="l",lwd=2,lty=2,col=1)

   points(td2,exp(f2009rates(ald=90,td=td2,kj=2)),lty=1,col=2,type="l")
   points(td,exp(f2002rates(ald=90,td=td,kj=2)),lty=1,col=4,type="l")
   points(tidsfr,exp(klograte(19)),type="l",lwd=1,lty=1,col=3)
   points(tidspktr,exp(logklippserie(19,kj=2)),type="l",lwd=2,lty=1,col=1)
   text(2080,0.30,"90+",cex=2)

  #X11()
   plot(td2,exp(f2009rates(ald=80,td=td2,kj=1)),ylim=c(0.0,0.15),xlim=c(1970,2100),lty=2,col=2,type="l", xlab="Ar",ylab="80-84 dodelighet")
   points(td,exp(f2002rates(ald=80,td=td,kj=1)),lty=2,col=4,type="l")
   points(tidsfr,exp(mlograte(17)),type="l",lwd=1,lty=2,col=3)
   points(tidspktr,exp(logklippserie(17)),type="l",lwd=2,lty=2,col=1)

   points(td2,exp(f2009rates(ald=80,td=td2,kj=2)),lty=1,col=2,type="l")
   points(td,exp(f2002rates(ald=80,td=td,kj=2)),lty=1,col=4,type="l")
   points(tidsfr,exp(klograte(17)),type="l",lwd=1,lty=1,col=3)
   points(tidspktr,exp(logklippserie(17,kj=2)),type="l",lwd=2,lty=1,col=1)
   text(2080,0.12,"80-84",cex=2)


  #X11()
   plot(td2,exp(f2009rates(ald=70,td=td2,kj=1)),ylim=c(0.0,0.05),xlim=c(1970,2100),lty=2,col=2,type="l", xlab="Ar",ylab="70-74 dodelighet")
   points(td,exp(f2002rates(ald=70,td=td,kj=1)),lty=2,col=4,type="l")
   points(tidsfr,exp(mlograte(15)),type="l",lwd=1,lty=2,col=3)
   points(tidspktr,exp(logklippserie(15)),type="l",lwd=2,lty=2,col=1)

   points(td2,exp(f2009rates(ald=70,td=td2,kj=2)),lty=1,col=2,type="l")
   points(td,exp(f2002rates(ald=70,td=td,kj=2)),lty=1,col=4,type="l")
   points(tidsfr,exp(klograte(15)),type="l",lwd=1,lty=1,col=3)
   points(tidspktr,exp(logklippserie(15,kj=2)),type="l",lwd=2,lty=1,col=1)
   text(2080,0.04,"70-74",cex=2)



}

# dodelutv.framskr.plot - plots the logs of the observed and
# future extrapolated rates for selected age groups
#
#
#

dodelutv.framskr.plot <- function() {
  
  logklippserie <- function(row,kj=1) {
    if (kj==1) { st=2 ; endd=11 ; }
    else {  st=12 ; endd=21 ; }
    s1 <- (alle.dodsr[row,st:endd]/100000) 

  }

  #X11() ; #  dev.set(2) ; 
  X11(width=5,height=7) ;
  tidspktr <- c(1971,1981,1986,1991,1996,2001,2005,2010,2020,2030,2040,2050,2060) 

  plot(tidspktr,logklippserie(19),type="l",ylim=c(-5,-1),lwd=2,lty=1,col=4,
       xlab="Ar",ylab="log(dodelighet)/100000" )
  points(tidspktr,logklippserie(18),type="l",lwd=1,lty=1,col=4)
  points(tidspktr,logklippserie(17),type="l",lwd=1,lty=1,col=4)
  points(tidspktr,logklippserie(16),type="l",lwd=1,lty=1,col=4)
  points(tidspktr,logklippserie(15),type="l",lwd=1,lty=1,col=4)
  points(tidspktr,logklippserie(14),type="l",lwd=1,lty=1,col=4)
  points(tidspktr,logklippserie(13),type="l",lwd=1,lty=1,col=4)

  points(tidspktr,logklippserie(19,kj=2),type="l",lwd=2,lty=2,col=2)
  points(tidspktr,logklippserie(18,kj=2),type="l",lwd=1,lty=2,col=2)
  points(tidspktr,logklippserie(17,kj=2),type="l",lwd=1,lty=2,col=2)
  points(tidspktr,logklippserie(16,kj=2),type="l",lwd=1,lty=2,col=2)
  points(tidspktr,logklippserie(15,kj=2),type="l",lwd=1,lty=2,col=2)
  points(tidspktr,logklippserie(14,kj=2),type="l",lwd=1,lty=2,col=2)
  points(tidspktr,logklippserie(13,kj=2),type="l",lwd=1,lty=2,col=2)


  #legend(1950,85,c("Kvinner","Menn","Kvinner over 80","Menn over 80"),
  #              lwd=c(2,2,1,1),lty=c(1,2,1,2),col=c(1,2,1,2))

}


korttid.plot <- function(yr1=1973) {

   #dev.set(3) ; 
   X11(width=9,height=7) ;
   eK <-  makeexpect(startyr=2006,endyr=2060,yr1=yr1,yr2=2002,yrinterv=1,minald=1,col1=1,col2=6,limfact=0.1)  
   eK2 <-  makeexpect(startyr=2008,endyr=2060,yr1=yr1,yr2=2008,yrinterv=1,minald=1,col1=1,col2=10,limfact=0.1)  
   eK2a <-  makeexpect(startyr=2008,endyr=2060,yr1=yr1,yr2=2008,yrinterv=1,minald=1,col1=1,col2=10,limfact=0.1,k=-0.4)  
   eK2b <-  makeexpect(startyr=2008,endyr=2060,yr1=yr1,yr2=2008,yrinterv=1,minald=1,col1=1,col2=10,limfact=0.1,k=0.4)  
   eK2c <-  makeexpect(startyr=2008,endyr=2060,yr1=yr1,yr2=2008,yrinterv=1,minald=1,col1=1,col2=10,limfact=0.1,k=-0.8)  
   eK2d <-  makeexpect(startyr=2008,endyr=2060,yr1=yr1,yr2=2008,yrinterv=1,minald=1,col1=1,col2=10,limfact=0.1,k=0.8)  
 
   #eK80p <-  makeexpect(2009,2100,10,81)

   eSSB <- ssb.m02trend()
   eSSB2 <- ssb.m09trend()
   eSSB2a <- ssb.m09trend(alt=1) # Low
   eSSB2b <- ssb.m09trend(alt=3) # High
 
   e80pSSB <- ssb.m80ptrend()

   plot(eK2[1,],eK2[2,],ylim=c(75,95),xlim=c(2000,2060),type="l",lwd=2,xlab="Ar",ylab="Forventet levealder",col=3)
   points(eK2[1,],eK2[3,],type="l",lty=2,lwd=2,col=3)
   #points(eK[1,],eK[2,],type="l",lty=1,lwd=2,col=3)
   #points(eK[1,],eK[3,],type="l",lty=2,lwd=2,col=3)
   points(eK2a[1,],eK2a[2,],type="l",lty=1,lwd=1,col=3)
   points(eK2b[1,],eK2b[2,],type="l",lty=1,lwd=1,col=3)
   #points(eK2c[1,],eK2c[2,],type="l",lty=5,lwd=1,col=3)
   #points(eK2d[1,],eK2d[2,],type="l",lty=5,lwd=1,col=3)

   #points(eSSB[1,],eSSB[2,],type="l",lty=1,col=2,lwd=1)
   #points(eSSB[1,],eSSB[3,],type="l",lty=2,col=2,lwd=1)
   points(eSSB2[1,],eSSB2[2,],type="l",lty=1,col=2,lwd=2)
   points(eSSB2[1,],eSSB2[3,],type="l",lty=2,col=2,lwd=2)
   points(eK2a[1,],eK2a[3,],type="l",lty=2,lwd=1,col=3)
   points(eK2b[1,],eK2b[3,],type="l",lty=2,lwd=1,col=3)
   points(eSSB2a[1,],eSSB2a[2,],type="l",lty=1,col=2,lwd=1)
   points(eSSB2a[1,],eSSB2a[3,],type="l",lty=2,col=2,lwd=1)
   points(eSSB2b[1,],eSSB2b[2,],type="l",lty=1,col=2,lwd=1)
   points(eSSB2b[1,],eSSB2b[3,],type="l",lty=2,col=2,lwd=1)
   #points(eK2c[1,],eK2c[3,],type="l",lty=5,lwd=1,col=3)
   #points(eK2d[1,],eK2d[3,],type="l",lty=5,lwd=1,col=3)


   #points(seq(1996,2008),menn.leveald[29:41,2],type="l",lwd=1,lty=1,col=3)
   #points(seq(1996,2008),kvinn.leveald[29:41,2],type="l",lwd=1,lty=2,col=3)
   #points(seq(1996,2008),menn.leveald[29:41,2],type="l",lwd=1,lty=2,col=3)
   #points(seq(1996,2008),kvinn.leveald[29:41,2],type="l",lwd=1,lty=1,col=3)
   #points(alle.leveser[23:27,1],alle.leveser[23:27,2],type="l",lwd=1,lty=2,col=3)
   #points(alle.leveser[23:27,1],alle.leveser[23:27,13],type="l",lwd=1,lty=1,col=3)
   points(alle.leveser[,1],alle.leveser[,13],type="l",lwd=2,lty=1,col=1)
   points(alle.leveser[,1],alle.leveser[,2],type="l",lwd=2,lty=2,col=1)
   #points(alle.leveser[,1],alle.leveser[,23]+80,type="l",lty=1,col=1)
   #points(alle.leveser[,1],alle.leveser[,12]+80,type="l",lty=2,col=1)
   #points(e80pSSB[1,],e80pSSB[2,]+80,type="l",lty=1,col=2,lwd=1)
   #points(e80pSSB[1,],e80pSSB[3,]+80,type="l",lty=2,col=2,lwd=1)
   #points(eK80p[1,],eK80p[2,],type="l",lty=1,lwd=1,col=3)
   #points(eK80p[1,],eK80p[3,],type="l",lty=2,lwd=1,col=3)
 


   legend(2000,95,c("Framskrivning fra observerte dodsrater, kvinner","Framskrivning fra observerte dodsrater, menn",
         "Framskrivning 2009, kvinner","Framskrivning 2009, menn","Hoey- og lavalt. fra observerte dodsrater, kvinner",
         "Hoey- og lavalt. fra observerte dodsrater, menn",
         "Hoey- og lavalt. framskrivning 2009, kvinner","Hoey- og lavalt. framskrivning 2009, menn"),
         lwd=c(2,2,2,2,1,1,1,1),lty=c(1,2,1,2,1,2,1,2),col=c(3,3,2,2,3,3,2,2))  

}

# utgper.plot - plotter ulike framskrvninger sammen

utgper.plot <- function(yr1=1973,col1=1) {

   #dev.set(3) ; 
   X11(width=7,height=8) ;
    eK4 <-  makeexpect(startyr=1995,endyr=2060,yr1=yr1,yr2=1992,yrinterv=1,minald=1,col1=col1,col2=4,limfact=0.1,fdgM=0.7,fdgK=0.7)  
    eK3 <-  makeexpect(startyr=2000,endyr=2060,yr1=yr1,yr2=1997,yrinterv=1,minald=1,col1=col1,col2=5,limfact=0.1,fdgM=0.6,fdgK=0.6)  
    eK <-  makeexpect(startyr=2004,endyr=2060,yr1=yr1,yr2=2003,yrinterv=1,minald=1,col1=col1,col2=6,limfact=0.1,fdgM=0.5,fdgK=0.3)  

   eK2 <-  makeexpect(startyr=2008,endyr=2060,yr1=yr1,yr2=2008,yrinterv=1,minald=1,col1=col1,col2=10,limfact=0.1,fdgM=0.2,fdgK=0.1)  
   eK2a <-  makeexpect(startyr=2008,endyr=2060,yr1=yr1,yr2=2008,yrinterv=1,minald=1,col1=col1,col2=10,limfact=0.1,k=-0.4)  
   eK2b <-  makeexpect(startyr=2008,endyr=2060,yr1=yr1,yr2=2008,yrinterv=1,minald=1,col1=col1,col2=10,limfact=0.1,k=0.4)  
   eK2c <-  makeexpect(startyr=2008,endyr=2060,yr1=yr1,yr2=2008,yrinterv=1,minald=1,col1=col1,col2=10,limfact=0.1,k=-0.8)  
   eK2d <-  makeexpect(startyr=2008,endyr=2060,yr1=yr1,yr2=2008,yrinterv=1,minald=1,col1=col1,col2=10,limfact=0.1,k=0.8)  
 
   eK3a <-  makeexpect(startyr=2004,endyr=2060,yr1=yr1,yr2=2003,yrinterv=1,minald=1,col1=col1,col2=6,limfact=0.1,k=-0.4,fdgM=0.5,fdgK=0.4)  
   eK3b <-  makeexpect(startyr=2004,endyr=2060,yr1=yr1,yr2=2003,yrinterv=1,minald=1,col1=col1,col2=6,limfact=0.1,k=0.4,fdgM=0.5,fdgK=0.4)  
 

   #eK80p <-  makeexpect(2009,2100,10,81)

   eSSB <- ssb.m02trend()
   eSSB2 <- ssb.m09trend()
   eSSB2a <- ssb.m09trend(alt=1) # Low
   eSSB2b <- ssb.m09trend(alt=3) # High
 
   e80pSSB <- ssb.m80ptrend()

   plot(eK2[1,],eK2[2,],ylim=c(70,90),xlim=c(1950,2030),type="l",lwd=2,xlab="Ar",ylab="Forventet levealder",col=3)
   points(eK2[1,],eK2[3,],type="l",lty=2,lwd=2,col=3)
   points(eK[1,],eK[2,],type="l",lty=1,lwd=1,col=3)
   points(eK[1,],eK[3,],type="l",lty=2,lwd=1,col=3)
   points(eK3[1,],eK3[2,],type="l",lty=1,lwd=1,col=3)
   points(eK4[1,],eK4[2,],type="l",lty=1,lwd=1,col=3)
   points(eK3[1,],eK3[3,],type="l",lty=2,lwd=1,col=3)
   points(eK4[1,],eK4[3,],type="l",lty=2,lwd=1,col=3)
   #points(eK3a[1,],eK3a[2,],type="l",lty=1,lwd=1,col=3)
   #points(eK3b[1,],eK3b[2,],type="l",lty=1,lwd=1,col=3)
   #points(eK3a[1,],eK3a[3,],type="l",lty=2,lwd=1,col=3)
   #points(eK3b[1,],eK3b[3,],type="l",lty=2,lwd=1,col=3)
 
 
   #points(eK2c[1,],eK2c[2,],type="l",lty=5,lwd=1,col=3)
   #points(eK2d[1,],eK2d[2,],type="l",lty=5,lwd=1,col=3)

   #points(eK2a[1,],eK2a[2,],type="l",lty=1,lwd=1,col=3)
   #points(eK2b[1,],eK2b[2,],type="l",lty=1,lwd=1,col=3)
   #points(eK2c[1,],eK2c[2,],type="l",lty=5,lwd=1,col=3)
   #points(eK2d[1,],eK2d[2,],type="l",lty=5,lwd=1,col=3)

   points(eSSB[1,],eSSB[2,],type="l",lty=1,col=2,lwd=1)
   points(eSSB[1,],eSSB[3,],type="l",lty=2,col=2,lwd=1)
   points(eSSB2[1,],eSSB2[2,],type="l",lty=1,col=2,lwd=2)
   points(eSSB2[1,],eSSB2[3,],type="l",lty=2,col=2,lwd=2)
   #points(eK2a[1,],eK2a[3,],type="l",lty=2,lwd=1,col=3)
   #points(eK2b[1,],eK2b[3,],type="l",lty=2,lwd=1,col=3)
   #points(eSSB2a[1,],eSSB2a[2,],type="l",lty=1,col=2,lwd=1)
   #points(eSSB2a[1,],eSSB2a[3,],type="l",lty=2,col=2,lwd=1)
   #points(eSSB2b[1,],eSSB2b[2,],type="l",lty=1,col=2,lwd=1)
   #points(eSSB2b[1,],eSSB2b[3,],type="l",lty=2,col=2,lwd=1)
   #points(eK2c[1,],eK2c[3,],type="l",lty=5,lwd=1,col=3)
   #points(eK2d[1,],eK2d[3,],type="l",lty=5,lwd=1,col=3)


   #points(seq(1996,2008),menn.leveald[29:41,2],type="l",lwd=1,lty=1,col=3)
   #points(seq(1996,2008),kvinn.leveald[29:41,2],type="l",lwd=1,lty=2,col=3)
   #points(seq(1996,2008),menn.leveald[29:41,2],type="l",lwd=1,lty=2,col=3)
   #points(seq(1996,2008),kvinn.leveald[29:41,2],type="l",lwd=1,lty=1,col=3)
   #points(alle.leveser[23:27,1],alle.leveser[23:27,2],type="l",lwd=1,lty=2,col=3)
   #points(alle.leveser[23:27,1],alle.leveser[23:27,13],type="l",lwd=1,lty=1,col=3)
   points(alle.leveser[,1],alle.leveser[,13],type="l",lwd=2,lty=1,col=1)
   points(alle.leveser[,1],alle.leveser[,2],type="l",lwd=2,lty=2,col=1)
   #points(alle.leveser[,1],alle.leveser[,23]+80,type="l",lty=1,col=1)
   #points(alle.leveser[,1],alle.leveser[,12]+80,type="l",lty=2,col=1)
   #points(e80pSSB[1,],e80pSSB[2,]+80,type="l",lty=1,col=2,lwd=1)
   #points(e80pSSB[1,],e80pSSB[3,]+80,type="l",lty=2,col=2,lwd=1)
   #points(eK80p[1,],eK80p[2,],type="l",lty=1,lwd=1,col=3)
   #points(eK80p[1,],eK80p[3,],type="l",lty=2,lwd=1,col=3)
 


   #legend(1925,55,c("Framskrivning fra observerte dodsrater, kvinner","Framskrivning fra observerte dodsrater, menn",
   #      "Pensjonskommisjonens framskriving, kvinner","Pensjonskommisjonens framskriving, menn"),
   #      lwd=c(3,3,2,2),lty=c(1,2,1,2),col=c(3,3,2,2))  

}

Aukrustrekr.plot <- function() {

  yrseq1 <- seq(1875,1980,by=5) ; 
  htseq1 <- c(169.2,168.9,170,168,168.6,170.4,170,171.1,171.15,171.6,171.9,172.8,173.4,174.1,174.9,175.8,176.8,177.2,178,178.7,179.1,179.4) ;

  rfm <- lm(htseq1[11:20] ~yrseq1[11:20]) ;  

  X11(width=9,height=5)
  plot(yrseq1,htseq1,type="l",ylim=c(0,185),xlim=c(1000,2000),lwd=3,lty=1,col=1,
       xlab="Ar",ylab="Gjennomsnittl hoeyde" )
  #points(yrseq1,obs.f2009_0[5:length(obs.f2009_0)],type="l",lwd=3,lty=1,col=2)
  #text(c(2008,2040),c(61500,57000),c("Faktisk","Framskrevet"))
  abline(rfm,col=2) 
}




fodseljfr.plot <- function() {

  yrseq1 <- c(2002:2009) ; yrseq2 <- c(2002:2050) ;
  f2002_0 <-  kvinn.f2002[1,] +  menn.f2002[1,]
  obs.f2009_0 <-  obs.kvinn.f2009[1,] +  obs.menn.f2009[1,]

  X11(width=8,height=6)
  plot(yrseq2,f2002_0,type="l",ylim=c(45000,65000),xlim=c(2002,2050),lwd=2,lty=2,col=1,
       xlab="Ar",ylab="Antall foedte" )
  points(yrseq1,obs.f2009_0[5:length(obs.f2009_0)],type="l",lwd=3,lty=1,col=2)
  text(c(2008,2040),c(61500,57000),c("Faktisk","Framskrevet"))

}

gamlejfr.plot <- function() {

  yrseq1 <- c(2002:2009) ; yrseq2 <- c(2002:2050) ; yrseq3 <- c(70:100) ;  yrseq4 <- c(70:98) ; 
  f2002_80k <-  kvinn.f2002[71:99,8]
  f2002_80m <-  menn.f2002[71:99,8]
  #obs.f2009_0 <-  obs.kvinn.f2009[1,] +  obs.menn.f2009[1,]

  X11(width=9,height=7)

  plot(yrseq3,obs.kvinn.f2009[71:101,12] ,type="l",ylim=c(0,35000),xlim=c(70,100),lwd=2,lty=1,col=1, xlab="Alder",ylab="" )
  points(yrseq3,obs.menn.f2009[71:101,12],type="l",lwd=2,lty=2,col=1)
  points(yrseq4,f2002_80k,type="l",lwd=2,lty=1,col=4)
  points(yrseq4,f2002_80m,type="l",lwd=2,lty=2,col=4)
  points(yrseq3,obs.kvinn.f2009[71:101,5],type="l",lwd=1,lty=1,col=1)
  points(yrseq3,obs.menn.f2009[71:101,5],type="l",lwd=1,lty=2,col=1)
  points(yrseq4,kvinn.f2002[71:99,49],type="l",lwd=1,lty=1,col=4)
  points(yrseq4,menn.f2002[71:99,49],type="l",lwd=1,lty=2,col=4)

  points(yrseq4,kvinn.f2009[71:99,42],type="l",lwd=1,lty=1,col=2)
  points(yrseq4,menn.f2009[71:99,42],type="l",lwd=1,lty=2,col=2)
 
  legend(88.5,35000,c("Kvinner 2009","Menn 2009","Kvinner 2002","Menn 2002","Kvinner 2009, framskr 2003",
                    "Menn 2009, framskr 2003","Kvinner 2050, framskr 2003","Menn 2050, framskr 2003",
                    "Kvinner 2050, framskr 2009","Menn 2050, framskr 2009"),
                lwd=c(2,2,1,1,2,2,1,1,1,1),lty=c(1,2,1,2,1,2,1,2,1,2),col=c(1,1,1,1,4,4,4,4,2,2))



}


fbyrde.plot <- function(  ) {


   X11(width=8, height=7) 
   plot(seq(1970,2060,by=1),framskr.fbyrde[,1],ylim=c(0,90),lty=1,lwd=2,
        type="l",xlab="Ar",ylab="Forsoergelsesbyrde")
   points(seq(1970,2060,by=1),framskr.fbyrde[,2],lty=2,type="l",lwd=2,col=2)

   legend(2015,15,c("Pensjonister","Pensjonister+barn/ungdom"),lwd=c(2,2),lty=c(1,1),col=c(1,2))


}
