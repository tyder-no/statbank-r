#
#
# source("gompertz_fitting_1.R")
#
#library(ggplot)
library(dplyr)
library(ggplot2)

source("ssb-json-functions.R")

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

sT <- getSurvivalTable()
