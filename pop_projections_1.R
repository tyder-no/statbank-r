#
# source("pop_projections_1.R")
#
#
#
#
#
#
# source("ssb-json-tests-1.R")




#f96 <- castOnContentsCodeKjonn(massage01302()) 
#f99 <- castOnContentsCodeKjonn(massage01313()) 
#b2019 <- castOnContentsCodeKjonn(massage07459b())


#f96[100:150,c(1:2,17:19)]
#f96[100:150,c(1:2,20:23)]
#f96[100:150,c(1:2,24:48)]
#f96[100:150,c(1:2,49:70)]
#f96[100:150,c(1:2,71:83)]
#f96[100:150,c(1:2,84:93)]
#f96[100:150,c(1:2,94:103)]


ageGroup11 <- function(ds,prefix='grp') {

    stpts <- c(4,5,10,17,20,24,49,71,84,94)  
    endpts <- c(4,9,16,19,23,48,70,83,93,103)  
    agenms <-c('0','1_5','6_12','13_15','16_19','20_44','45_66','67_79','80_89','90p','Tot')
    colnms <-paste(prefix,'_',agenms,sep='')
    grpds <- ds[,c(1:3)] ; cn0 <-colnames(grpds) ; colnms <- c(cn0,colnms) ; 
    for (i in (1:length(stpts))) {         print(endpts[i])
        if (endpts[i]>stpts[i])         newCol <- rowSums(ds[,stpts[i]:endpts[i]])
        else newCol <- ds[,stpts[i]]
        
        grpds <- cbind(grpds,newCol) 
    }
    newCol <- rowSums(grpds[,4:(3+length(endpts))])
     grpds <- cbind(grpds,newCol)
                                        #assign(paste('f96_','90p',sep=''),rowSums(f96[,94:103]))
    colnames(grpds) <- colnms 
    grpds
}

genderAdd <- function(ds,genderCol=3) {

  mDs <- ds[ds[genderCol]==1,4:14] ;
  fDs <- ds[ds[genderCol]==2,4:14] ;
  rDs <- mDs + fDs ;
  cbind(ds[ds[genderCol]==1,1:2],rDs)

}

#intervs <- c(4,c(5:9),10:16,17:19,20:23,24:48,49:70,71:83,84:93,94:103)

mkfg96 <- function() {
    f96 <- castOnContentsCodeKjonn(massage01302())
    gds <- ageGroup11(f96,prefix='f96')
    ads <- genderAdd(gds)
    ads

}


mkfg99 <- function() {
    f99 <- castOnContentsCodeKjonn(massage01313())
    gds <- ageGroup11(f99,prefix='f99')
    ads <- genderAdd(gds)
    ads

}


mkfg02 <- function() {
    f02 <- castOnContentsCodeKjonn(massage03375())
    gds <- ageGroup11(f02,prefix='f02')
    ads <- genderAdd(gds)
    ads

}

mkfg09 <- function() {
    f09 <- castOnContentsCodeKjonn(massage07268())
    gds <- ageGroup11(f09,prefix='f09')
    ads <- genderAdd(gds)
    ads

}



mkbg19 <- function() {
    b19 <- castOnContentsCodeKjonn(massage07459b())
    gds <- ageGroup11(b19,prefix='b19')
    ads <- genderAdd(gds)
    ads

}


#fg96 <mkfg96()
#fg99 <- mkfg99()
#fg02 <- mkfg02()
#fg09 <- mkfg09()



#bg19 <- mkbg19()


#bg19$Region[bg19$Region=='0'] <- '0A'
#fg96$Region[fg96$Region=='0'] <- '0A'
#fg02$Region[fg02$Region=='0'] <- '0A'
#fg09$Region[fg09$Region=='0'] <- '0A'



# bg19fg96fg02 <- dsJoinRegionTid(bg19fg96,fg02)
# bg19fg96fg02fg09 <- dsJoinRegionTid(bg19fg96fg02,fg09)

# fg96bg19 <- dsJoinRegionTid(fg96,bg19)
# bg19fg96 <- dsJoinRegionTid(bg19,fg96)
# bf221 <- bg19fg96[bg19fg96$Region=='0221',]
#bf0 <- bg19fg96[bg19fg96$Region=='0',]

#bf0221 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='0221',]

#plot(bf0221$Tid,bf0221$b19_Tot,col=3,ylim=c(11000,17000))
#points(bf0221$Tid,bf0221$f96_Tot,col=2)

#X11()
#plot(bf0221$Tid,bf0221$b19_90p,col=3,ylim=c(0,150))
#points(bf0221$Tid,bf0221$f96_90p,col=2)

#X11()
#plot(bf0221$Tid,bf0221$b19_6_12,col=3,ylim=c(0,1500))
#points(bf0221$Tid,bf0221$f96_6_12,col=2)

#bf00 <- bg19fg96[bg19fg96$Region=='0A',]
#bf01 <- bg19fg96[bg19fg96$Region=='01',]
#bf02 <- bg19fg96[bg19fg96$Region=='02',]

#bf03 <- bg19fg96[bg19fg96$Region=='03',]

#bf000 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='0A',]
#bf001 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='01',]
#bf002 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='02',]

#bf003 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='03',]
#bf014 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='14',]
#bf015 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='15',]
#bf016 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='16',]

#bf017 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='17',]
#bf018 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='18',]

#bf019 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='19',]
#bf020 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='20',]





#bfpanel(bf0221,mainTit='Aurskog-Høland, alle',plotFnm='fpanel_ah.png')

#bfpanel(bf000,ylim1=c(4000000,5500000),ylim2=c(200000,600000),ylim3=c(40000,70000),ylim4=c(5000,45000))
#bfpanel(bf002,ylim1=c(400000,650000),ylim2=c(20000,60000),ylim3=c(4000,7000),ylim4=c(500,4500))
#bfpanel(bf001,ylim1=c(200000,300000),ylim2=c(8000,26000),ylim3=c(1500,3500),ylim4=c(500,2500))
#bfpanel(bf003,ylim1=c(400000,700000),ylim2=c(20000,60000),ylim3=c(5000,11000),ylim4=c(1000,6000),mainTit='Oslo, personer',plotFnm='fpanel_oslo.png')
#bfpanel(bf014,ylim1=c(100000,120000),ylim2=c(7000,12000),ylim3=c(1000,1600),ylim4=c(500,1500))
#bfpanel(bf018,ylim1=c(200000,260000),ylim2=c(17000,25000),ylim3=c(1800,3600),ylim4=c(500,2500))
#
# bfpanel(bf003,ylim1=c(400000,700000),ylim2=c(20000,60000),ylim3=c(5000,11000),ylim4=c(1000,6000),mainTit='Oslo, alle',plotFnm='fpanel_oslo.png')
#bfpanel(bf014,ylim1=c(100000,120000),ylim2=c(7000,12000),ylim3=c(1000,1600),ylim4=c(500,1500),mainTit='Sogn og fjordane, alle',plotFnm='fpanel_sof.png')


bfpanel <- function(dfc,ylim1=c(11000,17000),ylim2=c(800,1600),ylim3=c(100,200),ylim4=c(0,150),mainTit='Hele landet, alle',plotFnm='fpanel_no.png') { 

    
    pkernel <- function() {
        par(mfrow=c(2,2))
        plot(dfc$Tid,dfc$b19_Tot,col=3,ylim=ylim1,type='l',lwd=3,xlab='Tid',ylab='Personer',main=mainTit)
        points(dfc$Tid,dfc$f96_Tot,col=2,type='l',lwd=2)
        points(dfc$Tid,dfc$f02_Tot,col=2,type='l',lty=2,lwd=2)
        points(dfc$Tid,dfc$f09_Tot,col=2,type='l',lty=3,lwd=2)
        legend(1986,ylim1[2],0.72,legend=c('Faktisk','F1996','F2002','F2009'),col=c(3,2,2,2),lwd=c(3,2,2,2),lty=c(1,1,2,3))


        plot(dfc$Tid,dfc$b19_6_12,col=3,ylim=ylim2,type='l',lwd=3,xlab='Tid',ylab='Personer',main='Barneskolealder 6-12 år')
        points(dfc$Tid,dfc$f96_6_12,col=2,type='l',lwd=2)
        points(dfc$Tid,dfc$f02_6_12,col=2,type='l',lty=2,lwd=2)
        points(dfc$Tid,dfc$f09_6_12,col=2,type='l',lty=3,lwd=2)

        plot(dfc$Tid,dfc$b19_0,col=3,ylim=ylim3,type='l',lwd=3,xlab='Tid',ylab='Personer',main='Fødte')
        points(dfc$Tid,dfc$f96_0,col=2,type='l',lwd=2)
        points(dfc$Tid,dfc$f02_0,col=2,type='l',lty=2,lwd=2)
        points(dfc$Tid,dfc$f09_0,col=2,type='l',lty=3,lwd=2)

        plot(dfc$Tid,dfc$b19_90p,col=3,ylim=ylim4,type='l',lwd=3,xlab='Tid',ylab='Personer',main='90 år og over')
        points(dfc$Tid,dfc$f96_90p,col=2,type='l',lwd=2)
        points(dfc$Tid,dfc$f02_90p,col=2,type='l',lty=2,lwd=2)
        points(dfc$Tid,dfc$f09_90p,col=2,type='l',lty=3,lwd=2)
    }
    graphics.off() 
    X11(width=12,height=10)
    pkernel()

    png(plotFnm,width = 1400, height = 1000)
    par(mfrow=c(2,2))
    pkernel()
    dev.off()
    
}


load(file='Rd_data/bg19fg96fg02fg09.Rdata')
attach(bg19fg96fg02fg09)

bbrd19 <- (b19_0+b19_1_5+b19_6_12+b19_13_15+b19_16_19+b19_67_79+b19_80_89+b19_90p)/(b19_20_44+b19_45_66)
fbrd96 <- (f96_0+f96_1_5+f96_6_12+f96_13_15+f96_16_19+f96_67_79+f96_80_89+f96_90p)/(f96_20_44+f96_45_66)
fbrd02 <- (f02_0+f02_1_5+f02_6_12+f02_13_15+f02_16_19+f02_67_79+f02_80_89+f02_90p)/(f02_20_44+f02_45_66)
fbrd09 <- (f09_0+f09_1_5+f09_6_12+f09_13_15+f09_16_19+f09_67_79+f09_80_89+f09_90p)/(f09_20_44+f09_45_66)


pltbrdn <- function() {
  
    plot(Tid[Region=='0A'],bbrd19[Region=='0A'],ylim=c(0.6,0.72),col=3,type='l',lwd=4,xlab='År',ylab='Byrde',main='Forsørgelsesbyrde (= Utenfor/Inne i arbeidsstyrken)')
    points(Tid[Region=='0A'],fbrd96[Region=='0A'],col=2,type='l',lwd=2)
    points(Tid[Region=='0A'],fbrd02[Region=='0A'],col=2,type='l',lwd=2,lty=2)
    points(Tid[Region=='0A'],fbrd09[Region=='0A'],col=2,type='l',lwd=2,lty=3)
    legend(2005,0.72,legend=c('Faktisk','F1996','F2002','F2009'),col=c(3,2,2,2),lwd=c(4,2,2,2),lty=c(1,1,2,3))
}

pltbrdndeviation <- function() {
    devIndex <- function(fb) { (fb/bb19 -1)*100 }
    
    
    Td <- Tid[Region=='0A'] ; bb19 <- bbrd19[Region=='0A'] ;
    fb96 <- fbrd96[Region=='0A'] ; fb02 <- fbrd02[Region=='0A'] ; fb09 <- fbrd09[Region=='0A'] ;
    plot(Td,devIndex(fb96),ylim=c(-2,5),xlim=c(1996,2019),col=2,type='l',lwd=3,xlab='År',ylab='Avviksindeks',main='Indeks: Prosentvis feil forsørgelsesbyrde')
    points(Td,devIndex(fb02),col=2,type='l',lwd=3,lty=2)
    points(Td,devIndex(fb09),col=2,type='l',lwd=3,lty=3)
    abline(a=0,b=0)
    legend(1996,5,legend=c('F1996','F2002','F2009'),col=c(2,2,2),lwd=c(3,3,3),lty=c(1,2,3))
}

pltboth <- function() {

    X11(width=14,height=10)
    par(mfrow=c(1,2))
    pltbrdn()
    pltbrdndeviation()
    
    png('fbyrde.png',width = 1400, height = 1000)
    par(mfrow=c(1,2))
    pltbrdn()
    pltbrdndeviation()
    dev.off()
    
}
