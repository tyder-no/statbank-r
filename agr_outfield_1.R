#
# source("agr_outfield_1.R")
#
#
source("ssb-json-tests-1.R")

#06462 	Jordbruksareal, etter bruken (dekar) (K) 	1969 - 2018
#12660 	Husdyr på utmarksbeite (K) 	1995 - 2018
#07366 	Produktivt skogareal (dekar) (K) 	2008 - 2017
 
#  load(file=paste('Rd_meta/sdf_','06462','.Rdata',sep='')) # get mVL



mkdsARes <- function() {
    load(file=paste('Rd_data/jd_','06462','.Rdata',sep=''))
    load(file=paste('Rd_data/jd_','07366','.Rdata',sep=''))
    load(file=paste('Rd_data/jd_','12660','.Rdata',sep=''))

    ds3 <- dsJoinRegionTid(castOnContentsCode(jd07366),castOnContentsCode(jd12660))
    dsARes <- dsJoinRegionTid(ds3,castOnContentsCode(massage06462()))

    aArNm <- c('jbrar','fdyrk','aakhage','kornolj','hvete','bygg','havre','potet','grfor','grsak','engslbe','fdeng','beite')
    colnames(dsARes) <- c(colnames(dsARes[1:7]),aArNm)
    save(dsARes,file=paste('Rd_data/','dsARes','.Rdata',sep=''))
}

#load(file=paste('Rd_data/','dsARes','.Rdata',sep=''))
dsARes17 <- dsARes[dsARes$Tid==2017,]

# Forest : 15-20 da/sheep, 5-7 FEm/daa
# Grass: 400 FEM/daa
# alpha=80% area for grass


alpha <- 0.8 ; FEmForest <- 6 ; FEmGrass <- 400 ; seasonL <- 140 ;

potential <- ifelse(dsARes17$Prodskog/dsARes17$jbrar < FEmGrass/FEmForest*alpha, dsARes17$Prodskog*FEmForest/seasonL, dsARes17$jbrar*FEmGrass/seasonL)







