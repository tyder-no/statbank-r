#
#  source("nlpPipe_1.R")
#
#
#
#
#


library(magick)
library(tesseract)
library(udpipe)



rep1 <- image_read('936742475_2017.tif')
image_write(rep1, path = "rep1.png", format = "png")


xpng <- image_convert(image_read('936742475_2017a.tif'),format='png')

#eng <- tesseract("eng")
nor <- tesseract("nor")
xtext <- tesseract::ocr(xpng, engine = nor)

#udmodel <- udpipe_download_model(language = "dutch")
#udmodel <- udpipe_download_model(language = "norwegian-bokmaal")
#udmodel <- udpipe_load_model(file = udmodel$file_model)
#
#
#
#
udmodel <- udpipe_load_model(file ='norwegian-bokmaal-ud-2.4-190531.udpipe')

xrapp <- udpipe_annotate(udmodel, x = xtext)
xrappdf <- as.data.frame(xrapp, detailed = TRUE)






