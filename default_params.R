# DEFAULT OPTIONS AND FACILITATIONS:


options(max.print = 50)

par(
  pch = 20
  ,lwd = 2
  ,cex.axis = 1
  ,cex =  1
  ,cex.main = 1
  ,cex.lab = 1
  ,cex.sub = 1
  )

#### functions ####


`%nin%` <- Negate(`%in%`)

# dev.off()



##### IMAGES SAVING PARAMS

# jpeg
jpeg("PATH/FILENAME.jpg"
     ,height = 5.83
     ,width = 5.83
     ,quality = 80
     ,units = "in"
     ,res = 150)

# pdf
pdf("PATH/FILENAME.pdf"
    ,height = 5.83
    ,width = 8.27)