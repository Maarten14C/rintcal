
# IntCal 98
### The files were made by reading the files of the calibration curves as distributed in http://intcal.org/curves:
# cc.terr <- read.table( "intcal98.14c", header=FALSE, skip=11)
# cc.terr[,1] = 1950-cc.terr[,1] # since the calendar ages are in BC/AD!
# cc.marine <- read.table( "marine98.14c", header=FALSE, skip=11)
# cc.marine[,1] = 1950-cc.marine[,1] # since the calendar ages are in BC/AD!

### ... and were then written in the desired format (increasing cal BP ages, 3 columns, tab separator):
# write.table( cc.terr[nrow(cc.terr):1,1:3], file="3Col_intcal98.14C", row.names=FALSE, col.names=FALSE)
# write.table( cc.marine[nrow(cc.marine):1,1:3], file="3Col_marine98.14C", row.names=FALSE, col.names=FALSE)


# IntCal04
### The files were made by reading the files of the calibration curves as distributed in http://intcal.org/curves:
# cc.terr <- read.csv( "intcal04.14c", header=FALSE, skip=11, sep=",")
# cc.marine <- read.csv( "marine04.14c", header=FALSE, skip=11)

### ... and were then written in the desired format (increasing cal BP ages, 3 columns, tab separator):
# write.table( cc.terr[nrow(cc.terr):1,1:3], file="3Col_intcal04.14C", row.names=FALSE, col.names=FALSE)
# write.table( cc.marine[nrow(cc.marine):1,1:3], file="3Col_marine04.14C", row.names=FALSE, col.names=FALSE)


# IntCal09
### The files were made by reading the files of the calibration curves as distributed in http://intcal.org/curves:
# cc.terr <- read.csv( "intcal09.14c", header=FALSE, skip=11, sep=",")
# cc.marine <- read.csv( "marine09.14c", header=FALSE, skip=11)

### ... and were then written in the desired format (increasing cal BP ages, 3 columns, tab separator):
# write.table( cc.terr[nrow(cc.terr):1,1:3], file="3Col_intcal09.14C", row.names=FALSE, col.names=FALSE)
# write.table( cc.marine[nrow(cc.marine):1,1:3], file="3Col_marine09.14C", row.names=FALSE, col.names=FALSE)


# IntCal13
### The files were made by reading the files of the calibration curves as distributed in http://intcal.org/curves:
# cc.terr <- read.csv( "intcal13.14c", header=FALSE, skip=11, sep=",")
# cc.marine <- read.csv( "marine13.14c", header=FALSE, skip=11)
# cc.south <- read.csv( "shcal13.14c", header=FALSE, skip=11, sep=",")

### ... and were then written in the desired format (increasing cal BP ages, 3 columns, tab separator):
# write.table( cc.terr[nrow(cc.terr):1,1:3], file="3Col_intcal13.14C", row.names=FALSE, col.names=FALSE)
# write.table( cc.marine[nrow(cc.marine):1,1:3], file="3Col_marine13.14C", row.names=FALSE, col.names=FALSE)
# write.table( cc.south[nrow(cc.south):1,1:3], file="3Col_shcal13.14C", row.names=FALSE, col.names=FALSE)

# the above was also done for IntCal20
