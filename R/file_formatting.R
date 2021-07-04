
# IntCal 98
### The files were made by reading the files of the calibration curves as distributed in http://intcal.org/curves:
# cc.terr <- read.table( "~/Downloads/intcal98.14c", header=FALSE, skip=11)
# cc.terr[,1] = 1950-cc.terr[,1] # since the calendar ages are in BC/AD!
# cc.marine <- read.table( "~/Downloads/marine98.14c", header=FALSE, skip=11)
# cc.marine[,1] = 1950-cc.marine[,1] # since the calendar ages are in BC/AD!

### ... and were then written in the desired format (increasing cal BP ages, 3 columns, tab separator):
# write.table( cc.terr[nrow(cc.terr):1,c(1,4,5)], file="3Col_intcal98.14C", row.names=FALSE, col.names=FALSE)
# write.table( cc.marine[nrow(cc.marine):1,c(1,4,5)], file="3Col_marine98.14C", row.names=FALSE, col.names=FALSE)


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


# IntCal20
### The files were made by reading the files of the calibration curves as distributed in http://intcal.org/curves:
#cc.terr <- read.csv( "intcal20.14c", header=FALSE, skip=11, sep=",")
#cc.marine <- read.csv( "marine20.14c", header=FALSE, skip=11)
#cc.south <- read.csv( "shcal20.14c", header=FALSE, skip=11, sep=",")

### ... and were then written in the desired format (increasing cal BP ages, 3 columns, tab separator):
#write.table( cc.terr[nrow(cc.terr):1,1:3], file="3Col_intcal20.14C", row.names=FALSE, col.names=FALSE)
#write.table( cc.marine[nrow(cc.marine):1,1:3], file="3Col_marine20.14C", row.names=FALSE, col.names=FALSE)
#write.table( cc.south[nrow(cc.south):1,1:3], file="3Col_shcal20.14C", row.names=FALSE, col.names=FALSE)


# postbomb curves:

# NH1, NH2, NH3, SH1-2 and SH3 postbomb curves from Hua et al. were copied from the clam and rbacon packages

# kure <- read.table("inst/extdata/Kure.14c", skip=11, header=FALSE)
# as.c <- array(pMC.age(kure[,2], kure[,3], ratio=1), dim=c(nrow(kure),2))
# kure <- cbind(kure[,1], as.c)
# write.table(kure, "inst/extdata/Kure.14C", row.names=FALSE, col.names=FALSE, sep="\t")

# levin <- read.table("inst/extdata/Levin-Kromer.14c", skip=11, header=FALSE)
# as.c <- array(pMC.age(levin[,2], levin[,3], ratio=1), dim=c(nrow(levin),2))
# levin <- cbind(levin[,1], as.c)
# write.table(levin, "inst/extdata/LevinKromer.14C", row.names=FALSE, col.names=FALSE, sep="\t")

# santos <- read.table("inst/extdata/Santos.14c", skip=11, header=FALSE)
# as.c <- array(pMC.age(santos[,2], santos[,3], ratio=1), dim=c(nrow(santos),2))
# santos <- cbind(santos[,1], as.c)
# write.table(santos, "inst/extdata/Santos.14C", row.names=FALSE, col.names=FALSE, sep="\t")
