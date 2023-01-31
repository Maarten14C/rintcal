## ---- echo=FALSE--------------------------------------------------------------
require(rintcal)

## ---- fig.width=4, fig.asp=.8-------------------------------------------------
draw.ccurve()

## ---- fig.width=4, fig.asp=.8-------------------------------------------------
draw.ccurve(1600, 2020, BCAD=TRUE, cc2='nh1', add.yaxis=TRUE)

## ---- fig.width=4, fig.asp=.8-------------------------------------------------
draw.ccurve(1600, 1950, BCAD=TRUE)

## ---- fig.width=4, fig.asp=.8-------------------------------------------------
draw.ccurve(1600, 2020, BCAD=TRUE, cc2='nh1')

## ---- fig.width=4, fig.asp=.8-------------------------------------------------
draw.ccurve(1600, 2020, BCAD=TRUE, cc2='nh1', add.yaxis=TRUE)

## ---- fig.width=5, fig.asp=1--------------------------------------------------
intcal.data(0, 500)

## ---- fig.width=5, fig.asp=1--------------------------------------------------
libby <- read.table(file.path(system.file(package = 'rintcal'), "extdata/Arnold_Libby_1951.txt"), header=T, sep=",")
plot(libby[,2], libby[,4], xlab="'cal' BP", ylab="C14 BP") # plot the radiocarbon dates and their known calendar ages
segments(libby[,2]-libby[,3], libby[,4], libby[,2]+libby[,3], libby[,4]) # calendar error bars (not all are quantified)
segments(libby[,2], libby[,4]-libby[,5], libby[,2], libby[,4]+libby[,5]) # radiocarbon error bars
abline(0, 1, lty=2)
draw.ccurve(0, 10e3, add=TRUE) # add the most recent calibration curve

