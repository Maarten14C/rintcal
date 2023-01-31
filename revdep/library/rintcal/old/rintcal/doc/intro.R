## ---- eval=FALSE--------------------------------------------------------------
#  install.packages('rintcal')

## ---- eval=FALSE--------------------------------------------------------------
#  update.packages()

## -----------------------------------------------------------------------------
library(rintcal)

## -----------------------------------------------------------------------------
ic20 <- ccurve()
head(ic20)

## ---- eval=FALSE--------------------------------------------------------------
#  ?ccurve

## -----------------------------------------------------------------------------
list.ccurves()

## -----------------------------------------------------------------------------
intcal <- intcal.read.data()
IrishOaks <- intcal.data.frames(intcal, taxon="Quercus sp.", country="Ireland")
length(IrishOaks)

## ---- fig.width=4, fig.asp=.8-------------------------------------------------
Bristle <- intcal.data.frames(intcal, taxon="Pinus longaeva")
Bristle_yearly <- Bristle[[20]]$data[,c(8,14,15)]
plot(Bristle_yearly[,1], Bristle_yearly[,2], xlab="cal BP", ylab="C14 BP")
segments(Bristle_yearly[,1], Bristle_yearly[,2]-Bristle_yearly[,3], Bristle_yearly[,1], Bristle_yearly[,2]+Bristle_yearly[,3])

## -----------------------------------------------------------------------------
mix.ccurves(0.4, cc1="IntCal20", cc2="Marine20", offset=c(100, 20))

## ---- fig.width=4, fig.asp=.8-------------------------------------------------
glued <- glue.ccurves("IntCal20", "NH1")
plot(glued[1:650,1:2], xlab="cal BP", ylab="C-14 BP", pch=".")

