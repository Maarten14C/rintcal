## ---- include=FALSE-----------------------------------------------------------
require(IntCal)

## -----------------------------------------------------------------------------
pMC.age(150, 1)

## -----------------------------------------------------------------------------
age.pMC(-2300, 40)

## ---- fig.width=4, fig.asp=.8-------------------------------------------------
calib.130 <- caldist(130, 20, BCAD=TRUE)
plot(calib.130, type="l")

## -----------------------------------------------------------------------------
hpd(calib.130)

## ---- fig.width=5, fig.asp=1--------------------------------------------------
calibrate(130,20)

## ---- fig.width=4, fig.asp=1--------------------------------------------------
dates <- sort(runif(5, 100, 1000))
errors <- .05*dates
depths <- 4:0
labels <- c("my", "very", "own", "simulated", "dates")
draw.dates(dates, errors, depths, BCAD=TRUE, labels=labels)

## ---- fig.width=4, fig.asp=1--------------------------------------------------
plot(0, type="n", xlim=c(600, 1850), ylim=c(0,5), xlab="AD", ylab="dates")
draw.dates(dates, errors, depths, BCAD=TRUE, add=TRUE, labels=labels)

