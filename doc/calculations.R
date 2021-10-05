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
set.seed(123)
dates <- sort(runif(5, 100, 1000))
errors <- .05*dates
depths <- 0:4
labels <- c("my", "very", "own", "simulated", "dates")
draw.dates(dates, errors, depths, BCAD=TRUE, labels=labels, cal.lim=range(dates, 600, 1700))

## ---- fig.width=4, fig.asp=1--------------------------------------------------
plot(0, type="n", xlim=c(600, 1700), ylim=c(5,-1), xlab="AD", ylab="dates")
draw.dates(dates, errors, depths, BCAD=TRUE, add=TRUE, labels=labels)

