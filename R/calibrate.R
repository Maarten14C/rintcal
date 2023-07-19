# functions copied and adapted from the clam R package (by the same author and he agreed)


#' @name caldist
#' @title Calculate calibrated distribution
#' @description Calculate the calibrated distribution of a radiocarbon date.
#' @return The probability distribution(s) as two columns: cal BP ages and their associated probabilities
#' @param age Uncalibrated radiocarbon age
#' @param error Lab error of the radiocarbon age
#' @param cc Calibration curve to use. Defaults to IntCal20 (\code{cc=1}).
#' @param postbomb Whether or not to use a postbomb curve. Required for negative radiocarbon ages.
#' @param thiscurve As an alternative to providing cc and/or postbomb, the data of a specific curve can be provided (3 columns: cal BP, C14 age, error). Defaults to FALSE. 
#' @param yrsteps Steps to use for interpolation. Defaults to the cal BP steps in the calibration curve
#' @param cc.resample The IntCal20 curves have different densities (every year between 0 and 5 kcal BP, then every 5 yr up to 15 kcal BP, then every 10 yr up to 25 kcal BP, and then every 20 yr up to 55 kcal BP). If calibrated ages span these density ranges, their drawn heights can differ, as can their total areas (which should ideally all sum to the same size). To account for this, resample to a constant time-span, using, e.g., \code{cc.resample=5} for 5-yr timespanes.
#' @param dist.res As an alternative to yrsteps, provide the amount of 'bins' in the distribution
#' @param threshold Report only values above a threshold. Defaults to \code{threshold=1e-6}.
#' @param normal Use the normal distribution to calibrate dates (default TRUE). The alternative is to use the t model (Christen and Perez 2016).
#' @param t.a Value a of the t distribution (defaults to 3).
#' @param t.b Value a of the t distribution (defaults to 4).
#' @param normalise Sum the entire calibrated distribution to 1. Defaults to \code{normalise=TRUE}.
#' @param BCAD Which calendar scale to use. Defaults to cal BP, \code{BCAD=FALSE}.
#' @param rule Which extrapolation rule to use. Defaults to \code{rule=1} which returns NAs.
#' @param cc.dir Directory of the calibration curves. Defaults to where the package's files are stored (system.file), but can be set to, e.g., \code{cc.dir="curves"}.
#' @examples
#' calib <- caldist(130,20)
#' plot(calib, type="l")
#' postbomb <- caldist(-3030, 20, "nh1", BCAD=TRUE)
#' @export
caldist <- function(age, error, cc=1, postbomb=FALSE, thiscurve=c(), yrsteps=FALSE, cc.resample=FALSE, dist.res=200, threshold=1e-3, normal=TRUE, t.a=3, t.b=4, normalise=TRUE, BCAD=FALSE, rule=1, cc.dir=NULL) {
	
  if(length(thiscurve) == 0) {
    if(cc == 0) { # no ccurve needed
      xseq <- seq(age-8*error, age+8*error, length=2e3) # hard-coded values, hmmm
      cc <- cbind(xseq, xseq, rep(0, length(xseq)))
    } else
        if(age < 3*error) { # was age - error < 0
          if(!postbomb)
            if(!(cc %in% c("nh1", "nh2", "nh3", "sh1-2", "sh3")))
              stop("This appears to be a postbomb age or close to being so. Please provide a postbomb curve")
		  cc <- glue.ccurves(cc, postbomb, cc.dir)
	    } else
          cc <- ccurve(cc, postbomb=postbomb, cc.dir, resample=cc.resample)
    } else
      cc <- thiscurve
  
  # calibrate; find how far age (measurement) is from cc[,2] of calibration curve
  if(normal)
    cal <- cbind(cc[,1], dnorm(cc[,2], age, sqrt(error^2+cc[,3]^2))) else
      cal <- cbind(cc[,1], (t.b + ((age-cc[,2])^2) / (2*(cc[,3]^2 + error^2))) ^ (-1*(t.a+0.5)))
  
  # interpolate and normalise calibrated distribution to 1
  if(postbomb)
    if(!yrsteps)
      yrsteps <- 0.05 # enough detail to enable calculation of hpd ranges also for postbomb dates
  if(yrsteps)
    yrsteps <- seq(min(cal[,1]), max(cal[,1]), by=yrsteps) else
      yrsteps <- cal[,1]
 #     yrsteps <- seq(min(cal[,1]), max(cal[,1]), length=dist.res)
  cal <- approx(cal[,1], cal[,2], yrsteps, rule=rule)
  # cal <- cbind(cal$x, cal$y/sum(cal$y)) # normalise
  cal <- cbind(cal$x, cal$y) 

  if(normalise)
    cal[,2] <- cal[,2]/sum(cal[,2])
  # remove years with very small probabilities on the extremes of the distribution
  above <- which(cal[,2] >= (threshold * max(cal[,2]))) # relative to its peak
  cal <- cal[min(above):max(above),] # now does not necessarily sum to exactly 1 any more

  colnames(cal) <- c("cal BP", "prob")
  if(BCAD) {
    yrs <- 1950 - cal[,1]
    colnames(cal)[1] <- "BC/AD"
    yrs[yrs<=0] <- yrs[yrs<=0] - 1 # 0 BC/AD does not exist
    cal[,1] <- yrs
  }

  return(cal)
}



#' @name point.estimates
#' @title Calculate a point estimate
#' @description Calculate a point estimate of a calibrated distribution - either the weighted mean, the median or the mode (maximum). Note that point estimates often tend to be very poor representations of entire calibrated distributions, so please be careful and do not reduce entire calibrated distributions to just 1 point value.
#' @return The chosen point estimates
#' @param calib The calibrated distribution, as returned from caldist()
#' @param wmean Report the weighted mean (defaults to TRUE)
#' @param median Report the median (defaults to TRUE)
#' @param mode Report the mode, which is the year with the maximum probability (defaults to TRUE)
#' @param midpoint Report the midpoint of the hpd range(s)
#' @param prob probability range for the hpd range(s)
#' @param rounded Rounding for reported probabilities. Defaults to 1 decimal.
#' @examples
#' point.estimates(caldist(130,20))
#' plot(tmp <- caldist(2450,50), type='l')
#' abline(v=point.estimates(tmp), col=1:4)
#' @export
point.estimates <- function(calib, wmean=TRUE, median=TRUE, mode=TRUE, midpoint=TRUE, prob=.95, rounded=1) {
  to.report <- c()
  name <- c()

  if(wmean) {
    wmean <- weighted.mean(calib[,1], calib[,2])
    to.report <- c(to.report, wmean)
    name <- c(name, "weighted mean")
  }
  if(median) {
    median <- approx(cumsum(calib[,2]), calib[,1], 0.5)$y
    to.report <- c(to.report, median)
    name <- c(name, "median")
  }
  if(mode) {
    mode <- calib[which(calib[,2] == max(calib[,2]))[1],1]
    to.report <- c(to.report, mode)
    name <- c(name, "mode")
  }
  if(midpoint) {
    midpoint <- range(hpd(calib, prob)[,1:2])
    midpoint <- midpoint[1] + (midpoint[2]-midpoint[1])/2
    to.report <- c(to.report, midpoint)
    name <- c(name, "midpoint")
  }

  names(to.report) <- name
  return(round(to.report, rounded))
}



#' @name hpd
#' @title Calculate highest posterior density
#' @description Calculate highest posterior density ranges of calibrated distribution
#' @return The highest posterior density ranges, as three columns: from age, to age, and the corresponding percentage(s) of the range(s)
#' @param calib The calibrated distribution, as returned from caldist()
#' @param prob Probability range which should be calculated. Default \code{prob=0.95}.
#' @param return.raw The raw data to calculate hpds can be returned, e.g. to draw polygons of the calibrated distributions. Defaults to \code{return.raw=FALSE}.
#' @param rounded Rounding for reported probabilities. Defaults to 1 decimal.
#' @examples
#' hpd(caldist(130,20))
#' plot(tmp <- caldist(2450,50), type='l')
#' abline(v=hpd(tmp)[,1:2], col=4)
#' @export
hpd <- function(calib, prob=0.95, return.raw=FALSE, rounded=1) {
  # rank the calibrated ages according to their probabilities (normalised to be sure)
  calib[,2] <- calib[,2] / sum(calib[,2]) # does not necessarily sum to 1
  o <- order(calib[,2], decreasing=TRUE)
  summed <- cbind(calib[o,1], cumsum(calib[o,2])/sum(calib[,2]))

  # find the ages that fall within the hpd range
  summed <- cbind(summed[,1], summed[,2] <= prob)
  BCAD <- ifelse(min(diff(calib[,1])) < 0, TRUE, FALSE) # christ...
  o <- order(summed[,1], decreasing=BCAD) # put ages ascending again
  calib <- cbind(calib, summed[o,2]) # add a column indicating ages within ranges
  
  # find the outer ages of the calibrated ranges. The 0 should help with truncated ages
  to <- calib[which( diff(c(0, calib[,3])) == 1), 1]
  from <- calib[which( diff(c(calib[,3], 0)) == -1), 1]
  to <- sort(to, ifelse(BCAD, FALSE, TRUE)) # sort from oldest to youngest
  from <- sort(from, ifelse(BCAD, FALSE, TRUE))

  # find the probability 'area' within each range (as %)
  perc <- 0
  for(i in 1:length(from)) {
    fromto <- which(calib[,1] == from[i]) : which(calib[,1] == to[i])
    perc[i] <- round(100*sum(calib[fromto,2]), rounded)
  }

  if(return.raw)
    return(list(calib, cbind(from, to, perc))) else
      return(cbind(from, to, perc))
}



#' @name calBP.14C
#' @title Find the 14C age and error belonging to a cal BP age.
#' @description Given a calendar age, the calibration curve (default cc=1) is interpolated and the corresponding 14C age and error are returned.
#' @details Interpolation is used, and values outside the calibration curve are given as NA. For negative cal BP ages, a postbomb curve will have to be provided. 
#' @return The calibration-curve 14C year belonging to the entered cal BP age
#' @param yr The cal BP year.
#' @param cc calibration curve for C14 (see \code{caldist()}).
#' @param postbomb Whether or not to use a postbomb curve (see \code{caldist()}).
#' @param rule How should R's approx function deal with extrapolation. If \code{rule=1}, the default, then NAs are returned for such points and if it is 2, the value at the closest data extreme is used.
#' @param cc.dir Directory of the calibration curves. Defaults to where the package's files are stored (system.file), but can be set to, e.g., \code{cc.dir="curves"}.
#' @author Maarten Blaauw
#' @examples
#' calBP.14C(100)
#' @export
calBP.14C <- function(yr, cc=1, postbomb=FALSE, rule=1, cc.dir=NULL) {
  cc <- ccurve(cc, postbomb, cc.dir)
  mu <- approx(cc[,1], cc[,2], yr, rule=rule)$y
  er <- approx(cc[,1], cc[,3], yr, rule=rule)$y
  return(c(mu, er))
}





#  find the calibrated probability of a calendar age for a 14C date
#' @name l.calib
#' @title Find the calibrated probability of a calendar age for a 14C date. 
#' @description Find the calibrated probability of a cal BP age for a radiocarbon date. Can handle either multiple calendar ages for a single radiocarbon date, or a single calendar age for multiple radiocarbon dates. 
#' @details The function cannot deal with multiple calibration curves if multiple calendar years or radiocarbon dates are entered.
#' @return The calibrated probability of a calendar age for a 14C age
#' @param yr The cal BP year.
#' @param y The radiocarbon date's mean.
#' @param er The radiocarbon date's lab error.
#' @param cc calibration curve for the radiocarbon date(s) (see \code{ccurve()}).
#' @param normal Use the normal distribution to calibrate dates (default TRUE). The alternative is to use the t model (Christen and Perez 2016).
#' @param t.a Value a of the t distribution (defaults to 3).
#' @param t.b Value b of the t distribution (defaults to 4).
#' @author Maarten Blaauw
#' @examples
#' l.calib(100, 130, 20)
#' l.calib(100:110, 130, 20) # multiple calendar ages of a single date
#' l.calib(100, c(130,150), c(15,20)) # multiple radiocarbon ages and a single calendar age
#' @export
l.calib <- function(yr, y, er, cc=ccurve(1,FALSE), normal=TRUE, t.a=3, t.b=4) {
  cc.y <- approx(cc[,1], cc[,2], yr)$y
  cc.er <- approx(cc[,1], cc[,3], yr)$y
  if(normal)
    prob <- dnorm(y, cc.y, sqrt(cc.er^2 + er^2)) else
      prob <- (t.b + ((y-cc.y)^2) / (2*(sqrt(er^2+cc.er^2)^2))) ^ (-1*(t.a+0.5))
  prob[is.na(prob)] <- 0
  return(prob)
}
