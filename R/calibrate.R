# functions copied and adapted from the clam R package (by the same author and he agreed)



#' @name pMC.age
#' @title Calculate C14 ages from pMC values.
#' @description Calculate C14 ages from pMC values of radiocarbon dates.
#' @details Post-bomb dates are often reported as pMC or percent modern carbon. Since Bacon expects radiocarbon ages,
#'  this function can be used to calculate radiocarbon ages from pMC values. The reverse function is \link{age.pMC}.
#' @param mn Reported mean of the pMC.
#' @param sdev Reported error of the pMC.
#' @param ratio Most modern-date values are reported against \code{100}. If it is against \code{1} instead, use \code{1} here.
#' @param decimals Amount of decimals required for the radiocarbon age.
#' @return Radiocarbon ages from pMC values. If pMC values are above 100\%, the resulting radiocarbon ages will be negative.
#' @examples
#'   pMC.age(110, 0.5) # a postbomb date, so with a negative 14C age
#'   pMC.age(80, 0.5) # prebomb dates can also be calculated
#'   pMC.age(.8, 0.005, 1) # pMC expressed against 1 (not against 100\%)
#' @seealso \url{http://www.qub.ac.uk/chrono/blaauw/manualBacon_2.3.pdf}
#' @export
pMC.age <- function(mn, sdev, ratio=100, decimals=0) {
  y <- -8033 * log(mn/ratio)
  sdev <- y - -8033 * log((mn+sdev)/ratio)
  round(c(y, sdev), decimals)
}



#' @name age.pMC
#' @title Calculate pMC values from C14 ages
#' @description Calculate pMC values from radiocarbon ages
#' @details Post-bomb dates are often reported as pMC or percent modern carbon. Since Bacon expects radiocarbon ages,
#' this function can be used to calculate pMC values from radiocarbon ages. The reverse function of \link{pMC.age}.
#' @param mn Reported mean of the 14C age.
#' @param sdev Reported error of the 14C age.
#' @param ratio Most modern-date values are reported against \code{100}. If it is against \code{1} instead, use \code{1} here.
#' @param decimals Amount of decimals required for the pMC value.
#' @return pMC values from C14 ages.
#' @examples
#'   age.pMC(-2000, 20)
#'   age.pMC(-2000, 20, 1)
#' @export
age.pMC <- function(mn, sdev, ratio=100, decimals=3) {
  y <- exp(-mn / 8033)
  sdev <- y - exp(-(mn + sdev) / 8033)
  signif(ratio*c(y, sdev), decimals)
}



#' @name caldist
#' @title Calculate calibrated distribution
#' @description Calculate the calibrated distribution of a radiocarbon date.
#' @param age Uncalibrated radiocarbon age
#' @param error Lab error of the radiocarbon age
#' @param cc Calibration curve to use. Defaults to IntCal20 (\code{cc=1}).
#' @param postbomb Whether or not to use a postbomb curve. Required for negative radiocarbon ages.
#' @param yrsteps Steps to use for interpolation. Defaults to the cal BP steps in the calibration curve
#' @param threshold Report only values above a threshold. Defaults to \code{threshold=1e-6}.
#' @param calibt Use the student-t distribution as alternative to the normal distribution. Requires 2 parameters, e.g., \code{calibt=c(3,4)}, defaults to FALSE.
#' @param BCAD Which calendar scale to use. Defaults to cal BP, \code{BCAD=FALSE}.
#' @param rule Which extrapolation rule to use. Defaults to \code{rule=1} which returns NAs.
#' @param ccdir Directory of the calibration curves. Defaults to where the package's files are stored (system.file), but can be set to, e.g., \code{ccdir="curves"}.
#' @examples
#' calib <- caldist(130,20)
#' plot(calib, type="l")
#' postbomb <- caldist(-3030, 20, "nh1", BCAD=TRUE)
#' @export
caldist <- function(age, error, cc=1, postbomb=FALSE, yrsteps=FALSE, threshold=1e-3, calibt=FALSE, BCAD=FALSE, rule=1, ccdir=NULL) {
  # deal with cal BP and negative ages	
  if(cc == 0) { # no ccurve needed
    xseq <- seq(age-5*error, age+5*error, length=1e3) # hard-coded values
    cc <- cbind(xseq, xseq, rep(0, length(xseq)))
  } else {
    if(age < 0)
      if(!postbomb)
        if(!(cc %in% c("nh1", "nh2", "nh3", "sh1-2", "sh3")))
          stop("This appears to be a postbomb age. Please provide a postbomb curve")
    cc <- ccurve(cc, postbomb, ccdir)
  }
  
  # calibrate; find how far age (measurement) is from cc[,2] of calibration curve
  if(length(calibt) < 2)
    cal <- cbind(cc[,1], dnorm(cc[,2], age, sqrt(error^2+cc[,3]^2))) else
      cal <- cbind(cc[,1], (calibt[2] + ((age-cc[,2])^2) / (2*(cc[,3]^2 + error^2))) ^ (-1*(calibt[1]+0.5)))

  # interpolate and normalise calibrated distribution to 1
  if(yrsteps)
    yrsteps <- seq(min(cal[,1]), max(cal[,1]), by=yrsteps) else
      yrsteps <- cal[,1]
  cal <- approx(cal[,1], cal[,2], yrsteps, rule=rule)
  cal <- cbind(cal$x, cal$y/sum(cal$y)) # normalise

  # remove years with very small probabilities on the extremes of the distribution
  above <- which(cal[,2] >= threshold * max(cal[,2])) # relative to its peak
  cal <- cal[min(above):max(above),]

  colnames(cal) <- c("cal BP", "prob")
  if(BCAD) {
    cal[,1] <- 1950 - cal[,1]
    colnames(cal)[1] <- "BC/AD"
    if(0 %in% cal[,1])
      cal <- cal[-which(cal[,1] == 0),] # 0 BC/AD does not exist
  }

  return(cal)
}



#' @name hpd
#' @title Calculate highest posterior density
#' @description Calculate highest posterior density ranges of calibrated distribution
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



# find the 14C age of a single cal BP year
#' @name calBP.14C
#' @title Find the 14C age and error belonging to a cal BP age.
#' @description Given a calendar age, the calibration curve (default cc=1) is interpolated and the corresponding 14C age and error are returned.
#' @details Interpolation is used, and values outside the calibration curve are given as NA. For negative cal BP ages, a postbomb curve will have to be provided. 
#' @param yr The cal BP year.
#' @param cc calibration curve for C14 (see \code{caldist()}).
#' @param postbomb Whether or not to use a postbomb curve (see \code{caldist()}).
#' @param rule How should R's approx function deal with extrapolation. If \code{rule=1}, the default, then NAs are returned for such points and if it is 2, the value at the closest data extreme is used.
#' @param ccdir Directory of the calibration curves. Defaults to where the package's files are stored (system.file), but can be set to, e.g., \code{ccdir="curves"}.
#' @author Maarten Blaauw
#' @examples
#' calBP.14C(100)
#' @export
calBP.14C <- function(yr, cc=1, postbomb=FALSE, rule=1, ccdir=NULL) {
  cc <- ccurve(cc, postbomb, ccdir)
  mu <- approx(cc[,1], cc[,2], yr, rule=rule)$y
  er <- approx(cc[,1], cc[,3], yr, rule=rule)$y
  return(c(mu, er))
}


