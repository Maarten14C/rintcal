# functions copied and adapted from clam R package


# See Christen and Perez 2009, Radiocarbon 51:1047-1059. Instead of assuming the standard Gaussian model (default in clam), a student t distribution can be used with two parameters. Christen and Perez 2009 suggest t.a = 3 and t.b = 4; this can be put as clam( calibt=c(3,4) )
calibt <- function(t.a, t.b, f.cage, f.error, f.mu, f.sigma) # removed theta as par
  (t.b + ((f.cage-f.mu)^2) / (2*(f.sigma^2 + f.error^2))) ^ (-1*(t.a+0.5))




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
#' @examples
#' calib <- caldist(130,20)
#' plot(calib, type="l")
#' postbomb <- caldist(-3030, 20, "nh1", BCAD=TRUE)
#' @export
caldist <- function(age, error, cc=1, postbomb=FALSE, yrsteps=FALSE, threshold=1e-6, calibt=FALSE, BCAD=FALSE, rule=1) {
  if(age < 0)
    if(!postbomb)
      if(!(cc %in% c("nh1", "nh2", "nh3", "sh1-2", "sh3")))
      stop("This appears to be a postbomb age. Please provide a postbomb curve")
  cc <- ccurve(cc)

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
  above <- which(cal[,2] >= threshold)
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
#' @param caldist The calibrated distribution, as returned from caldist()
#' @param prob Probability range which should be calculated. Default \code{prob=0.95}.
#' @param hpdsteps It works.
#' @param yrsteps This too. Could be used e.g. for postbomb dates?
#' @param rule Which extrapolation rule to use. Defaults to \code{rule=1} which returns NAs.
#' @examples
#' hpd(caldist(130,20))
#' @export
hpd <- function(caldist, prob=0.95, hpdsteps=1, yrsteps=FALSE, rule=1) {
  # interpolate and rank the ages according to their calibrated distribution probabilities
  if(yrsteps)
    yrsteps <- seq(min(caldist[,1]), max(caldist[,1]), by=yrsteps) else
      yrsteps <- caldist[,1]
  dat <- approx(caldist[,1], caldist[,2], yrsteps, rule=rule)
  o <- order(dat$y, decreasing=TRUE)
  dat <- cbind(dat$x[o], dat$y[o]/sum(dat$y))

  # only retain those ages with cumulative normalised probabilities within required percentage
  dat <- dat[which(cumsum(dat[,2]) <= prob),]
  dat <- dat[order(dat[,1]),]

  # identify any individual ranges within the hpd range and calculate their probability
  dif <- which(diff(dat[,1]) > hpdsteps) # a bit unclear if this will always work, e.g. w postbomb dates
  if(length(dif) == 0) # then there's one single range
    hpds <- cbind(min(dat[,1]), max(dat[,1]), 100*prob) else {
      dif <- c(dat[1,1], sort(c(dat[dif,1], dat[dif+1,1])), dat[nrow(dat),1])
      dif <- matrix(dif, ncol=2, byrow=TRUE)
      colnames(dif) <- c("from", "to")
      prob <- numeric(nrow(dif))
      for(i in 1:nrow(dif))
        prob[i] <- round(100*sum(dat[which(dat[,1]==dif[i,1]):which(dat[,1]==dif[i,2]),2]), 1)
        hpds <- cbind(dif, prob)
      }
  return(hpds)
}



