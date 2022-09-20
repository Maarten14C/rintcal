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
#'   pMC.age(.8, 0.005, 1) # pMC expressed against 1 (not against 100\%), throws a warning, use F14C.age instead
#' @export
pMC.age <- function(mn, sdev=c(), ratio=100, decimals=0) {
  if(ratio !=100)
    warning("pMC.age expects a ratio of 100. For ratio=1, use F14C.age")
  y <- -8033 * log(mn/ratio)
  if(length(sdev) == 0)
    signif(y, decimals) else {
    sdev <- y - -8033 * log((mn+sdev)/ratio)
    round(c(y, sdev), decimals)
  }
}



#' @name age.pMC
#' @title Calculate pMC values from C14 ages
#' @description Calculate pMC values from radiocarbon ages
#' @details Post-bomb dates are often reported as pMC or percent modern carbon. Since Bacon expects radiocarbon ages,
#' this function can be used to calculate pMC values from radiocarbon ages. The reverse function of \link{pMC.age}.
#' @param mn Reported mean of the 14C age.
#' @param sdev Reported error of the 14C age.
#' @param ratio Most modern-date values are reported against \code{100}. If it is against \code{1} instead, a warning is provided; use \code{age.F14C}.
#' @param decimals Amount of decimals required for the pMC value.
#' @return pMC values from C14 ages.
#' @examples
#'   age.pMC(-2000, 20)
#'   age.pMC(-2000, 20, 1)
#' @export
age.pMC <- function(mn, sdev=c(), ratio=100, decimals=3) {
  if(ratio !=100)
    warning("age.pMC expects a ratio of 100. For ratio=1, use age.F14C")
  y <- exp(-mn / 8033)
  if(length(sdev) == 0)
    signif(ratio*y, decimals) else {
    sdev <- y - exp(-(mn + sdev) / 8033)
    signif(ratio*cbind(y, sdev), decimals)
  }
}



#' @name F14C.age
#' @title Calculate C14 ages from F14C values.
#' @description Calculate C14 ages from F14C values of radiocarbon dates.
#' @details Post-bomb dates are often reported as F14C or fraction modern carbon. Since Bacon expects radiocarbon ages,
#'  this function can be used to calculate radiocarbon ages from F14C values. The reverse function is \link{age.F14C}.
#' @param mn Reported mean of the F14C
#' @param sdev Reported error of the F14C. Returns just the mean if left empty.
#' @param decimals Amount of decimals required for the radiocarbon age.
#' @return Radiocarbon ages from F14C values. If F14C values are above 100\%, the resulting radiocarbon ages will be negative.
#' @examples
#'   F14C.age(1.10, 0.5) # a postbomb date, so with a negative 14C age
#'   F14C.age(.80, 0.5) # prebomb dates can also be calculated
#' @export
F14C.age <- function(mn, sdev=c(), decimals=0) {
  y <- -8033 * log(mn)
  if(length(sdev) == 0)
    signif(y, decimals) else {
    sdev <- y - -8033 * log((mn+sdev))
    signif(cbind(y, sdev), decimals)
  }
}



#' @name age.F14C
#' @title Calculate F14C values from C14 ages
#' @description Calculate F14C values from radiocarbon ages
#' @details Post-bomb dates are often reported as F14C or fraction modern carbon. Since Bacon expects radiocarbon ages,
#' this function can be used to calculate F14C values from radiocarbon ages. The reverse function of \link{F14C.age}.
#' @param mn Reported mean of the 14C age.
#' @param sdev Reported error of the 14C age. If left empty, will translate mn to F14C.
#' @param decimals Amount of decimals required for the F14C value.
#' @return F14C values from C14 ages.
#' @examples
#'   age.F14C(-2000, 20)
#' @export
age.F14C <- function(mn, sdev=c(), decimals=3) {
  y <- exp(-mn / 8033)
  if(length(sdev) == 0)
    signif(y, decimals) else {
      sdev <- y - exp(-(mn + sdev) / 8033)
      signif(cbind(y, sdev), decimals)
    }
}



#' @name D14C.F14C
#' @title Transform D14C into F14C
#' @details As explained by Heaton et al. 2020 (Radiocarbon), 14C measurements are commonly expressed in
#' three domains: Delta14C, F14C and the radiocarbon age. This function translates Delta14C, the historical level of Delta14C in the year t cal BP, to F14C values. Note that per convention, this function uses the Cambridge half-life, not the Libby half-life.
#' @param D14C The Delta14C value to translate
#' @param the cal BP age
#' @return The corresponding F14C value
#' @examples
#' D14C.F14C(-10, 238)
#' @export
D14C.F14C <- function(D14C, t)
  return(1/1000 * D14C + 1) * exp(-(t/8267))



#' @name F14C.D14C
#' @title Transform F14C into D14C
#' @details As explained by Heaton et al. 2020 (Radiocarbon), 14C measurements are commonly expressed in
#' three domains: Delta14C, F14C and the radiocarbon age. This function translates F14C values into Delta14C, the historical level of Delta14C in the year t cal BP. Note that per convention, this function uses the Cambridge half-life, not the Libby half-life.
#' @param F14C The F14C value to translate
#' @param the cal BP age
#' @return The corresponding D14C value
#' @examples
#' F14C.D14C(0.985, 222)
#' cc <- ccurve()
#' plot IntCal20 as D14C:
#' cc.Fmin <- age.F14C(cc[,2]+cc[,3])
#' cc.Fmax <- age.F14C(cc[,2]-cc[,3])
#' cc.D14Cmin <- F14C.D14C(cc.Fmin, cc[,1])
#' cc.D14Cmax <- F14C.D14C(cc.Fmax, cc[,1])
#' plot(cc[,1]/1e3, cc.D14Cmax, type="l", xlab="kcal BP", ylab=expression(paste(Delta, ""^{14}, "C")))
#' lines(cc[,1]/1e3, cc.D14Cmin)
#' @export
F14C.D14C <- function(F14C, t)
  return(1000* ((F14C/exp(-(t/8267)))-1))



#' @name caldist
#' @title Calculate calibrated distribution
#' @description Calculate the calibrated distribution of a radiocarbon date.
#' @return The probability distribution(s) as two columns: cal BP ages and their associated probabilities
#' @param age Uncalibrated radiocarbon age
#' @param error Lab error of the radiocarbon age
#' @param cc Calibration curve to use. Defaults to IntCal20 (\code{cc=1}).
#' @param postbomb Whether or not to use a postbomb curve. Required for negative radiocarbon ages.
#' @param yrsteps Steps to use for interpolation. Defaults to the cal BP steps in the calibration curve
#' @param dist.res As an alternative to yrsteps, provide the amount of 'bins' in the distribution
#' @param threshold Report only values above a threshold. Defaults to \code{threshold=1e-6}.
#' @param normal Use the normal distribution to calibrate dates (default TRUE). The alternative is to use the t model (Christen and Perez 2016).
#' @param t.a Value a of the t distribution (defaults to 3).
#' @param t.b Value a of the t distribution (defaults to 4).
#' @param BCAD Which calendar scale to use. Defaults to cal BP, \code{BCAD=FALSE}.
#' @param rule Which extrapolation rule to use. Defaults to \code{rule=1} which returns NAs.
#' @param ccdir Directory of the calibration curves. Defaults to where the package's files are stored (system.file), but can be set to, e.g., \code{ccdir="curves"}.
#' @examples
#' calib <- caldist(130,20)
#' plot(calib, type="l")
#' postbomb <- caldist(-3030, 20, "nh1", BCAD=TRUE)
#' @export
caldist <- function(age, error, cc=1, postbomb=FALSE, yrsteps=FALSE, dist.res=200, threshold=1e-3, normal=TRUE, t.a=3, t.b=4, BCAD=FALSE, rule=1, ccdir=NULL) {
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
  if(normal)
    cal <- cbind(cc[,1], dnorm(cc[,2], age, sqrt(error^2+cc[,3]^2))) else
      cal <- cbind(cc[,1], (t.b + ((age-cc[,2])^2) / (2*(cc[,3]^2 + error^2))) ^ (-1*(t.a+0.5)))
  
  # interpolate and normalise calibrated distribution to 1
  if(yrsteps)
    yrsteps <- seq(min(cal[,1]), max(cal[,1]), by=yrsteps) else
      yrsteps <- cal[,1]
 #     yrsteps <- seq(min(cal[,1]), max(cal[,1]), length=dist.res)
  cal <- approx(cal[,1], cal[,2], yrsteps, rule=rule)
  # cal <- cbind(cal$x, cal$y/sum(cal$y)) # normalise
  cal <- cbind(cal$x, cal$y) # do NOT normalise (not to peaks, nor to sums)

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



#' @name point.estimates
#' @title Calculate a point estimate
#' @description Calculate a point estimate of a calibrated distribution - either the weighted mean, the median or the mode (maximum). Note that point estimates often tend to be very poor representations of entire calibrated distributions, so please be careful and do not reduce entire calibrated distributions to just 1 point value.
#' @return The chosen point estimates
#' @param calib The calibrated distribution, as returned from caldist()
#' @param wmean Report the weighted mean (defaults to TRUE)
#' @param median Report the median (defaults to TRUE)
#' @param mode Report the mode, which is the year with the maximum probability (defaults to TRUE)
#' @param midpoint Report the midpoint of the hpd range(s)
#' @param probability range for the hpd range(s)
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



# calculate the impacts of contamination
#' @name contaminate
#' @title Simulate the impact of contamination on a radiocarbon age
#' @description Given a certain radiocarbon age, calculate the observed impact of contamination with a ratio of material with a different 14C content (for example, 1% contamination with modern carbon)
#' @return The observed radiocarbon age and error
#' @param y the true radiocarbon age
#' @param er the error of the true radiocarbon age
#' @param fraction Relative amount of contamination. Must be between 0 and 1
#' @param F14C the F14C of the contamination. Set at 1 for carbon of modern radiocarbon age, at 0 for 14C-free carbon, or anywhere inbetween.
#' @param F14C.er error of the contamination. Defaults to 0.
#' @author Maarten Blaauw
#' @examples
#' contaminate(5000, 20, .01, 1) # 1% contamination with modern carbon
#' Impacts of different amounts of contamination with modern carbon:
#' real.14C <- seq(0, 50e3, length=200)
#' contam <- seq(0, .1, length=101) # 0 to 10% contamination
#' contam.col <- rainbow(length(contam))
#' plot(0, type="n", xlim=c(0, 55e3), xlab="real 14C age", ylim=range(real.14C), ylab="observed 14C age")
#' for(i in 1:length(contam))
#'   lines(real.14C, contamination(real.14C, c(), contam[i], 1, decimals=5), col=contam.col[i])
#' contam.legend <- seq(0, .1, length=6)
#' contam.col <- rainbow(length(contam.legend))
#' text(52e3, contaminate(50e3, c(), contam.legend, 1), labels=contam.legend, col=contam.col, cex=.7)
#' @export
contaminate <- function(y, sdev=c(), fraction, F14C, F14C.er=0, decimals=5) {
  y.F <- age.F14C(y, sdev, decimals)
  mn <- ((1-fraction)*y.F[,1]) + (fraction*F14C)
  if(length(sdev) == 0)
    return(F14C.age(mn, c(), decimals)) else {
      er <- sqrt(y.F[,2]^2 + F14C.er^2)
      return(F14C.age(mn, er, decimals))
    }
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
