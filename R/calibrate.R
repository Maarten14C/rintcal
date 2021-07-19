# functions copied and adapted from the clam R package



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
caldist <- function(age, error, cc=1, postbomb=FALSE, yrsteps=FALSE, threshold=1e-3, calibt=FALSE, BCAD=FALSE, rule=1) {
  # deal with cal BP and negative ages	
  if(cc == 0) { # no ccurve needed
    xseq <- seq(age-5*error, age+5*error, length=1e3) # hard-coded values
    cc <- cbind(xseq, xseq, rep(0, length(xseq)))
  } else {
    if(age < 0)
      if(!postbomb)
        if(!(cc %in% c("nh1", "nh2", "nh3", "sh1-2", "sh3")))
          stop("This appears to be a postbomb age. Please provide a postbomb curve")
    cc <- ccurve(cc)
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



#' @name calibrate
#' @title Plot individual calibrated dates.
#' @description Calibrate individual 14C dates, plot them and report calibrated ranges.
#' @details
#' Type \code{calibrate()} to see how a date of 2450 +- 50 14C BP gets calibrated (the calibration curve happens to show
#' a plateau around this 14C age). To calibrate a different date, provide its reported mean and error (1
#' standard deviation error as reported by the radiocarbon laboratory) as follows: \code{calibrate(mean, error)},
#' e.g., for a date of 130 +- 20 14C BP, type calibrate\code{(age=130, error=20)} or, shorter, \code{calibrate(130,20)}.
#'
#' In case the date has a reservoir effect or age offset, e.g. of 100 14C years, provide this as follows:
#' \code{calibrate(130, 20, reservoir=100)}. If you want to include an uncertainty for this offset, provide this as follows,
#' e.g., for an uncertainty of 50yr, \code{calibrate(130,20,reservoir=c(100, 50))}.
#' The uncertainty for the age offset will then be added to the error (by taking the square root of the sum
#' of the squared error and the squared offset uncertainty). If the carbon of your sample has mixed marine/terrestrial sources,
#' instead apply the marine offset using mix.curves and calibrate the date using that custom-built curve (cc="mixed").
#'
#' If you prefer to work with, e.g., 68 \% as opposed to the default 95 \% confidence intervals,
#' type: \code{calibrate(130, 20, prob=0.68)} or \code{calibrate(130, 20,, 0.68)} (the commas between the brackets indicate the position of the option;
#' the standard deviation is the fourth option of the \code{calibrate} function). The calibrated distribution can be calculated
#' for every single calendar year (\code{yrsteps=1}) within a wide range of the 14C date. Probabilities below a threshold (default \code{threshold=0.0005}) will be neglected.
#'
#' By default the northern hemisphere terrestrial calibration curve is used (\code{cc=1 or cc1="IntCal20"}).
#' To use alternative curves, use \code{cc=2} (\code{cc2="Marine20"}), \code{cc=3} (\code{cc3="SHCal20C"}),
#' \code{cc=4} (\code{cc4="mixed.14C"}), or specify a postbomb curve (e.g., \code{cc="nh1"}).
#'
#' Calibrate works in cal BP (calendar years before AD 1950) by default, but can work with cal BC/AD through the option \code{BCAD=TRUE}.
#'
#' By default the Gaussian distribution is used to calibrate dates. For use of the student-t distribution instead,
#' provide two sensible values, e.g., \code{calibt=c(3,4)}.
#'
#' Calibrated distributions are usually reduced to their 68\% or 95\% calibrated ranges, taking into account the asymmetric
#' and multi-peaked shape of these distributions.
#' Calibrated ranges at 68\% will obviously result in narrower confidence intervals, and a perceived higher precision, than 95\% ranges. However, given the often
#' asymmetric and multi-modal nature of calibrated distributions, the probability that the 'true' calendar date
#' lies outside the 1 standard deviation hpd ranges is considerable (c. 32\%). Therefore the use of 95\% calibrated ranges is preferable,
#' and default.
#'
#' Negative radiocarbon ages are calibrated with postbomb curves, but the user needs to tell clam which curve to use.
#' For example, to use the first of the three northern hemisphere curves, provide the option \code{cc="nh1"}, \code{cc="nh2"}, \code{cc="nh3"},
#' while for southern hemisphere samples, use \code{cc="sh1-2"} or \code{cc="sh3"}.
#'
#' A graph of the calibration is produced, and it can be adapted in several ways.
#' The limits of the horizontal (calendar scale) and vertical (14C scale) axes are calculated automatically
#' but can be changed by providing alternative values for the options \code{cal.lim, C14.lim}.
#' The titles of both axis can be changed by providing alternative titles to \code{cal.lab} and/or \code{C14.lab}. The heights of the distributions of the 14C and calibrated
#' ages can be set to alternative values using \code{dist.height} (default \code{0.3} which plots the distribution up to 30\% of the height of the entire graph).
#' Parameters for white space around the
#' graph can be changed (default \code{mar=c(3.5, 2, 2, 1}) for spacing below, to the left, above and to the right respectively),
#' as can the spacing for the axis labels (\code{mgp=c(2,1,0)}). By default, the axes are connected at the lower left, \code{bty="l"}.
#' Check the R documentation of \code{par()} for more options.
#'
#' The colours of the 14C date, the calibration curve, the distributions, and the highest posterior density (hpd)
#' ranges, can be changed by providing an alternative colour in \code{date.col}, \code{cc.col}, \code{dist.col}, and/or \code{hpd.col}, respectively.
#' The default colours are transparent grey for the dates probability distributions (\code{dist.col=rgb(0,0,0, 0.3)} and \code{sd.col=rgb(0,0,0, 0.5)};
#' change the last value of rgb for different greyscale values), red for the uncalibrated mean and error bars (\code{date.col="red"}),
#' and transparent green for the calibration curve (\code{cc.col=rgb(0, 0.5, 0, 0.7)}). R's rgb() function expects values between \code{0} and \code{1}
#' for red, green and blue, respectively, followed by a value for the semi-transparency (also between 0 and 1). Some graphic devices
#' such as postscript are unable to use transparency; in that case provide different colours or leave the fourth value empty.
#' @param age Mean of the uncalibrated C-14 age.
#' @param error Error of the uncalibrated C-14 age.
#' @param reservoir Reservoir age, or reservoir age and age offset.
#' @param prob Probability confidence intervals (between 0 and 1).
#' @param cc Calibration curve for C-14 dates (1, 2, 3, or 4, or, e.g., "IntCal20", "Marine20", "SHCal20", "nh1", "sh3", or "mixed").
#' @param BCAD Use BC/AD or cal BP scale (default cal BP).
#' @param cal.lab Label of the calendar/horizontal axis. Defaults to the calendar scale, but alternative names can be provided.
#' @param C14.lab Label of the C-14/vertical axis. Defaults to the 14C scale, but alternative names can be provided.
#' @param cal.lim Minimum and maximum of calendar axis (default calculated automatically).
#' @param C14.lim Minimum and maximum of C-14 axis (default calculated automatically).
#' @param cc.col Colour of the lines of the calibration curve. Defaults to semi-transparent dark green; \code{cc.col=rgb(0,.5,0,0.7)}.
#' @param cc.fill Colour of the inner part of the calibration curve. Defaults to semi-transparent dark green; \code{cc.col=rgb(0,.5,0,0.7)}.
#' @param date.col Colour of the "dot-bar" plot of the C14 date. Defaults to \code{date.col="red"}.
#' @param dist.col Colour of the outer lines of the distributions. Defaults to semi-transparent grey, \code{dist.col=rgb(0,0,0,0.2)}.
#' @param dist.fill Colour of the inner part of the distributions. Defaults to semi-transparent grey, \code{dist.col=rgb(0,0,0,0.2)}.
#' @param dist.height Maximum height of the C14 and calibrated distributions (as proportion of the invisible secondary axes). Defaults to 0.3.
#' @param cal.rev Whether or not to reverse the direction of the calendar axis.
#' @param yr.steps Temporal resolution at which C-14 ages are calibrated (in calendar years). By default follows the spacing in the calibration curve.
#' @param threshold Below which value should probabilities be excluded from calculations.
#' @param calibt Calibration based on the student-t distribution. By default, the Gaussian distribution is used (\code{calibt=FALSE}). To use the student-t distribution, provide two parameters such as \code{calibt=c(3,4)}.
#' @param rounded Rounding of the percentages of the reported hpd ranges. Defaults to 1 decimal.
#' @param extend.range Range by which the axes are extended beyond the data limits. Defaults to 5\%.
#' @param legend.cex Size of the font of the legends. Defaults to 0.8.
#' @param legend1.loc Where the first legend (with the calibration curve name and the uncalibrated date) is plotted. Defaults to topleft.
#' @param legend2.loc Where the second legend (with the hpd ranges) is plotted. Defaults to topright.
#' @param mgp Axis text margins (where should titles, labels and tick marks be plotted).
#' @param mar Plot margins (amount of white space along edges of axes 1-4).
#' @param xaxs Whether or not to extend the limits of the horizontal axis. Defaults to \code{xaxs="i"} which does not extend the limits.
#' @param yaxs Whether or not to extend the limits of the vertical axis. Defaults to \code{yaxs="i"} which does not extend the limits.
#' @param bty Draw a box around the graph ("n" for none, and "l", "7", "c", "u", "]" or "o" for correspondingly shaped boxes).
#' @param ... Other plotting parameters.
#' @return A graph of the raw and calibrated C-14 date, the calibrated ranges and, invisibly, the calibrated distribution and hpd ranges.
#' @examples
#' calibrate()
#' calibrate(130, 20)
#' cal <- calibrate(2550, 20, reservoir=100)
#' cal; plot(cal$calib)
#' calibrate(130, 20, prob=0.68)
#' calibrate(age=130, error=20, BCAD=TRUE)
#' calibrate(4450, 40, reservoir=c(100, 50))
#' @export
calibrate <- function(age=2450, error=50, reservoir=0, prob=0.95, cc=1, BCAD=FALSE, cal.lab=c(), C14.lab=c(), cal.lim=c(), C14.lim=c(), cc.col=rgb(0,.5,0,0.7), cc.fill=rgb(0,.5,0,0.7), date.col="red", dist.col=rgb(0,0,0,0.2), dist.fill=rgb(0,0,0,0.2), hpd.fill=rgb(0,0,0,0.3), dist.height=0.3, cal.rev=FALSE, yr.steps=FALSE, threshold=0.0005, calibt=FALSE, rounded=1, extend.range=.05, legend.cex=0.8, legend1.loc="topleft", legend2.loc="topright", mgp=c(2,1,0), mar=c(2,2,1,1), xaxs="i", yaxs="i", bty="l", ...) {
  # read the data
  age <- age-reservoir[1]
  if(length(reservoir) > 1)
    error <- sqrt(error^2 + reservoir[2]^2)
  Cc <- ccurve(cc)

  # calculate the raw and calibrated distributions
  C14.dist <- caldist(age, error, cc=0, BCAD=FALSE)
  C14.hpd <- hpd(C14.dist, return.raw=TRUE)[[1]]
  C14.hpd <- C14.hpd[which(C14.hpd[,3] == 1),1:2] # extract only the values within the hpd
  C14.hpd <- rbind(c(C14.hpd[1,1],0), C14.hpd, c(C14.hpd[nrow(C14.hpd),1],0))
  dat <- caldist(age, error, cc=cc, yrsteps=yr.steps, threshold=threshold, calibt, BCAD=FALSE)
  cal.hpd <- hpd(dat, prob=prob, return.raw=TRUE, rounded=rounded)
  hpds <- cal.hpd[[2]]
  cal.hpd <- cal.hpd[[1]]
  cal.dist <- cal.hpd[,1:2]

  # copy entries at edges of calibrated hpds, to ensure rectangular polygons
  before <- rbind(cal.hpd[which(diff(c(0,cal.hpd[,3])) == 1),])
  after <- rbind(cal.hpd[which(diff(c(cal.hpd[,3])) == -1),])
  if(length(before) > 2) {
    before[,3] <- 0 # and set their hpds to 0
    cal.hpd <- rbind(before, cal.hpd)
    cal.hpd <- cal.hpd[order(cal.hpd[,1]),]
  }
  if(length(after) > 2) {
    after[,3] <- 0
    cal.hpd <- rbind(cal.hpd, after)
    cal.hpd <- cal.hpd[order(cal.hpd[,1]),]
  }

  # deal with ages truncated by the calibration curve
  cal.hpd[which(cal.hpd[,3] == 0),2] <- 0
  if(cal.hpd[nrow(cal.hpd),1] == Cc[nrow(Cc),1])
    cal.hpd <- rbind(cal.hpd, c(cal.hpd[nrow(cal.hpd),1],0,0))
  if(cal.hpd[1,1] == Cc[1,1])
    cal.hpd <- rbind(c(cal.hpd[1,1],0,1), cal.hpd)
  if(cal.dist[nrow(cal.dist),1] == Cc[nrow(Cc),1])
    cal.dist <- rbind(cal.dist, c(cal.dist[nrow(cal.dist),1],0))
  if(cal.dist[1,1] == Cc[1,1])
    cal.dist <- rbind(c(cal.dist[1,1],0), cal.dist)

  # calculate limits
  if(length(cal.lim) == 0)
    cal.lim <- range(dat[,1])
  lims <- cal.lim
  cal.lim <- rev(extendrange(cal.lim, f=extend.range))
  if(cal.rev)
    cal.lim <- rev(cal.lim)
  if(length(C14.lim) == 0) {
    cc.min <- max(1, min(which(Cc[,1] >= min(cal.lim))))
    cc.max <- min(nrow(Cc), max(which(Cc[,1] <= max(cal.lim))))
  } else {
     cc.min <- min(which(Cc[,2] >= min(C14.lim)))
     cc.max <- max(which(Cc[,2] <= max(C14.lim)))
  }

  # we don't need the entire calibration curve
  Cc <- Cc[cc.min:cc.max,]
  cc.lim <- extendrange(c(Cc[,2]-Cc[,3], Cc[,2]+Cc[,3], C14.dist[,1]), f=extend.range)
  ccpol <- cbind(c(Cc[,1], rev(Cc[,1])), c(Cc[,2]-Cc[,3], rev(Cc[,2]+Cc[,3])))

  # to plot the probability distributions on the age-scales, they need to be transposed
  prob2age <- function(prob, age1, age2, prob1=min(prob[,2]), prob2=max(prob[,2])) {
    if(BCAD) {
      a <- (age1 - age2) / (prob1 - prob2)
      b <- age1 + (a * prob1)
      return(cbind(prob[,1], b + dist.height*a*prob[,2]))
    } else {
        a <- (age2 - age1) / (prob2 - prob1)
        b <- age1 + (a * prob1)
        return(cbind(prob[,1], b + dist.height*a*prob[,2]))
    }
  }
  tC14.dist <- prob2age(C14.dist, cal.lim[1], cal.lim[2])
  tC14.hpd <- prob2age(C14.hpd, cal.lim[1], cal.lim[2])
  tcal.dist <- prob2age(cal.dist, cc.lim[1], cc.lim[2])
  tcal.hpd <- prob2age(cal.hpd, cc.lim[1], cc.lim[2])

  # plot
  if(length(cal.lab) == 0)
    if(BCAD)
      cal.lab <- "BC/AD" else
        cal.lab <- "cal BP"
  if(length(C14.lab) == 0)
    C14.lab <- expression(""^14*C~BP)

  # adapt axis labels and hpds if BCAD
  xaxt <- ifelse(BCAD, "n", "s")
  plot(0, type="n", xlim=cal.lim, ylim=cc.lim, xlab=cal.lab, ylab=C14.lab, xaxt=xaxt, xaxs=xaxs, yaxs=yaxs, bty=bty, mgp=mgp, mar=mar)
  if(BCAD) {
    axis(1, pretty(cal.lim), labels=1950-pretty(cal.lim))
    hpds[,1:2] <- 1950 - hpds[,1:2]
    rownames(hpds) <- "BC/AD"
    colnames(cal.dist)[1] <- "BC/AD"
    cal.dist[,1] <- 1950 - cal.dist[,1]
  }

  # draw the data
  polygon(ccpol, border=cc.col, col=cc.fill)
  polygon(tC14.dist[,2:1], border=dist.col, col=dist.fill)
  polygon(tC14.hpd[,2:1], border=NA, col=hpd.fill)
  polygon(tcal.dist, border=dist.col, col=dist.fill)
  polygon(tcal.hpd, border=dist.col, col=hpd.fill)
  dot <- ifelse(cal.rev, min(lims), max(lims))
  points(dot, age, col=date.col, pch=20)
  segments(dot, age-error, dot, age+error, col=date.col)

  # legends
  if(cc == 1)
    cc <- "IntCal20 " else
    if(cc == 2)
      cc <- "Marine20 " else
        if(cc == 3)
          cc <- "SHCal20 "
  legend(legend1.loc, legend=c(cc, paste(age, "\u00B1", error)), text.col=c(cc.col, 1),  ncol=1, bty="n", cex=legend.cex)
  legend(legend2.loc, legend=rbind(c("from", "to", "%"), cbind(hpds)), ncol=3, bty="n", cex=legend.cex)

  invisible(list(cal.dist, hpds))
}


