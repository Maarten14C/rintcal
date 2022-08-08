#' @name draw.ccurve
#' @title Draw a calibration curve.
#' @description Draw one or two of the calibration curves, or add a calibration curve to an existing plot.
#' @return The calibration curve (invisible).
#' @param cal1 First calendar year for the plot
#' @param cal2 Last calendar year for the plot
#' @param cc1 Name of the calibration curve. Can be "IntCal20", "Marine20", "SHCal20", or for the previous curves "IntCal13", "Marine13" or "SHCal13". Can also be "nh1", "nh2", "nh3", "sh1-2", "sh3", "nh1_monthly", "nh1_monthly", "nh2_monthly", "nh3_monthly", "sh1-2_monthly", "sh3_monthly", "Kure", "LevinKromer" or "Santos" for postbomb curves.
#' @param cc2 Optional second calibration curve to plot. Can be "IntCal20", "Marine20", "SHCal20", or for the previous curves "IntCal13", "Marine13" or "SHCal13". Defaults to nothing, NA.
#' @param cc1.postbomb Use \code{postbomb=TRUE} to get a postbomb calibration curve for cc1 (default \code{cc1.postbomb=FALSE}).
#' @param cc2.postbomb Use \code{postbomb=TRUE} to get a postbomb calibration curve for cc2 (default \code{cc2.postbomb=FALSE}).
#' @param BCAD The calendar scale of graphs and age output-files is in cal BP (calendar or calibrated years before the present, where the present is AD 1950) by default, but can be changed to BC/AD using \code{BCAD=TRUE}.
#' @param cal.lab The labels for the calendar axis (default \code{age.lab="cal BP"} or \code{"BC/AD"} if \code{BCAD=TRUE}), or to \code{age.lab="kcal BP"} etc. if ka=TRUE.
#' @param cal.rev Reverse the calendar axis. 
#' @param c14.lab Label for the C-14 axis. Defaults to 14C BP (or 14C kBP if ka=TRUE).
#' @param c14.lim Axis limits for the C-14 axis. Calculated automatically by default. 
#' @param c14.rev Reverse the C-14 axis.
#' @param ka Use kcal BP (and C14 kBP).
#' @param add.yaxis Whether or not to plot the second calibration. Defaults to \code{add.yaxis=FALSE}.
#' @param cc1.col Colour of the calibration curve (outline).
#' @param cc1.fill Colour of the calibration curve (fill).
#' @param cc2.col Colour of the calibration curve (outline), if activated (default cc2=NA).
#' @param cc2.fill Colour of the calibration curve (fill), if activated (default cc2=NA).
#' @param add Whether or not to add the curve(s) to an existing plot. Defaults to FALSE, which draws a new plot
#' @param bty Draw a box around a box of a certain shape. Defaults to \code{bty="l"}.
#' @param ccdir Directory of the calibration curves. Defaults to where the package's files are stored (system.file), but can be set to, e.g., \code{ccdir="curves"}.
#' @param ... Any additional optional plotting parameters. 
#' @examples 
#' draw.ccurve()
#' draw.ccurve(1000, 3000, cc2="Marine20")
#' draw.ccurve(1800, 2020, BCAD=TRUE, cc2="nh1", cc2.postbomb=TRUE)
#' draw.ccurve(1800, 2010, BCAD=TRUE, cc2="nh1", add.yaxis=TRUE)
#' @export
draw.ccurve <- function(cal1=-50, cal2=55e3, cc1="IntCal20", cc2=NA, cc1.postbomb=FALSE, cc2.postbomb=FALSE, BCAD=FALSE, cal.lab=NA, cal.rev=FALSE, c14.lab=NA, c14.lim=NA, c14.rev=FALSE, ka=FALSE, add.yaxis=FALSE, cc1.col=rgb(0,0,1,.5), cc1.fill=rgb(0,0,1,.2), cc2.col=rgb(0,.5,0,.5), cc2.fill=rgb(0,.5,0,.2), add=FALSE, bty="l", ccdir=NULL, ...) {

  # read and narrow down the calibration curve(s)
  cc.1 <- ccurve(cc1, cc1.postbomb, ccdir)
  if(BCAD)
    cc.1[,1] <- 1950 - cc.1[,1] 
  mindat <- cc.1[,1] >= min(cal1, cal2)
  maxdat <- cc.1[,1] <= max(cal1, cal2)
  cc.1 <- cc.1[which(mindat * maxdat == 1),]
  if(ka)
    cc.1 <- cc.1/1e3
  cc1.pol <- cbind(c(cc.1[,1], rev(cc.1[,1])), c(cc.1[,2]-cc.1[,3], rev(cc.1[,2]+cc.1[,3])))
  
  if(!is.na(cc2)) {
    cc.2 <- ccurve(cc2, cc2.postbomb, ccdir)
    if(BCAD)
      cc.2[,1] <- 1950 - cc.2[,1] 
    mindat <- cc.2[,1] >= min(cal1, cal2)
    maxdat <- cc.2[,1] <= max(cal1, cal2)
    cc.2 <- cc.2[which(mindat * maxdat == 1),]
    if(ka)
      cc.2 <- cc.2/1e3
    cc2.pol <- cbind(c(cc.2[,1], rev(cc.2[,1])), c(cc.2[,2]-cc.2[,3], rev(cc.2[,2]+cc.2[,3])))
  }

  if(!add) { # then prepare plotting parameters
    if(is.na(cal.lab))
      if(ka) {
        if(BCAD) 
          cal.lab <- "ka BC/AD" else
            cal.lab <- "kcal BP"
      } else
        if(BCAD)
          cal.lab <- "BC/AD" else
            cal.lab <- "cal. yr BP"
    if(is.na(c14.lab))
      if(ka)
        c14.lab <- expression(""^14*C~kBP) else
          c14.lab <- expression(""^14*C~BP)

    cal.lim <- c(cal1, cal2)
    if(cal.rev)
      cal.lim <- rev(cal.lim)
    if(ka) 
      cal.lim <- cal.lim/1e3

    if(is.na(c14.lim[1]))
      if(is.na(cc2))
        c14.lim <- range(cc1.pol[,2]) else
          if(add.yaxis)
            c14.lim <- range(cc1.pol[,2]) else
              c14.lim <- range(cc1.pol[,2], cc2.pol[,2])
    if(c14.rev)
      c14.lim <- rev(c14.lim)

    # draw the graph and data
    plot(0, type="n", xlim=cal.lim, xlab=cal.lab, ylim=c14.lim, ylab=c14.lab, bty=bty, ...)
  }

  # add the calibration curve
  polygon(cc1.pol, col=cc1.fill, border=NA) # calibration curve
  lines(cc.1[,1], cc.1[,2]-cc.1[,3], col=cc1.col)
  lines(cc.1[,1], cc.1[,2]+cc.1[,3], col=cc1.col)

  # add a second curve?
  if(!is.na(cc2)) {
    if(add.yaxis) {
      op <- par(new=TRUE)
      plot(cc2.pol, type="n", xlim=cal.lim, xlab="", ylab="", bty="n", xaxt="n", yaxt="n")
    }
    polygon(cc2.pol, col=cc2.fill, border=NA) # calibration curve
    lines(cc.2[,1], cc.2[,2]-cc.2[,3], col=cc2.col)
    lines(cc.2[,1], cc.2[,2]+cc.2[,3], col=cc2.col)
    if(add.yaxis)
      axis(4, col=cc2.col, col.axis=cc2.col)
  }
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
#' By default the Gaussian distribution is used to calibrate dates. For use of the t distribution (Christen and Perez 2016) instead,
#' set \code{normal=FALSE} provide values for t.a and t.b (defaults to \code{t.a=3} and \code{t.b=4}).
#'
#' Calibrated distributions are usually reduced to their 68\% or 95\% calibrated ranges, taking into account the asymmetric
#' and multi-peaked shape of these distributions.
#' Calibrated ranges at 68\% will obviously result in narrower confidence intervals, and a perceived higher precision, than 95\% ranges. However, given the often
#' asymmetric and multi-modal nature of calibrated distributions, the probability that the 'true' calendar date
#' lies outside the 1 standard deviation hpd ranges is considerable (c. 32\%). Therefore the use of 95\% calibrated ranges is preferable,
#' and default.
#'
#' Negative radiocarbon ages are calibrated with postbomb curves, but the user needs to tell which curve to use.
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
#' @param cc Calibration curve for C-14 dates (1, 2, 3, or 4, or, e.g., "IntCal20", "Marine20", "SHCal20", "nh1", "sh3", or "mixed").
#' @param postbomb Whether or not this is a postbomb age. Defaults to FALSE. 
#' @param reservoir Reservoir age, or reservoir age and age offset.
#' @param prob Probability confidence intervals (between 0 and 1).
#' @param BCAD Use BC/AD or cal BP scale (default cal BP).
#' @param ka Use thousands of years instead of years in the plots and hpd ranges. Defaults to FALSE. 
#' @param cal.lab Label of the calendar/horizontal axis. Defaults to the calendar scale, but alternative names can be provided.
#' @param C14.lab Label of the C-14/vertical axis. Defaults to the 14C scale, but alternative names can be provided.
#' @param cal.lim Minimum and maximum of calendar axis (default calculated automatically).
#' @param C14.lim Minimum and maximum of C-14 axis (default calculated automatically).
#' @param cc.col Colour of the lines of the calibration curve. Defaults to semi-transparent dark green; \code{cc.col=rgb(0,.5,0,0.7)}.
#' @param cc.fill Colour of the inner part of the calibration curve. Defaults to semi-transparent dark green; \code{cc.col=rgb(0,.5,0,0.7)}.
#' @param date.col Colour of the "dot-bar" plot of the C14 date. Defaults to \code{date.col="red"}.
#' @param dist.col Colour of the outer lines of the distributions. Defaults to semi-transparent grey, \code{dist.col=rgb(0,0,0,0.2)}.
#' @param dist.fill Colour of the inner part of the distributions. Defaults to semi-transparent grey, \code{dist.col=rgb(0,0,0,0.2)}.
#' @param hpd.fill Colour of the highest posterior density. Defaults to semi-transparent grey, \code{dist.col=rgb(0,0,0,0.3)}.
#' @param dist.height Maximum height of the C14 and calibrated distributions (as proportion of the invisible secondary axes). Defaults to 0.3.
#' @param cal.rev Whether or not to reverse the direction of the calendar axis.
#' @param yr.steps Temporal resolution at which C-14 ages are calibrated (in calendar years). By default follows the spacing in the calibration curve.
#' @param threshold Below which value should probabilities be excluded from calculations.
#' @param edge How to treat dates are at or beyond the edge of the calibration curve. If dates are truncated, a warning is given. If they lie beyond the calibration curve, an error is given.
#' @param normal Use the normal distribution to calibrate dates (default TRUE). The alternative is to use the t model (Christen and Perez 2016).
#' @param t.a Value a of the t distribution (defaults to 3).
#' @param t.b Value a of the t distribution (defaults to 4).
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
#' @param ccdir Directory of the calibration curves. Defaults to where the package's files are stored (system.file), but can be set to, e.g., \code{ccdir="curves"}.
#' @param ... Other plotting parameters.
#' @return A graph of the raw and calibrated C-14 date, the calibrated ranges and, invisibly, the calibrated distribution and hpd ranges.
#' @examples
#' calibrate()
#' calibrate(130, 20)
#' cal <- calibrate(2550, 20, reservoir=100)
#' cal; plot(cal[[1]])
#' calibrate(130, 20, prob=0.68)
#' calibrate(age=130, error=20, BCAD=TRUE)
#' calibrate(4450, 40, reservoir=c(100, 50))
#' @export
calibrate <- function(age=2450, error=50, cc=1, postbomb=FALSE, reservoir=0, prob=0.95, BCAD=FALSE, ka=FALSE, cal.lab=c(), C14.lab=c(), cal.lim=c(), C14.lim=c(), cc.col=rgb(0,.5,0,0.7), cc.fill=rgb(0,.5,0,0.7), date.col="red", dist.col=rgb(0,0,0,0.2), dist.fill=rgb(0,0,0,0.2), hpd.fill=rgb(0,0,0,0.3), dist.height=0.3, cal.rev=FALSE, yr.steps=FALSE, threshold=0.0005, edge=TRUE, normal=TRUE, t.a=3, t.b=4, rounded=1, extend.range=.05, legend.cex=0.8, legend1.loc="topleft", legend2.loc="topright", mgp=c(2,1,0), mar=c(3,3,1,1), xaxs="i", yaxs="i", bty="l", ccdir=NULL, ...) {
  # read the data
  age <- age-reservoir[1]
  if(length(reservoir) > 1)
    error <- sqrt(error^2 + reservoir[2]^2)
  Cc <- ccurve(cc, postbomb, ccdir)
  
  # warn/stop if the date lies (partly) beyond the calibration curve
  if(edge) {
    border <- 0
    if(age-2*error < min(Cc[,2]-2*Cc[,3]))
      if(age+2*error > min(Cc[,2]+2*Cc[,3]))
        border <- 1 else border <- 2
    if(age+2*error > max(Cc[,2]-2*Cc[,3]))
      if(age-2*error < min(Cc[,2]+2*Cc[,3]))
        border <- 1 else border <- 2
    if(border == 1)
      message("Date falls partly beyond calibration curve and will be truncated!")
    if(border == 2)
      stop("Cannot calibrate dates beyond calibration curve!")
  }

  # calculate the raw and calibrated distributions
  C14.dist <- caldist(age, error, cc=0, BCAD=FALSE)
  C14.hpd <- hpd(C14.dist, return.raw=TRUE)[[1]]
  C14.hpd <- C14.hpd[which(C14.hpd[,3] == 1),1:2] # extract only the values within the hpd
  C14.hpd <- rbind(c(C14.hpd[1,1],0), C14.hpd, c(C14.hpd[nrow(C14.hpd),1],0))
  dat <- caldist(age, error, cc=cc, yrsteps=yr.steps, threshold=threshold, normal=normal, t.a=t.a, t.b=t.b, BCAD=FALSE, postbomb=postbomb, ccdir=ccdir)
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

  # deal with drawing ages truncated by the calibration curve
  cal.hpd[which(cal.hpd[,3] == 0),2] <- 0
  if(cal.hpd[nrow(cal.hpd),1] == Cc[nrow(Cc),1])
    cal.hpd <- rbind(cal.hpd, c(cal.hpd[nrow(cal.hpd),1],0,0))
  if(cal.hpd[1,1] == Cc[1,1])
    cal.hpd <- rbind(c(cal.hpd[1,1],0,1), cal.hpd)
  if(cal.dist[nrow(cal.dist),1] == Cc[nrow(Cc),1])
    cal.dist <- rbind(cal.dist, c(cal.dist[nrow(cal.dist),1],0))
#  if(cal.dist[1,1] == Cc[1,1])
  cal.dist <- rbind(c(cal.dist[1,1],0), cal.dist)
  cal.dist <- rbind(cal.dist, c(cal.dist[nrow(cal.dist),1],0))

  # calculate limits
  if(length(cal.lim) == 0) {
    cal.lim <- range(dat[,1])
  lims <- cal.lim
  cal.lim <- rev(extendrange(cal.lim, f=extend.range))
  if(cal.rev)
    cal.lim <- rev(cal.lim)
  } else
    lims <- cal.lim
  if(length(C14.lim) == 0) {
    cc.min <- max(1, min(which(Cc[,1] >= min(cal.lim))))
    cc.max <- min(nrow(Cc), max(which(Cc[,1] <= max(cal.lim))))
    # we don't need the entire calibration curve
    Cc <- Cc[cc.min:cc.max,]
    cc.lim <- extendrange(c(Cc[,2]-Cc[,3], Cc[,2]+Cc[,3], C14.dist[,1]), f=extend.range)
  } else {
     cc.min <- min(1, which(Cc[,2] >= min(C14.lim)))
     cc.max <- max(nrow(Cc), which(Cc[,2] <= max(C14.lim)))
     Cc <- Cc[cc.min:cc.max,]
     cc.lim <- range(C14.lim)
  }
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
cat("\n", cal.lim, "\n")
  # adapt axis labels and hpds if BCAD and/or ka
  xaxt <- ifelse(BCAD || ka, "n", "s")
  yaxt <- ifelse(ka, "n", "s")
  plot(0, type="n", xlim=cal.lim, ylim=cc.lim, xlab=cal.lab, ylab=C14.lab, xaxt=xaxt, yaxt=yaxt, xaxs=xaxs, yaxs=yaxs, bty=bty, mgp=mgp, mar=mar)
  if(BCAD) {
    cal.dist[,1] <- 1950 - cal.dist[,1]
    if(ka) {
      axis(1, pretty(cal.lim), labels=1.950-pretty(cal.lim/1e3)) 
      axis(2, pretty(cc.lim), labels=pretty(cc.lim/1e3)) 
      cal.dist[,1] <- cal.dist[,1]/1e3
      hpds[,1:2] <- hpds[,1:2]/1e3 
    } else
        axis(1, pretty(cal.lim), labels=1950-pretty(cal.lim))
    hpds[,1:2] <- 1950 - hpds[,1:2]
    colnames(cal.dist)[1] <- "BC/AD"
  } else 
    if(ka) {
      axis(1, pretty(cal.lim), labels=pretty(cal.lim/1e3)) 
      axis(2, pretty(cc.lim), labels=pretty(cc.lim/1e3)) 
      cal.dist[,1] <- cal.dist[,1]/1e3
      hpds[,1:2] <- hpds[,1:2]/1e3
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



#' @name draw.dates
#' @title add calibrated distributions to a plot.
#' @description Add individual calibrated dates to a plot.
#' @param age Mean of the uncalibrated C-14 age (or multiple ages).
#' @param error Error of the uncalibrated C-14 age (or ages).
#' @param depth Depth(s) of the date(s)
#' @param cc Calibration curve for C-14 dates (1, 2, 3, or 4, or, e.g., "IntCal20", "Marine20", "SHCal20", "nh1", "sh3", or "mixed"). If there are multiple dates but all use the same calibration curve, one value can be provided. 
#' @param postbomb Whether or not this is a postbomb age. Defaults to FALSE. 
#' @param reservoir Reservoir age, or reservoir age and age offset.
#' @param normal Use the normal distribution to calibrate dates (default TRUE). The alternative is to use the t model (Christen and Perez 2016).
#' @param t.a Value a of the t distribution (defaults to 3).
#' @param t.b Value a of the t distribution (defaults to 4).#' @param prob Probability confidence intervals (between 0 and 1).
#' @param threshold Report only values above a threshold. Defaults to \code{threshold=0.001}.
#' @param BCAD Use BC/AD or cal BP scale (default cal BP).
#' @param ex Exaggeration of the height of the distribution
#' @param normalise If TRUE, the date is normalised by setting its peak value to 1 (handy for estimating how high to draw it). If there are multiple dates, it is normalised to the peak of the most precise date. Otherwise the peak of each date is at the same height.
#' @param draw.hpd Whether or not to draw the hpd ranges as a line
#' @param hpd.lwd Width of the line of the hpd ranges
#' @param hpd.col Colour of the hpd rectangle
#' @param mirror Plot distributions mirrored, a bit like a swan. Confuses some people but looks nice to the author so is the default.
#' @param up If mirror is set to FALSE, the distribution can be plotted up or down, depending on the direction of the axis.
#' @param on.axis Which axis to plot on. Defaults to 'x' or 1, but can be set to 'y' or 2. 
#' @param col Colour of the inside of the distribution
#' @param border Colour of the border of the distribution
#' @param add Whether or not to add the dates to an existing plot. If set to FALSE (default), a plot will be set up.
#' @param cal.lab Title of the calendar axis (if present)
#' @param cal.lim Limits of the calendar axis (if present)
#' @param y.lab Title of the vertical axis (if present)
#' @param y.lim Limits of the vertical axis (if present)
#' @param y.rev Reverse the y-axis. Defaults to TRUE
#' @param labels Add labels to the dates. Empty by default.
#' @param label.x Horizontal position of the date labels. By default draws them before the youngest age (1), but can also draw them after the oldest age (2), or above its mean (3). 
#' @param label.y Vertical positions of the labels. Defaults to 0 (or 1 if label.x is 3 or 4).
#' @param label.offset Offsets of the positions of the labels, giving the x and y offsets. Defaults to c(0,0).
#' @param label.cex Size of labels. 
#' @param label.col Colour of the labels. Defaults to the colour given to the borders of the dates.
#' @param label.adj  Justification of the labels. Follows R's adj option: A value of ‘0’ produces left-justified text, ‘0.5’ (the default) centered text and ‘1’ right-justified text. 
#' @param label.rot Rotation of the label. 0 by default (horizontal).
#' @param ccdir Directory of the calibration curves. Defaults to where the package's files are stored (system.file), but can be set to, e.g., \code{ccdir="curves"}.
#' @param ... Additional plotting options
#' @examples
#'   plot(0, xlim=c(500,0), ylim=c(0, 2))
#'   draw.dates(130, 20, depth=1) 
#' @export
draw.dates <- function(age, error, depth, cc=1, postbomb=FALSE, reservoir=c(), normal=TRUE, t.a=3, t.b=4, prob=0.95, threshold=.001, BCAD=FALSE, ex=.9, normalise=TRUE, draw.hpd=TRUE, hpd.lwd=2, hpd.col=rgb(0,0,1,.7), mirror=TRUE, up=FALSE, on.axis=1, col=rgb(0,0,1,.3), border=rgb(0,0,1,.5), add=FALSE, cal.lab=c(), cal.lim=c(), y.lab=c(), y.lim=c(), y.rev=TRUE, labels=c(), label.x=1, label.y=c(), label.cex=0.8, label.col=border, label.offset=c(0,0), label.adj=c(1,0), label.rot=0, ccdir=NULL, ...) {
  if(length(reservoir) > 0) {
    age <- age - reservoir[1]
    if(length(reservoir) > 1)
      error <- sqrt(error^2 + reservoir[2]^2)
  }
  
  # deal with multiple dates
  if(length(age) > 1) {
    if(length(cc) == 1)
      cc <- rep(cc, length(age)) 
    if(length(postbomb) == 1)
      postbomb <- rep(postbomb, length(age))
    if(length(hpd.lwd) == 1)
      hpd.lwd <- rep(hpd.lwd, length(age))
    if(length(hpd.col) == 1)
      hpd.col <- rep(hpd.col, length(age))
    if(length(col) == 1)
      col <- rep(col, length(age))
    if(length(border) == 1)
      border <- rep(border, length(age))
  }
  
  max.hght <- 0; age.range <- c();
  for(i in 1:length(age)) {
    tmp <- caldist(age[i], error[i], cc=cc[i], postbomb=postbomb[i], normal=normal, t.a=t.a, t.b=t.b, BCAD=BCAD, ccdir=ccdir)
    tmp <- approx(tmp[,1], tmp[,2], min(tmp[,1]):max(tmp[,1]))
    tmp <- cbind(tmp$x, tmp$y/sum(tmp$y))
    max.hght <- max(max.hght, tmp[,2])
    age.range <- range(age.range, range(tmp[,1]))
  }
  
  if(!add) {
    if(length(cal.lab) == 0)
      cal.lab <- ifelse(BCAD, "BC/AD", "cal BP")
    if(length(y.lab) == 0)
      y.lab <- "depth"
    if(length(cal.lim) == 0)
      cal.lim <- age.range
    if(!BCAD)
      cal.lim <- cal.lim[2:1]
    if(length(y.lim) == 0)
      y.lim <- range(depth, depth-ex, depth+ex)
    if(y.rev)
      y.lim <- rev(y.lim)
    plot(0, type="n", xlim=cal.lim, xlab=cal.lab, ylim=y.lim, ylab=y.lab, ...)
  }
  
  for(i in 1:length(age)) {
    dat <- hpd(caldist(age[i], error[i], cc=cc[i], postbomb=postbomb[i], normal=normal, t.a=t.a, t.b=t.b, BCAD=BCAD, threshold=threshold, ccdir=ccdir), prob, return.raw=TRUE)
    probs <- dat[[1]]
    hpds <- dat[[2]]

    probs <- approx(probs[,1], probs[,2], min(probs[,1]):max(probs[,1]))
    probs <- cbind(probs$x, probs$y/sum(probs$y))

#    if(min(diff(probs[,1])) >=1) # if
#      if(length(unique(diff(probs[,1]))) > 1)
#        probs[,2] <- probs[,2] / max(diff(probs[,1]))

    if(normalise)
      probs[,2] <- probs[,2] / (max.hght) else # most precise date peaks at 1. Correcting for different calcurve steps
        probs[,2] <- probs[,2]/max(probs[,2])

    if(mirror) {
      pol <- cbind(c(probs[,1], rev(probs[,1])), depth[i]+(ex/2)*c(probs[,2], -rev(probs[,2])))
      } else
        if(up)
          pol <- cbind(c(probs[1,1], probs[,1], probs[nrow(probs),1]), depth[i]+ex*c(0, probs[,2], 0)) else
            pol <- cbind(c(probs[1,1], probs[,1], probs[nrow(probs),1]), depth[i]-ex*c(0, probs[,2], 0))

    if(on.axis == 'y' || on.axis == 2)
      pol <- pol[,2:1]
    polygon(pol, col=col[i], border=border[i])
  
    if(draw.hpd)
      segments(hpds[,1], depth[i], hpds[,2], depth[i], lwd=hpd.lwd, col=hpd.col)

    if(length(labels) > 0) {
      xx <- c(min(probs[,1]), max(probs[,1]), mean(probs[,1]))
      if(!BCAD) xx <- xx[c(2,1,3)]
      x <- xx[label.x]
      if(length(label.y) == 0) {
        y <- depth[i]   
        if(label.x > 2)
          ifelse(up, y <- y+1, y <- y-1)
      }
      text(x+label.offset[1], y+label.offset[2], labels[i], cex=label.cex, col=label.col, adj=label.adj, srt=label.rot)
    }
  }
}

