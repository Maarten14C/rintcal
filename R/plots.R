#' @name draw.ccurve
#' @title Draw a calibration curve.
#' @description Draw one or two of the calibration curves, or add a calibration curve to an existing plot.
#' @return A plot of the calibration curve
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
#' @param cc.dir Directory of the calibration curves. Defaults to where the package's files are stored (system.file), but can be set to, e.g., \code{cc.dir="curves"}.
#' @param legend Location of the legend (only activated if more than one curve is plotted). Plotted in the topleft corner by default. Use \code{legend=c()} to leave empty
#' @param ... Any additional optional plotting parameters. 
#' @examples 
#' draw.ccurve()
#' draw.ccurve(1000, 3000, cc2="Marine20")
#' draw.ccurve(1800, 2020, BCAD=TRUE, cc2="nh1", cc2.postbomb=TRUE)
#' draw.ccurve(1800, 2010, BCAD=TRUE, cc2="nh1", add.yaxis=TRUE)
#' @export
draw.ccurve <- function(cal1=-50, cal2=55e3, cc1="IntCal20", cc2=NA, cc1.postbomb=FALSE, cc2.postbomb=FALSE, BCAD=FALSE, cal.lab=NA, cal.rev=FALSE, c14.lab=NA, c14.lim=NA, c14.rev=FALSE, ka=FALSE, add.yaxis=FALSE, cc1.col=rgb(0,0,1,.5), cc1.fill=rgb(0,0,1,.2), cc2.col=rgb(0,.5,0,.5), cc2.fill=rgb(0,.5,0,.2), add=FALSE, bty="l", cc.dir=NULL, legend="topleft", ...) {

  # read and narrow down the calibration curve(s)
  cc.1 <- ccurve(cc1, cc1.postbomb, cc.dir)
  if(BCAD)
    cc.1[,1] <- 1950 - cc.1[,1] 
  mindat <- cc.1[,1] >= min(cal1, cal2)
  maxdat <- cc.1[,1] <= max(cal1, cal2)
  cc.1 <- cc.1[which(mindat * maxdat == 1),]
  if(ka)
    cc.1 <- cc.1/1e3
  cc1.pol <- cbind(c(cc.1[,1], rev(cc.1[,1])), c(cc.1[,2]-cc.1[,3], rev(cc.1[,2]+cc.1[,3])))
  
  if(!is.na(cc2)) {
    cc.2 <- ccurve(cc2, cc2.postbomb, cc.dir)
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
      oldpar <- par(no.readonly = TRUE)
      on.exit(par(oldpar))
      par(new=TRUE)
      plot(cc2.pol, type="n", xlim=cal.lim, xlab="", ylab="", bty="n", xaxt="n", yaxt="n")
    }
    polygon(cc2.pol, col=cc2.fill, border=NA) # calibration curve
    lines(cc.2[,1], cc.2[,2]-cc.2[,3], col=cc2.col)
    lines(cc.2[,1], cc.2[,2]+cc.2[,3], col=cc2.col)
    if(add.yaxis)
      axis(4, col=cc2.col, col.axis=cc2.col)
    if(length(legend) > 0)
      legend(legend, legend=c(cc1, cc2), text.col=c(cc1.col, cc2.col), bty="n")
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
#' @param cc.dir Directory of the calibration curves. Defaults to where the package's files are stored (system.file), but can be set to, e.g., \code{cc.dir="curves"}.
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
calibrate <- function(age=2450, error=50, cc=1, postbomb=FALSE, reservoir=0, prob=0.95, BCAD=FALSE, ka=FALSE, cal.lab=c(), C14.lab=c(), cal.lim=c(), C14.lim=c(), cc.col=rgb(0,.5,0,0.7), cc.fill=rgb(0,.5,0,0.7), date.col="red", dist.col=rgb(0,0,0,0.2), dist.fill=rgb(0,0,0,0.2), hpd.fill=rgb(0,0,0,0.3), dist.height=0.3, cal.rev=FALSE, yr.steps=FALSE, threshold=0.0005, edge=TRUE, normal=TRUE, t.a=3, t.b=4, rounded=1, extend.range=.05, legend.cex=0.8, legend1.loc="topleft", legend2.loc="topright", mgp=c(2,1,0), mar=c(3,3,1,1), xaxs="i", yaxs="i", bty="l", cc.dir=NULL, ...) {
  # read the data
  age <- age-reservoir[1]
  if(length(reservoir) > 1)
    error <- sqrt(error^2 + reservoir[2]^2)
  Cc <- ccurve(cc, postbomb, cc.dir)
  
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
  C14.dist <- caldist(age, error, cc=0, BCAD=FALSE) # just to draw a normal dist
  C14.hpd <- hpd(C14.dist, return.raw=TRUE)[[1]]
  C14.hpd <- C14.hpd[which(C14.hpd[,3] == 1),1:2] # extract only the values within the hpd
  C14.hpd <- rbind(c(C14.hpd[1,1],0), C14.hpd, c(C14.hpd[nrow(C14.hpd),1],0))
  dat <- caldist(age, error, cc=cc, yrsteps=yr.steps, threshold=threshold, normal=normal, t.a=t.a, t.b=t.b, BCAD=FALSE, postbomb=postbomb, cc.dir=cc.dir)

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
  prob2age <- function(prob, age1, age2, prob1=min(prob[,2]), prob2=max(prob[,2]), bcad=BCAD) {
        a <- (age2 - age1) / (prob2 - prob1)
        b <- age1 + (a * prob1)
        return(cbind(prob[,1], b + dist.height*a*prob[,2]))
  }
  tC14.dist <- prob2age(C14.dist, cal.lim[1], cal.lim[2])
  tC14.hpd <- prob2age(C14.hpd, cal.lim[1], cal.lim[2])
  tcal.dist <- prob2age(cal.dist, cc.lim[1], cc.lim[2])
  tcal.hpd <- prob2age(cal.hpd, cc.lim[1], cc.lim[2])

  # after all calcs, adapt for anything that has to be as BCAD:
  if(BCAD) {
    cal.lim <- 1950-cal.lim
    cal.dist[,1] <- 1950 - cal.dist[,1]
    tC14.dist[,2] <- 1950 - tC14.dist[,2]
    tC14.hpd[,2] <- 1950 - tC14.hpd[,2]
    tcal.dist[,1] <- 1950-tcal.dist[,1]
    tcal.hpd[,1] <- 1950 - tcal.hpd[,1]
    ccpol[,1] <- 1950 - ccpol[,1]
    hpds[,1:2] <- 1950 - hpds[,1:2]
  }

  # plot
  if(length(cal.lab) == 0)
    if(BCAD)
      cal.lab <- "BC/AD" else
        cal.lab <- "cal BP"
  if(length(C14.lab) == 0)
    C14.lab <- expression(""^14*C~BP)
  # adapt axis labels and hpds if BCAD and/or ka
  xaxt <- ifelse(BCAD || ka, "n", "s")
  yaxt <- ifelse(ka, "n", "s")
  plot(0, type="n", xlim=cal.lim, ylim=cc.lim, xlab=cal.lab, ylab=C14.lab, xaxt="n", yaxt="n", xaxs=xaxs, yaxs=yaxs, bty=bty, mgp=mgp, mar=mar)
  if(ka) {
    axis(1, pretty(cal.lim), labels=pretty(cal.lim/1e3))
    axis(2, pretty(cc.lim), labels=pretty(cc.lim/1e3))
    cal.dist[,1] <- cal.dist[,1]/1e3
    hpds[,1:2] <- hpds[,1:2]/1e3
  } else {
     axis(1)
     axis(2)
  }

  # draw the data
  polygon(ccpol, border=cc.col, col=cc.fill)
  polygon(tC14.dist[,2:1], border=dist.col, col=dist.fill)
  polygon(tC14.hpd[,2:1], border=NA, col=hpd.fill)
  polygon(tcal.dist, border=dist.col, col=dist.fill)
  polygon(tcal.hpd, border=dist.col, col=hpd.fill)
  dot <- ifelse(cal.rev, min(lims), max(lims))
  if(BCAD)
    dot <- 1950 - dot
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
#' @description Add individual or multiple calibrated dates to a plot.
#' @return A plot of the (calibrated) dates
#' @param age Mean of the uncalibrated C-14 age (or multiple ages).
#' @param error Error of the uncalibrated C-14 age (or ages).
#' @param depth Depth(s) of the date(s). Can also be their relative positions if no depths are available.
#' @param cc Calibration curve for C-14 dates (1, 2, 3, or 4, or, e.g., "IntCal20", "Marine20", "SHCal20", "nh1", "sh3", or "mixed"). If there are multiple dates but all use the same calibration curve, one value can be provided. 
#' @param postbomb Whether or not this is a postbomb age. Defaults to FALSE. 
#' @param reservoir Reservoir age, or reservoir age and age offset.
#' @param normal Use the normal distribution to calibrate dates (default TRUE). The alternative is to use the t model (Christen and Perez 2009).
#' @param t.a Value a of the t distribution (defaults to 3).
#' @param t.b Value a of the t distribution (defaults to 4).
#' @param prob Probability confidence intervals (between 0 and 1).
#' @param threshold Report only values above a threshold. Defaults to \code{threshold=0.001}.
#' @param BCAD Use BC/AD or cal BP scale (default cal BP).
#' @param draw.hpd Whether or not to draw the hpd ranges as a line
#' @param hpd.lwd Width of the line of the hpd ranges
#' @param hpd.col Colour of the hpd rectangle for all dates or radiocarbon dates
#' @param cal.hpd.col Colour of the hpd rectangle for cal BP dates
#' @param mirror Plot distributions mirrored, a bit like a swan. Confuses some people but looks nice to the author so is the default.
#' @param up If mirror is set to FALSE, the distribution can be plotted up or down, depending on the direction of the axis.
#' @param draw.base By default, the base of the calibrated distributions is plotted. This can be avoided by supplying \code{draw.base=FALSE} as an option.
#' @param col Colour of the inside of the distribution
#' @param border Colour of the border of the distribution
#' @param cal.col Colour of the inside of distribution of non-radiocarbon dates that didn't need calibration
#' @param cal.border Colour of the border of the distribution of non-radiocarbon dates that didn't need calibration
#' @param add Whether or not to add the dates to an existing plot. If set to FALSE (default), a plot will be set up.
#' @param ka Whether or not to plot ages as thousands of years. Defaults to \code{ka=FALSE}.
#' @param rotate.axes By default, the calendar age axis is plotted on the horizontal axis, and depth/position on the vertical one. Use \code{rotate.axes=TRUE} to rotate the axes.
#' @param ex Exaggeration of the height of the distribution, defaults to \code{ex=1}.
#' @param normalise If TRUE, the age distributions are normalised by plotting each distribution with the same total area. Precise dates will therefore peak higher than less precise dates (default). If \code{normalise=FALSE}, the peak of each date will be drawn at the same height.
#' @param cc.resample The IntCal20 curves have different densities (every year between 0 and 5 kcal BP, then every 5 yr up to 15 kcal BP, then every 10 yr up to 25 kcal BP, and then every 20 yr up to 55 kcal BP). If calibrated ages span these density ranges, their drawn heights can differ, as can their total areas (which should ideally all sum to the same size). To account for this, resample to a constant time-span, using, e.g., \code{cc.resample=5} for 5-yr timespanes.
#' @param age.lab Title of the calendar axis (if present)
#' @param age.lim Limits of the calendar axis (if present)
#' @param age.rev Reverse the age axis. Defaults to TRUE
#' @param d.lab Title of the vertical axis (if present)
#' @param d.lim Limits of the vertical axis (if present)
#' @param d.rev Reverse the y-axis. Defaults to TRUE
#' @param labels Add labels to the dates. Empty by default.
#' @param label.x Horizontal position of the date labels. By default draws them before the youngest age (1), but can also draw them after the oldest age (2), or above its mean (3). 
#' @param label.y Vertical positions of the depths/labels. Defaults to 0 (or 1 if label.x is 3 or 4).
#' @param label.offset Offsets of the positions of the depths/labels, giving the x and y offsets. Defaults to c(0,0).
#' @param label.cex Size of labels. 
#' @param label.col Colour of the labels. Defaults to the colour given to the borders of the dates.
#' @param label.adj  Justification of the labels. Follows R's adj option: A value of "0" produces left-justified text, "0.5" (the default) centered text and "1" right-justified text.
#' @param label.rot Rotation of the label. 0 by default (horizontal).
#' @param cc.dir Directory of the calibration curves. Defaults to where the package's files are stored (system.file), but can be set to, e.g., \code{cc.dir="curves"}.
#' @param dist.res Resolution of the distribution polygons. Defaults to \code{dist.res=100}.
#' @param ... Additional plotting options
#' @examples
#'   plot(0, xlim=c(500,0), ylim=c(0, 2))
#'   draw.dates(130, 20, depth=1) 
#' @export
draw.dates <- function(age, error, depth, cc=1, postbomb=FALSE, reservoir=c(), normal=TRUE, t.a=3, t.b=4, prob=0.95, threshold=.001, BCAD=FALSE, draw.hpd=TRUE, hpd.lwd=2, hpd.col=rgb(0,0,1,.7), cal.hpd.col=rgb(0, 0.5, 0.5, 0.35), mirror=TRUE, up=FALSE, draw.base=TRUE, col=rgb(0,0,1,.3), border=rgb(0,0,1,.5), cal.col=rgb(0, 0.5, 0.5, 0.35), cal.border=rgb(0, 0.5, 0.5, 0.35), add=FALSE, ka=FALSE, rotate.axes=FALSE, ex=1, normalise=TRUE, cc.resample=5, age.lab=c(), age.lim=c(), age.rev=FALSE, d.lab=c(), d.lim=c(), d.rev=TRUE, labels=c(), label.x=1, label.y=c(), label.cex=0.8, label.col=border, label.offset=c(0,0), label.adj=c(1,0), label.rot=0, cc.dir=NULL, dist.res=100, ...) {
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
    if(0 %in% cc) { # then we've got cal BP dates
      these <- which(cc==0)
      hpd.col[these] <- cal.hpd.col
      col[these] <- cal.col
      border[these] <- cal.border
    }
  }

  ages <- array(NA, dim=c(dist.res, length(age))) # later fill with years
  probs <- ages # later fill with probs
  mx <- rep(0, length(age))
  hpds <- list()
  for(i in 1:length(age)) {
    tmp <- caldist(age[i], error[i], cc=cc[i], postbomb=postbomb[i], normal=normal, t.a=t.a, t.b=t.b, normalise=normalise, cc.resample=cc.resample, threshold=threshold, BCAD=BCAD, cc.dir=cc.dir)
    hpds[[i]] <- hpd(tmp, prob, return.raw=TRUE)

    tmp <- approx(tmp[,1], tmp[,2], seq(min(tmp[,1]), max(tmp[,1]), length=dist.res))
    if(normalise)
      tmp <- cbind(tmp$x, tmp$y) else
        tmp <- cbind(tmp$x, tmp$y/max(tmp$y))

    mx[i] <- max(tmp[,2])
    ages[,i] <- tmp[,1]
    probs[,i] <- tmp[,2]
  }
  if(normalise)
    probs <- probs/median(mx)

  if(ka)
    ages <- ages/1e3

  ages <- cbind(ages)
  probs <- cbind(probs)

  if(!add) {
    if(length(age.lab) == 0)
      if(ka) {
        age.lab <- ifelse(BCAD, "k BC/AD", "kcal BP")
      } else
        age.lab <- ifelse(BCAD, "BC/AD", "cal BP")

    if(length(d.lab) == 0)
      d.lab <- "depth"
    if(length(age.lim) == 0)
      age.lim <- extendrange(ages)
    if(!BCAD)
      age.lim <- age.lim[2:1]
    if(length(d.lim) == 0)
      d.lim <- extendrange(depth)
    if(d.rev)
      d.lim <- rev(d.lim)
    if(age.rev)
      age.lim <- rev(age.lim)
    if(rotate.axes)
      plot(0, type="n", ylim=age.lim, ylab=age.lab, xlim=d.lim, xlab=d.lab, ...) else
        plot(0, type="n", xlim=age.lim, xlab=age.lab, ylim=d.lim, ylab=d.lab, ...)
  } else {
      ax.lm <- par("usr")
        if(rotate.axes)
          d.lim <- ax.lm[1:2] else
            d.lim <- ax.lm[3:4]
  }

  # the heights of the distributions scale with the depth axis limits
  if(length(age) > 1)
    ex <- ex * (max(d.lim) - min(d.lim)) / 10
  if(mirror)
    ex <- ex/2
  
  if(length(age) == 1) {
    if(mirror) {
      agepol <- ages[c(1:length(ages), length(ages):1)]
      probpol <- ex * c(probs, -probs[length(probs):1])
    } else
      if(draw.base) {
        agepol <- ages[c(1, 1:length(ages), length(ages))]
        probpol <- ex * c(0, probs, 0)
      } else {
        agepol <- ages
        probpol <- ex * probs # else
      }
      if(rotate.axes)
        polygon(depth+probpol, agepol, col=col, border=border) else
          polygon(agepol, depth+probpol, col=col, border=border)
    } else {
      if(mirror) {
        agepol <- ages[c(1:nrow(ages), nrow(ages):1),]
        probpol <- ex*rbind(probs[1:nrow(probs),], -1*probs[nrow(probs):1,])
      } else {
          agepol <- t(t(ages[c(1,1:nrow(ages), nrow(ages)),]))
          probpol <- ex*rbind(rep(0, ncol(probs)), probs, rep(0, ncol(probs)))
      }
    }  
    if(!up)
      probpol <- -1*probpol

  # now draw the dates
  for(i in 1:length(age)) {
    if(length(age) > 1) # then already drawn before 
      if(draw.base) {
        if(rotate.axes)
          polygon(depth[i]+probpol[,i], agepol[,i], col=col[i], border=border[i]) else
            polygon(agepol[,i], depth[i]+probpol[,i], col=col[i], border=border[i])
    } else
        if(rotate.axes) # Jan 2023
          lines(depth[i]+probpol[,i], agepol[,i], col=col[i]) else
            lines(agepol[,i], depth[i]+probpol[,i], col=col[i])
    if(draw.hpd) {
      if(ka)
        this.hpd <- hpds[[i]][[2]]/1e3 else
          this.hpd <- hpds[[i]][[2]]
      if(rotate.axes)
        segments(depth[i], this.hpd[,1], depth[i], this.hpd[,2], lwd=hpd.lwd, col=hpd.col[i]) else
          segments(this.hpd[,1], depth[i], this.hpd[,2], depth[i], lwd=hpd.lwd, col=hpd.col[i])
    }

    if(length(labels) > 0) {
      xx <- c(min(ages[,i]), max(ages[,i]), mean(ages[,i]))
      if(!BCAD) xx <- xx[c(2,1,3)]
      x <- xx[label.x]
      if(length(label.y) == 0) {
        y <- depth[i]
        if(label.x > 2)
          ifelse(up, y <- y+1, y <- y-1)
      }

    if(rotate.axes)
       text(y+label.offset[1], x+label.offset[2], labels[i], cex=label.cex, col=label.col, adj=label.adj, srt=label.rot) else
         text(x+label.offset[2], y+label.offset[1], labels[i], cex=label.cex, col=label.col, adj=label.adj, srt=label.rot)#)
    }
  }
  invisible(list(ages=ages, probs=probs))
}



#' @name draw.D14C
#' @title Draw d14C and the calibration curve.
#' @description Draw a proxy of the atmospheric 14C concentration (d14C) as well as the calibration curve.
#' @return A plot of d14C and the calibration curve
#' @param cal1 First calendar year for the plot. Defaults to youngest calendar age of the calibration curve
#' @param cal2 Last calendar year for the plot. Defaults to oldest calendar age of the calibration curve
#' @param cc The calibration curve to use. Defaults to IntCal20
#' @param BCAD The calendar scale of graphs and age output-files is in cal BP (calendar or calibrated years before the present, where the present is AD 1950) by default, but can be changed to BC/AD using \code{BCAD=TRUE}.
#' @param mar Plot margins (amount of white space along edges of axes 1-4).
#' @param mgp Axis text margins (where should titles, labels and tick marks be plotted).
#' @param xaxs Whether or not to extend the limits of the horizontal axis. Defaults to \code{xaxs="r"} which extends it by R's default.
#' @param yaxs Whether or not to extend the limits of the vertical axis. Defaults to \code{yaxs="r"} which extends it by R's default.
#' @param bty Draw a box around the graph ("n" for none, and "l", "7", "c", "u", "]" or "o" for correspondingly shaped boxes).
#' @param ka Use kcal BP (and C14 kBP). Defaults to FALSE.
#' @param cal.lab The labels for the calendar axis (default \code{age.lab="cal BP"} or \code{"BC/AD"} if \code{BCAD=TRUE}), or to \code{age.lab="kcal BP"} etc. if ka=TRUE.
#' @param cal.rev Reverse the calendar axis (defaults to FALSE).
#' @param C14.lab Label for the C-14 axis. Defaults to 14C BP (or 14C kBP if ka=TRUE).
#' @param C14.lim Limits for the C-14 axis. Calculated automatically by default.
#' @param cc.col Colour of the calibration curve (fill).
#' @param cc.border Colour of the calibration curve (border).
#' @param D14C.lab Label for the D14C axis.
#' @param D14C.lim Axis limits for the D14C axis. Calculated automatically by default.
#' @param D14C.col Colour of the D14C curve (fill).
#' @param D14C.border Colour of the D14C curve (border).
#' @examples
#'   draw.D14C()
#'   draw.D14C(30e3, 55e3, ka=TRUE)
#'   draw.D14C(cc=ccurve("NH1_monthly"), BCAD=TRUE)
#' @export
draw.D14C <- function(cal1=c(), cal2=c(), cc=ccurve(), BCAD=FALSE, mar=c(4,4,1,4), mgp=c(2.5,1,0), xaxs="r", yaxs="r", bty="u", ka=FALSE, cal.lab=c(), cal.rev=FALSE, C14.lab=c(), C14.lim=c(), cc.col=rgb(0,.5,0,.5), cc.border=rgb(0,.5,0,.5), D14C.lab=c(), D14C.lim=c(), D14C.col=rgb(0,0,1,.5), D14C.border=rgb(0,0,1,.5)) {
  if(BCAD)
    cc[,1] <- 1950 - cc[,1]
  if(length(cal1) == 0)
    cal1 <- min(cc[,1])
  if(length(cal2) == 0)
    cal2 <- max(cc[,1])
  yrmin <- max(1, which(cc[,1] <= min(cal1, cal2)))
  yrmax <- min(nrow(cc), which(cc[,1] >= max(cal1, cal2)))
  cc <- cc[yrmin:yrmax,]
  cc.Fmin <- age.F14C(cc[,2]+cc[,3])
  cc.Fmax <- age.F14C(cc[,2]-cc[,3])
  cc.D14Cmin <- F14C.D14C(cc.Fmin, cc[,1])
  cc.D14Cmax <- F14C.D14C(cc.Fmax, cc[,1])
  op <- par(mar=mar, bty=bty, mgp=mgp, xaxs=xaxs, yaxs=yaxs)
  on.exit(par(op))
  kyr <- ifelse(ka, 1e3, 1)
  cal.lim <- range(cal1, cal2)/kyr
  if(cal.rev)
    cal.lim <- rev(cal.lim)
  if(length(cal.lab) == 0)
    cal.lab <- ifelse(kyr > 1, "kcal BP", "cal BP")
  if(length(D14C.lab) == 0)
    D14C.lab <- expression(paste(Delta, ""^{14}, "C (per-mille)"))
  if(length(C14.lab) == 0)
    C14.lab <- ifelse(kyr > 1, expression(""^14*C~kBP), expression(""^14*C~BP))
  if(length(D14C.lim) == 0)
    D14C.lim <- range(cc.D14Cmin, cc.D14Cmax)
  if(length(C14.lim) == 0)
    C14.lim <- range((cc[,2]-cc[,3]), (cc[,2]+cc[,3]))
  if(ka)
    C14.lim <- C14.lim/1e3

  plot(cc[,1]/kyr, cc.D14Cmax, type="n", xlab=cal.lab, yaxt="n", ylab="", xlim=cal.lim, ylim=D14C.lim)
  pol.D14C <- cbind(c(cc[,1]/kyr, rev(cc[,1]/kyr)), c(cc.D14Cmin, rev(cc.D14Cmax)))
  polygon(pol.D14C, col=D14C.col, border=D14C.border)
  axis(2, col=D14C.border, col.axis=D14C.border)
  mtext(D14C.lab, 2, mgp[1], col=D14C.border)

  op <- par(new=TRUE)
  on.exit(par(op))
  plot(cc[,1]/kyr, (cc[,2]+cc[,3])/kyr, type="n", xaxt="n", yaxt="n", col=4, xlim=cal.lim, xlab="", ylab="", ylim=C14.lim)
  pol.cc <- cbind(c(cc[,1]/kyr, rev(cc[,1]/kyr)), c((cc[,2]+cc[,3])/kyr, rev((cc[,2]-cc[,3])/kyr)))
  polygon(pol.cc, col=cc.col, border=cc.border)

  axis(4, col=cc.border, col.axis=cc.border)
  mtext(C14.lab, 4, mgp[1], col=cc.border)
}

