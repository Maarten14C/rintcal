#' IntCal
#'
#' The international IntCal research group publishes ratified radiocarbon calibration curves such as IntCal20, Marine20 and SHCal20 (Reimer et al. 2020).
#' This data package provides the files of these curves, for use by other R package (reducing the need for replication and the size of other packages that use IntCal).
#' It also comes with a limited number of relevant functions, to read in calibration curves, translate pMC ages to 14C ages (et vice versa), etc. 
#' @docType package
#' @author Maarten Blaauw <maarten.blaauw@qub.ac.uk> 
#' @importFrom utils read.csv read.table write.table packageName
#' @importFrom stats approx
#' @name IntCal
NULL



#' @name copyCalibrationCurve
#' @title Copy a calibration curve.
#' @description Copy one of the the calibration curves into memory.
#' @details Copy the radiocarbon calibration curve defined by cc into memory.
#' @return The calibration curve (invisible).
#' @param cc Calibration curve for 14C dates: \code{cc=1} for IntCal20 (northern hemisphere terrestrial), \code{cc=2} for Marine20 (marine),
#' \code{cc=3} for SHCal20 (southern hemisphere terrestrial).
#' @param postbomb Use \code{postbomb=TRUE} to get a postbomb calibration curve (default \code{postbbomb=FALSE}).
#' @examples
#' intcal20 <- copyCalibrationCurve(1)
#' @export
copyCalibrationCurve <- function(cc=1, postbomb=FALSE) {
  if(postbomb) {
    if(cc==1) fl <- "postbomb_NH1.14C" else
      if(cc==2) fl <- "postbomb_NH2.14C" else
        if(cc==3) fl <- "postbomb_NH3.14C" else
          if(cc==4) fl <- "postbomb_SH1-2.14C" else
            if(cc==5) fl <- "postbomb_SH3.14C" else
              stop("calibration curve doesn't exist\n", call.=FALSE)
  } else
  if(cc==1) fl <- "3Col_intcal20.14C" else
    if(cc==2) fl <- "3Col_marine20.14C" else
      if(cc==3) fl <- "3Col_shcal20.14C" else
        stop("calibration curve doesn't exist\n", call.=FALSE)
  cc <- system.file("extdata/", fl, package='IntCal')
  cc <- read.table(cc)
  invisible(cc)
}



#' @name mix.curves
#' @title Build a custom-made, mixed calibration curve.
#' @description If two curves need to be `mixed' to calibrate, e.g. for dates of mixed terrestrial and marine carbon sources, then this function can be used.
#' @details The proportional contribution of each of both calibration curves has to be set.
#'
#' @param proportion Proportion of the first calibration curve required. e.g., change to \code{proportion=0.7} if \code{cc1} should contribute 70\% (and \code{cc2} 30\%) to the mixed curve.
#' @param cc1 The first calibration curve to be mixed. Defaults to the northern hemisphere terrestrial curve IntCal20.
#' @param cc2 The second calibration curve to be mixed. Defaults to the marine curve IntCal20.
#' @param name Name of the new calibration curve.
#' @param dirname Directory where the file will be written. If using the default \code{dirname="."},
#' the new curve will be saved in current working directory.
#' @param offset Any offset and error to be applied to \code{cc2} (default 0 +- 0).
#' @param sep Separator between fields (tab by default, "\\t")
#' @return A file containing the custom-made calibration curve, based on calibration curves \code{cc1} and \code{cc2}.
#' @examples
#' mix.curves(, dirname=tempdir())
#' @export
mix.curves <- function(proportion=.5, cc1="3Col_intcal20.14C", cc2="3Col_marine20.14C", name="mixed.14C", dirname=".", offset=c(0,0), sep="\t") {
  ccloc <- system.file("extdata/", package='IntCal')
#  dirname <- .validateDirectoryName(dirname)

  cc1 <- read.table(paste0(ccloc, "/", cc1))
  cc2 <- read.table(paste0(ccloc, "/", cc2))
  cc2.mu <- approx(cc2[,1], cc2[,2], cc1[,1], rule=2)$y + offset[1] # interpolate cc2 to the calendar years of cc1
  cc2.error <- approx(cc2[,1], cc2[,3], cc1[,1], rule=2)$y
  cc2.error <- sqrt(cc2.error^2 + offset[2]^2)
  mu <- proportion * cc1[,2] + (1-proportion) * cc2.mu
  error <- proportion * cc1[,3] + (1-proportion) * cc2.error
  write.table(cbind(cc1[,1], mu, error), paste0(dirname, "/", name), row.names=FALSE, col.names=FALSE, sep=sep)
}



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


#' @name intcal.data
#' @title plot the IntCal20 data
#' @description plot the C14 ages underpinning the IntCal20/Marine20/SHCal20 calibration curves
#' @details These datasets were downloaded from Intcal.org. All data have both uncertainties in C14 age and on the calendar scale. For trees this is the sample thickness (e.g., 10 years or 1 year).
#' @param calmin Minimum calendar year for the plot
#' @param calmax Maximum calendar year for the plot
#' @param cc1 Name of the calibration curve. Can be "IntCal20", "Marine20", "SHCal20", or for the previous curves "IntCal13", "Marine13" or "SHCal13".
#' @param cc2. Optional second calibration curve to plot. Can be "IntCal20", "Marine20", "SHCal20", or for the previous curves "IntCal13", "Marine13" or "SHCal13". Defaults to nothing, NA. 
#' @param BCAD The calendar scale of graphs and age output-files is in cal BP (calendar or calibrated years before the present, where the present is AD 1950) by default, but can be changed to BC/AD using \code{BCAD=TRUE}.
#' @param cal.lab The labels for the calendar axis (default \code{age.lab="cal BP"} or \code{"BC/AD"} if \code{BCAD=TRUE}), or to \code{age.lab="kcal BP"} etc. if ka=TRUE.
#' @param cal.rev Reverse the calendar axis. 
#' @param c14.lab Label for the C-14 axis. Defaults to 14C BP (or 14C kBP if ka=TRUE).
#' @param c14.lim Axis limits for the C-14 axis. Calculated automatically by default. 
#' @param c14.rev Reverse the C-14 axis.
#' @param ka Use kcal BP (and C14 kBP).
#' @param cc1.col Colour of the calibration curve (outline).
#' @param cc1.fill Colour of the calibration curve (fill).
#' @param cc2.col Colour of the calibration curve (outline), if activated (default cc2=NA).
#' @param cc2.fill Colour of the calibration curve (fill), if activated (default cc2=NA).
#' @param data.cols colours of the data points. Defaults to R's colours 1 to 8 (black, red, green, darkblue, lightblue, purple, orange, and grey)
#' @param data.pch Symbols of the data points. Defaults to R's symbols 1, 2, 5, 6, and 15 to 19 (open circle, open upward triangle, open diamond, open downward triangle, closed square, closed circle, closed upward triangle, closed diamond)
#' @param pch.cex Size of the data symbols. Defaults to 0.5. 
#' @param legend.loc Location of the data legend. Defaults to topleft. Set to NA for no plotting.
#' @param legend.ncol Number of columns of the data legend.
#' @param legend.cex Size of the legend. Defaults to 0.7.
#' @param cc.legend Location of the legend for the calibration curve(s).
#' @param bty Box type around the plot. Defaults to "l"-shaped. 
#' @param ... Any additional optional plotting parameters. 
#' @examples
#'   intcal.data(100, 200)
#'   intcal.data(40e3, 55e3, ka=TRUE)
#' @export
intcal.data <- function(calmin, calmax, cc1="IntCal20", cc2=NA, BCAD=FALSE, cal.lab=NA, cal.rev=FALSE, c14.lab=NA, c14.lim=NA, c14.rev=FALSE, ka=FALSE, cc1.col=rgb(0,0,1,.5), cc1.fill=rgb(0,0,1,.2), cc2.col=rgb(0,.5,0,.5), cc2.fill=rgb(0,.5,0,.2), data.cols=1:8, data.pch=c(1,2,5,6,15:19), pch.cex=.5, legend.loc="topleft", legend.ncol=2, legend.cex=0.7, cc.legend="bottomright", bty="l",  ...) {

  # read the data. For now this is IntCal20 data only. SHCal20 has other datasets
  dat <- system.file("extdata/", "intcal20_data.txt", package = "IntCal")
  dat <- read.table(dat, header=TRUE, sep=" ")
  sourcesdat <- system.file("extdata/", "intcal20_data_sources.txt", package = "IntCal")
  sourcesdat <- read.table(sourcesdat, nrows=32, sep=",")

  # find the data corresponding to the period of interest
  mindat <- dat$cal >= calmin
  maxdat <- dat$cal <= calmax
  dat <- dat[which( mindat * maxdat == 1),]

  # read and narrow down the calibration curve(s)
  if(tolower(cc1) == "intcal20")
    cc.1 <- "3Col_intcal20.14C" else
    if(tolower(cc1) == "marine20")
      cc.1 <- "3Col_marine20.14C" else
      if(tolower(cc1) == "shcal20")
        cc.1 <- "3Col_shcal20.14C" else
        if(tolower(cc1) == "intcal13")
          cc.1 <- "3Col_intcal13.14C" else
          if(tolower(cc1) == "marine13")
            cc.1 <- "3Col_marine13.14C" else
            if(tolower(cc1) == "shcal13")
              cc.1 <- "3Col_shcal13.14C" else
                message("I don't know which curve you mean")
  cc.1 <- system.file("extdata/", cc.1, package = "IntCal")
  cc.1 <- read.table(cc.1, header=FALSE)
  mindat <- cc.1[,1] >= calmin
  maxdat <- cc.1[,1] <= calmax
  cc.1 <- cc.1[which(mindat * maxdat == 1),]
  if(ka)
    cc.1 <- cc.1/1e3
  cc1.pol <- cbind(c(cc.1[,1], rev(cc.1[,1])), c(cc.1[,2]-cc.1[,3], rev(cc.1[,2]+cc.1[,3])))
  
  if(!is.na(cc2)) {
    if(tolower(cc2) == "intcal20")
      cc2. <- "3Col_intcal20.14C" else
      if(tolower(cc2) == "marine20")
        cc.2 <- "3Col_marine20.14C" else
        if(tolower(cc2) == "shcal20")
          cc.2 <- "3Col_shcal20.14C" else
          if(tolower(cc2) == "intcal13")
            cc.2 <- "3Col_intcal13.14C" else
            if(tolower(cc2) == "marine13")
              cc.2 <- "3Col_marine13.14C" else
              if(tolower(cc2) == "shcal13")
                cc.2 <- "3Col_shcal13.14C" else
                  message("I don't know which curve you mean")
    cc.2 <- system.file("extdata/", cc.2, package = "IntCal")
    cc.2 <- read.table(cc.2, header=FALSE)
    mindat <- cc.2[,1] >= calmin
    maxdat <- cc.2[,1] <= calmax
    cc.2 <- cc.2[which(mindat * maxdat == 1),]
    if(ka)
      cc.2 <- cc.2/1e3
    cc2.pol <- cbind(c(cc.2[,1], rev(cc.2[,1])), c(cc.2[,2]-cc.2[,3], rev(cc.2[,2]+cc.2[,3])))
  }

  # different datasets need different colours and symbols
  sets <- dat$set
  set.cols <- NA; set.pchs <- NA
  these.sets <- sort(unique(sets))
  these.sets <<- these.sets
  these.cols <- data.cols[1:length(sets)]
  these.pchs <- data.pch[1:length(sets)]
  for(i in 1:length(these.sets)) {
    set.cols[sets %in% these.sets[i]] <- these.cols[i]
    set.pchs[sets %in% these.sets[i]] <- these.pchs[i]
  }

  # set up the plot parameters
  cal <- dat$cal
  cal.err <- dat$calsig
  cal.err[which(cal.err == 1)] <- 0 # do not plot yearly errors
  cal.err[which(cal.err <= 10)] <- cal.err[which(cal.err <= 10)]/2 # decadal wood slices
  # needs to identify datasets with wood, based on set number, not on error size
  c14 <- dat$c14
  c14.err <- dat$c14sig

  if(is.na(cal.lab))
    callab <- "cal. yr BP"
  if(is.na(c14.lab))
    c14lab <- expression(""^14*C~BP)

  cal.lim <- c(calmin, calmax)
  if(cal.rev)
    cal.lim <- rev(cal.lim)

  if(BCAD) {
    cal <- 1950-cal
    if(is.na(cal.lab))
      callab <- "BC/AD"
  }

  if(ka) {
    cal <- cal/1e3
    cal.err <- cal.err/1e3
    c14 <- c14/1e3
    c14.err <- c14.err/1e3
    if(is.na(c14.lab))
      c14lab <- expression(""^14*C~kBP)
    if(is.na(cal.lab))
      callab <- ifelse(BCAD, "kcal BC/AD", "kcal BP")
    cal.lim <- cal.lim/1e3
  }

  if(is.na(c14.lim))
    c14.lim <- range(c14-c14.err, c14+c14.err, cc1.pol[,2])
  if(c14.rev)
    c14.lim <- rev(c14.lim)

  # draw the graph and data
  plot(0, type="n", xlim=cal.lim, xlab=callab, ylim=c14.lim, ylab=c14lab, bty=bty, ...)
  points(cal, c14, col=set.cols, pch=set.pchs, cex=pch.cex) # data points
  segments(cal, c14-c14.err, cal, c14+c14.err, col=set.cols) # c14 errors
  segments(cal-cal.err, c14, cal+cal.err, c14, col=set.cols) # cal errors

  # add the calibration curve(s)
  polygon(cc1.pol, col=cc1.fill, border=NA) # calibration curve
  lines(cc.1[,1], cc.1[,2]-cc.1[,3], col=cc1.col)
  lines(cc.1[,1], cc.1[,2]+cc.1[,3], col=cc1.col)
  if(!is.na(cc2)) {
    polygon(cc2.pol, col=cc2.fill, border=NA) # calibration curve
    lines(cc.2[,1], cc.2[,2]-cc.2[,3], col=cc2.col)
    lines(cc.2[,1], cc.2[,2]+cc.2[,3], col=cc2.col)	
  }

  # legend
  if(!is.na(legend.loc)) {
    set <- sourcesdat[which(sourcesdat[,1] %in% these.sets),2]
    legend(legend.loc, set, col=these.cols, pch=these.pchs, cex=legend.cex, ncol=legend.ncol, text.col=these.cols, bty="n")
  }
  
  if(!is.na(cc.legend)) {
    if(is.na(cc2)) {
      tt <- cc1 
      cc.col <- cc1.col
    } else {
        tt <- c(cc1, cc2)
        cc.col <- c(cc1.col, cc2.col)
      }
    legend(cc.legend, legend=tt, text.col=cc.col, cex=legend.cex, bty="n")
  }
  
}

