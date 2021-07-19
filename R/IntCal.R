#' IntCal
#'
#' The international IntCal research group publishes ratified radiocarbon calibration curves such as IntCal20, Marine20 and SHCal20 (Reimer et al. 2020).
#' This data package provides the files of these curves, for use by other R package (reducing the need for replication and the size of other packages that use IntCal).
#' It also comes with a limited number of relevant functions, to read in calibration curves, translate pMC ages to 14C ages (et vice versa), etc. 
#' @docType package
#' @author Maarten Blaauw <maarten.blaauw@qub.ac.uk> 
#' @importFrom utils read.csv read.table write.table packageName
#' @importFrom stats approx dnorm
#' @importFrom grDevices rgb
#' @importFrom graphics axis par legend lines points polygon segments
#' @name IntCal
NULL

# todo: function to draw multiple calibrated dates?


#' @name list.ccurves
#' @title List the calibration curves.
#' @description List the file names of the calibration curves available within the IntCal package.
#' @export
list.ccurves <- function() {
  cc <- system.file("extdata/", package='IntCal')
  message(cc)
  list.files(cc)
}



#' @name copyCalibrationCurve
#' @title Copy a calibration curve
#' @description Copy one of the calibration curves into memory. Renamed to ccurve, and copyCalibrationCurve will become obsolete
#' @details Copy the radiocarbon calibration curve defined by cc into memory.
#' @return The calibration curve (invisible).
#' @param cc Calibration curve for 14C dates: \code{cc=1} for IntCal20 (northern hemisphere terrestrial), \code{cc=2} for Marine20 (marine),
#' \code{cc=3} for SHCal20 (southern hemisphere terrestrial). Alternatively, one can also write, e.g., "IntCal20", "Marine13".
#' @param postbomb Use \code{postbomb=TRUE} to get a postbomb calibration curve (default \code{postbomb=FALSE}).
#' @export
copyCalibrationCurve <- function(cc=1, postbomb=FALSE) {
  message("This function will be removed in future versions, as it has been renamed to ccurve.")
  ccurve(cc, postbomb)
}


#' @name ccurve
#' @title Copy a calibration curve
#' @description Copy one of the calibration curves into memory.
#' @details Copy the radiocarbon calibration curve defined by cc into memory.
#' @return The calibration curve (invisible).
#' @param cc Calibration curve for 14C dates: \code{cc=1} for IntCal20 (northern hemisphere terrestrial), \code{cc=2} for Marine20 (marine),
#' \code{cc=3} for SHCal20 (southern hemisphere terrestrial). Alternatively, one can also write, e.g., "IntCal20", "Marine13".
#' @param postbomb Use \code{postbomb=TRUE} to get a postbomb calibration curve (default \code{postbomb=FALSE}).
#' @examples
#' intcal20 <- ccurve(1)
#' marine20 <- ccurve(2)
#' shcal20 <- ccurve(3)
#' marine98 <- ccurve("Marine98")
#' pb.sh3 <- ccurve("sh3")
#' @references
#' Hammer and Levin 2017, "Monthly mean atmospheric D14CO2 at Jungfraujoch and Schauinsland from 1986 to 2016", heiDATA: Heidelberg Research Data Repository V2 \doi{10.11588/data/10100} 
#'
#' Hogg et al. 2013 SHCal13 Southern Hemisphere Calibration, 0–50,000 Years cal BP. Radiocarbon 55, 1889-1903. \doi{10.2458/azu_js_rc.55.16783}
#'
#' Hogg et al. 2020 SHCal20 Southern Hemisphere calibration, 0-55,000 years cal BP. Radiocarbon 62. \doi{10.1017/RDC.2020.59}
#'
#' Hua et al. 2013 Atmospheric radiocarbon for the period 1950-2010. Radiocarbon 55(4), \doi{10.2458/azu_js_rc.v55i2.16177}
#'
#' Hughen et al. 2020 Marine20-the marine radiocarbon age calibration curve (0-55,000 cal BP). Radiocarbon 62. \doi{10.1017/RDC.2020.68}
#'
#' Levin and Kromer 2004 "The tropospheric 14CO2 level in mid latitudes of the Northern Hemisphere" Radiocarbon 46, 1261-1272
#'
#' Reimer et al. 2004 IntCal04 terrestrial radiocarbon age calibration, 0–26 cal kyr BP. Radiocarbon 46, 1029–1058. \doi{10.1017/S0033822200032999}
#'
#' Reimer et al. 2009 IntCal09 and Marine09 radiocarbon age calibration curves, 0–50,000 years cal BP. Radiocarbon 51, 1111–1150. \doi{10.1017/S0033822200034202}
#'
#' Reimer et al. 2013 IntCal13 and Marine13 radiocarbon age calibration curves 0–50,000 years cal BP. Radiocarbon 55, 1869–1887. \doi{10.2458/azu_js_rc.55.16947}
#'
#' Reimer et al. 2020 The IntCal20 Northern Hemisphere radiocarbon age calibration curve (0–55 cal kBP). Radiocarbon 62, 725-757. \doi{10.1017/RDC.2020.41}
#'
#' Stuiver et al. 1998 INTCAL98 radiocarbon age calibration, 24,000–0 cal BP. Radiocarbon 40, 1041-1083. \doi{10.1017/S0033822200019123}
#' @export
ccurve <- function(cc=1, postbomb=FALSE) {
  if(postbomb) {
    if(cc==1 || tolower(cc) == "nh1")
      fl <- "postbomb_NH1.14C" else
      if(cc==2 || tolower(cc) == "nh2")
        fl <- "postbomb_NH2.14C" else
        if(cc==3 || tolower(cc) == "nh3")
          fl <- "postbomb_NH3.14C" else
          if(cc==4 || tolower(cc) == "sh1-2")
            fl <- "postbomb_SH1-2.14C" else
            if(cc==5 || tolower(cc) == "sh3")
              fl <- "postbomb_SH3.14C" else
              if(tolower(cc) == "kure")
                fl <- "Kure.14C" else
                if(tolower(cc) == "levinkromer")
                  fl <- "LevinKromer.14C" else
                  if(tolower(cc) == "santos")
                    fl <- "Santos.14C" else
                      stop("cannot find this postbomb curve\n", call.=FALSE)
  } else
  if(cc==1 || tolower(cc) == "intcal20")
    fl <- "3Col_intcal20.14C" else
    if(cc==2 || tolower(cc) == "marine20")
      fl <- "3Col_marine20.14C" else
      if(cc==3 || tolower(cc) == "shcal20")
        fl <- "3Col_shcal20.14C" else
        if(tolower(cc) == "intcal13")
          fl <- "3Col_intcal13.14C" else
          if(tolower(cc) == "marine13")
            fl <- "3Col_marine13.14C" else
            if(tolower(cc) == "shcal13")
              fl <- "3Col_shcal13.14C" else
              if(tolower(cc) == "intcal09")
                fl <- "3Col_intcal09.14C" else
                if(tolower(cc) == "marine09")
                  fl <- "3Col_marine09.14C" else
                  if(tolower(cc) == "intcal04")
                    fl <- "3Col_intcal04.14C" else
                    if(tolower(cc) == "marine04")
                      fl <- "3Col_marine04.14C" else
                      if(tolower(cc) == "intcal98")
                        fl <- "3Col_intcal98.14C" else
                        if(tolower(cc) == "marine98")
                          fl <- "3Col_marine98.14C" else  
                          if(tolower(cc) == "nh1")
                            fl <- "postbomb_NH1.14C" else
                            if(tolower(cc) == "nh2")
                              fl <- "postbomb_NH2.14C" else
                              if(tolower(cc) == "nh3")
                                fl <- "postbomb_NH3.14C" else
                                if(tolower(cc) == "sh1-2")
                                  fl <- "postbomb_SH1-2.14C" else
                                  if(tolower(cc) == "sh3")
                                    fl <- "postbomb_SH3.14C" else
                                    if(tolower(cc) == "kure")
                                      fl <- "kure.14C" else
                                      if(tolower(cc) == "levinkromer")
                                        fl <- "LevinKromer.14C" else
                                        if(tolower(cc) == "santos")
                                          fl <- "Santos.14C" else
                                          if(tolower(cc) == "mixed")
                                            fl <- "mixed.14C" else
                                            stop("cannot find this curve", call.=FALSE)
  cc <- system.file("extdata/", fl, package='IntCal')
  cc <- read.table(cc)
  invisible(cc)
}



#' @name draw.ccurve
#' @title Draw a calibration curve.
#' @description Draw one or two of the calibration curves, or add a calibration curve to an existing plot.
#' @return The calibration curve (invisible).
#' @param cal1 First calendar year for the plot
#' @param cal2 Last calendar year for the plot
#' @param cc1 Name of the calibration curve. Can be "IntCal20", "Marine20", "SHCal20", or for the previous curves "IntCal13", "Marine13" or "SHCal13". Can also be "nh1", "nh2", "nh3", "sh1-2", "sh3", "Kure", "LevinKromer" or "Santos" for postbomb curves.
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
#' @param ... Any additional optional plotting parameters. 
#' @examples 
#' draw.ccurve()
#' draw.ccurve(1000, 3000, cc2="Marine20")
#' draw.ccurve(1800, 2020, BCAD=TRUE, cc2="nh1", cc2.postbomb=TRUE)
#' draw.ccurve(1800, 2010, BCAD=TRUE, cc2="nh1", add.yaxis=TRUE)
#' @export
draw.ccurve <- function(cal1=-50, cal2=55e3, cc1="IntCal20", cc2=NA, cc1.postbomb=FALSE, cc2.postbomb=FALSE,  BCAD=FALSE, cal.lab=NA, cal.rev=FALSE, c14.lab=NA, c14.lim=NA, c14.rev=FALSE, ka=FALSE, add.yaxis=FALSE, cc1.col=rgb(0,0,1,.5), cc1.fill=rgb(0,0,1,.2), cc2.col=rgb(0,.5,0,.5), cc2.fill=rgb(0,.5,0,.2), add=FALSE, bty="l", ...) {

  # read and narrow down the calibration curve(s)
  cc.1 <- ccurve(cc1, cc1.postbomb)
  if(BCAD)
    cc.1[,1] <- 1950 - cc.1[,1] 
  mindat <- cc.1[,1] >= min(cal1, cal2)
  maxdat <- cc.1[,1] <= max(cal1, cal2)
  cc.1 <- cc.1[which(mindat * maxdat == 1),]
  if(ka)
    cc.1 <- cc.1/1e3
  cc1.pol <- cbind(c(cc.1[,1], rev(cc.1[,1])), c(cc.1[,2]-cc.1[,3], rev(cc.1[,2]+cc.1[,3])))
  
  if(!is.na(cc2)) {
    cc.2 <- ccurve(cc2, cc2.postbomb)
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
      callab <- "cal. yr BP"
    if(is.na(c14.lab))
      c14lab <- expression(""^14*C~BP)

    cal.lim <- c(cal1, cal2)
    if(cal.rev)
      cal.lim <- rev(cal.lim)

    if(BCAD)
      if(is.na(cal.lab))
        callab <- "BC/AD"

    if(ka) {
      if(is.na(c14.lab))
        c14lab <- expression(""^14*C~kBP)
      if(is.na(cal.lab))
        callab <- ifelse(BCAD, "kcal BC/AD", "kcal BP")
      cal.lim <- cal.lim/1e3
    }

    if(is.na(c14.lim))
      if(is.na(cc2) || add.yaxis)
        c14.lim <- range(cc1.pol[,2]) else
          c14.lim <- range(cc1.pol[,2], cc2.pol[,2])
    if(c14.rev)
      c14.lim <- rev(c14.lim)

    # draw the graph and data
    plot(0, type="n", xlim=cal.lim, xlab=callab, ylim=c14.lim, ylab=c14lab, bty=bty, ...)
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




#' @name mix.curves
#' @title Build a custom-made, mixed calibration curve.
#' @description If two curves need to be `mixed' to calibrate, e.g. for dates of mixed terrestrial and marine carbon sources, then this function can be used. The curve will be saved together with the calibration curves. 
#' @details The proportional contribution of each of both calibration curves has to be set.
#'
#' @param proportion Proportion of the first calibration curve required. e.g., change to \code{proportion=0.7} if \code{cc1} should contribute 70\% (and \code{cc2} 30\%) to the mixed curve.
#' @param cc1 The first calibration curve to be mixed. Defaults to the northern hemisphere terrestrial curve IntCal20.
#' @param cc2 The second calibration curve to be mixed. Defaults to the marine curve IntCal20.
#' @param name Name of the new calibration curve.
#' @param offset Any offset and error to be applied to \code{cc2} (default 0 +- 0).
#' @param sep Separator between fields (tab by default, "\\t")
#' @return A file containing the custom-made calibration curve, based on calibration curves \code{cc1} and \code{cc2}.
#' @examples
#' mix.curves(, dirname=tempdir())
#' @export
mix.curves <- function(proportion=.5, cc1="IntCal20", cc2="Marine20", name="mixed.14C", offset=c(0,0), sep="\t") {
  ccloc <- system.file("extdata/", package='IntCal')
#  dirname <- .validateDirectoryName(dirname)

  cc1 <- ccurve(cc1)
  cc2 <- ccurve(cc2)
  cc2.mu <- approx(cc2[,1], cc2[,2], cc1[,1], rule=2)$y + offset[1] # interpolate cc2 to the calendar years of cc1
  cc2.error <- approx(cc2[,1], cc2[,3], cc1[,1], rule=2)$y
  cc2.error <- sqrt(cc2.error^2 + offset[2]^2)
  mu <- proportion * cc1[,2] + (1-proportion) * cc2.mu
  error <- proportion * cc1[,3] + (1-proportion) * cc2.error
  write.table(cbind(cc1[,1], mu, error), paste0(ccloc, name), row.names=FALSE, col.names=FALSE, sep=sep)
}



