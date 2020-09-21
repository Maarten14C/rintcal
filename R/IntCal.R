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

