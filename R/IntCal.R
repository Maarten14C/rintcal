#' IntCal
#'
#' The international IntCal research group publishes ratified radiocarbon calibration curves such as IntCal20, Marine20 and SHCal20 (Reimer et al. 2020).
#' This data package provides the files of these curves, for use by other R package (reducing the need for replication and the size of other packages that use IntCal).
#' It also comes with a limited number of relevant functions, to read in calibration curves, translate pMC ages to 14C ages (et vice versa), etc. 
#' @docType package
#' @author Maarten Blaauw <maarten.blaauw@qub.ac.uk>
#' @importFrom utils read.csv read.table write.table packageName
#' @importFrom stats approx dnorm
#' @importFrom grDevices rgb extendrange
#' @importFrom graphics axis par legend lines points polygon segments text
#' @name IntCal
NULL

# todo: add another caldist function a la l.calib of coffee for single cal yrs but multiple dates? hpds sometimes are >100%??? prepare calib function with MCMC ccurve.

# done: 


#' @name list.ccurves
#' @title List the calibration curves
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



#' @name new.ccdir
#' @title Make directory and fill with calibration curves
#' @description Make an alternative `curves' directory and fill it with the calibration curves.
#' @details Copies all calibration curves within the `IntCal' package to the new directory.
#' @param ccdir Name and location of the new directory. Defaults to `ccurves', a folder within the current working directory, \code{ccdir="./ccurves"}.
#' @examples
#' new.ccdir(tempdir())
#' @export
new.ccdir <- function(ccdir="./ccurves") {
  if(!dir.exists(ccdir))
    dir.create(ccdir)

  # find all calibration curves (files ending in .14C) and copy them into the new directory
  fl <- list.files(file.path(system.file(package = 'IntCal'), "extdata"), full.names=TRUE, pattern=".14C")
  file.copy(fl, ccdir)
  message("Calibration curves placed in folder ", ccdir)
}



#' @name ccurve
#' @title Copy a calibration curve
#' @description Copy one of the calibration curves into memory.
#' @details Copy the radiocarbon calibration curve defined by cc into memory.
#' @return The calibration curve (invisible).
#' @param cc Calibration curve for 14C dates: \code{cc=1} for IntCal20 (northern hemisphere terrestrial), \code{cc=2} for Marine20 (marine),
#' \code{cc=3} for SHCal20 (southern hemisphere terrestrial). Alternatively, one can also write, e.g., "IntCal20", "Marine13".
#' @param postbomb Use \code{postbomb=TRUE} to get a postbomb calibration curve (default \code{postbomb=FALSE}). For monthly data, type e.g. \code{ccurve("sh1-2_monthly")}
#' @param ccdir Directory of the calibration curves. Defaults to where the package's files are stored (system.file), but can be set to, e.g., \code{ccdir="curves"}.
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
#' Hua et al. 2021 Atmospheric radiocarbon for the period 1950-2019. Radiocarbon in press,  \doi{10.1017/RDC.2021.95}
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
ccurve <- function(cc=1, postbomb=FALSE, ccdir=NULL) {
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
                                       if(tolower(cc) == "nh1_monthly")
                                        fl <- "postbomb_NH1_monthly.14C" else
                                        if(tolower(cc) == "nh2_monthly")
                                          fl <- "postbomb_NH2_monthly.14C" else
                                          if(tolower(cc) == "nh3_monthly")
                                            fl <- "postbomb_NH3_monthly.14C" else
                                            if(tolower(cc) == "sh1-2_monthly")
                                              fl <- "postbomb_SH1-2_monthly.14C" else
                                              if(tolower(cc) == "sh3_monthly")
                                                fl <- "postbomb_SH3_monthly.14C" else
                                                if(tolower(cc) == "kure")
                                                  fl <- "kure.14C" else
                                                  if(tolower(cc) == "levinkromer")
                                                    fl <- "LevinKromer.14C" else
                                                    if(tolower(cc) == "santos")
                                                      fl <- "Santos.14C" else
                                                      if(tolower(cc) == "mixed")
                                                        fl <- "mixed.14C" else
                                                        stop("cannot find this curve", call.=FALSE)
  if(length(ccdir) == 0)
    cc <- system.file("extdata/", fl, package='IntCal') else
      cc <- file.path(ccdir, fl)
  cc <- read.table(cc)
  invisible(cc)
}



#' @name mix.ccurves
#' @title Build a custom-made, mixed calibration curve.
#' @description If two curves need to be `mixed' to calibrate, e.g. for dates of mixed terrestrial and marine carbon sources, then this function can be used. The curve will be saved, together with the main calibration curves, in a temporary directory. This temporary directory then has to be specified in further commands, e.g. for rbacon: \code{Bacon(, ccdir=tmpdr)} (see examples). It is advisable to make your own curves folder and have ccdir point to that folder. 
#' @details The proportional contribution of each of both calibration curves has to be set.
#'
#' @param proportion Proportion of the first calibration curve required. e.g., change to \code{proportion=0.7} if \code{cc1} should contribute 70\% (and \code{cc2} 30\%) to the mixed curve.
#' @param cc1 The first calibration curve to be mixed. Defaults to the northern hemisphere terrestrial curve IntCal20.
#' @param cc2 The second calibration curve to be mixed. Defaults to the marine curve IntCal20.
#' @param name Name of the new calibration curve.
#' @param dir Name of the directory where to save the file. Since R does not allow automatic saving of files, this points to a temporary directory by default. Adapt to your own folder, e.g., \code{dir="~/Curves"} or in your current working directory, \code{dir="."}.
#' @param offset Any offset and error to be applied to \code{cc2} (default 0 +- 0).
#' @param sep Separator between fields (tab by default, "\\t")
#' @return A file containing the custom-made calibration curve, based on calibration curves \code{cc1} and \code{cc2}.
#' @examples
#' mix.ccurves()
#' tmpdir <- tempdir()
#' mix.ccurves(dir=tmpdir)
#' # clean up:
#' unlink(tmpdir)
#' @export
mix.ccurves <- function(proportion=.5, cc1="IntCal20", cc2="Marine20", name="mixed.14C", dir=c(), offset=c(0,0), sep="\t") {
  # place the IntCal curves within the same folder as the new curve:
  if(length(dir) == 0)
     dir <- tempdir()   
  curves <- list.files(system.file("extdata", package='IntCal'), pattern=".14C", full.names=T)
  file.copy(curves, dir)

  cc1 <- ccurve(cc1)
  cc2 <- ccurve(cc2)
  cc2.mu <- approx(cc2[,1], cc2[,2], cc1[,1], rule=2)$y + offset[1] # interpolate cc2 to the calendar years of cc1
  cc2.error <- approx(cc2[,1], cc2[,3], cc1[,1], rule=2)$y
  cc2.error <- sqrt(cc2.error^2 + offset[2]^2)
  mu <- proportion * cc1[,2] + (1-proportion) * cc2.mu
  error <- proportion * cc1[,3] + (1-proportion) * cc2.error

  write.table(cbind(cc1[,1], mu, error), file.path(dir, name), row.names=FALSE, col.names=FALSE, sep=sep)
  message(name, " saved in folder ", dir)
}



#' @name glue.ccurves
#' @title Glue prebomb and postbomb curves
#' @description Produce a custom curve by merging two calibration curves, e.g. a prebomb and a postbomb one for dates which straddle both curves.
#' @return The custom-made curve (invisibly)
#' @param prebomb The prebomb curve. Defaults to "IntCal20"
#' @param postbomb The postbomb curve. Defaults to "NH1" (Hua et al. 2013)
#' @examples
#' my.cc <- glue.ccurves()
#' @export
glue.ccurves <- function(prebomb="IntCal20", postbomb="NH1") {
  glued <- rbind(ccurve(prebomb, FALSE), ccurve(postbomb, TRUE))
  glued <- glued[order(glued[,1]),]
  repeated <- which(diff(glued[,1]) == 0)
  if(length(repeated) > 0)
    invisible(glued[-repeated,]) else # remove any repeated years
      invisible(glued[order(glued[,1]),])
}
