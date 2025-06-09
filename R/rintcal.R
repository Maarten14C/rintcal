# todo: write more detail as to what can be found in the intcal.data.frames

# during package development, the data/intcal.rda file was written as such:
# intcal <- rintcal::intcal.read.data(TRUE) # download from the server
# save(intcal, file="~/Dropbox/devsoftware/rintcal/data/intcal.rda")
# tools::resaveRdaFiles("~/Dropbox/devsoftware/rintcal/data/intcal.rda") # to compress
# some platforms complain about non-ASCII characters in the datafilestringi::stri_enc_mark

# stringi::stri_enc_mark(intcal)
# [1] "ASCII"  "native" "ASCII"  "ASCII"  "native" "ASCII" 
# stringi::stri_enc_mark(intcal[[2]]) shows which entries contain non-ASCII chars



# internal functions to speed up reading and writing files, using the data.table R package if present
fastread <- function(fl, skip=0, sep=" ", ...)
  if("data.frame" %in% (.packages())) # some Macs have problems with this package
    as.data.frame(data.table::fread(fl, skip=skip, sep=sep), ...) else
      read.table(fl, skip=skip, sep=sep, ...)



fastwrite <- function(fl, ...)
  if("data.frame" %in% (.packages())) # some Macs have problems with this package
    data.table::fwrite(as.data.frame(fl), ...) else
      write.table(fl, ...)



#' @name list.ccurves
#' @title List the calibration curves
#' @description List the file names of the calibration curves available within the rintcal package.
#' @return A list of the available calibration curves
#' @export
list.ccurves <- function() {
  cc <- system.file("extdata/", package='rintcal')
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
#' @details Copies all calibration curves within the `rintcal' package to the new directory.
#' @param cc.dir Name and location of the new directory. For example, this could be a folder called 'ccurves', living within the current working directory, \code{cc.dir="./ccurves"}.
#' @return A message informing the user the name of the folder into which the calibration curves have been copied.
#' @examples
#' new.ccdir(tempdir())
#' @export
new.ccdir <- function(cc.dir) {
  if(!dir.exists(cc.dir))
    dir.create(cc.dir)

  # find all calibration curves (files ending in .14C) and copy them into the new directory
  fl <- list.files(file.path(system.file(package = 'rintcal'), "extdata"), full.names=TRUE, pattern="\\.14[Cc]$")
  file.copy(fl, cc.dir)
  message("Calibration curves placed in folder ", cc.dir)
}



#' @name ccurve
#' @title Copy a calibration curve
#' @description Copy one of the calibration curves into memory.
#' @details Copy the radiocarbon calibration curve defined by cc into memory.
#' @return The calibration curve (invisible).
#' @param cc Calibration curve for 14C dates: \code{cc=1} for IntCal20 (northern hemisphere terrestrial), \code{cc=2} for Marine20 (marine),
#' \code{cc=3} for SHCal20 (southern hemisphere terrestrial). Alternatively, one can also write, e.g., "IntCal20", "Marine13". One can also make a custom-built calibration curve, e.g. using \code{mix.ccurves()}, and load this using \code{cc=4}. In this case, it is recommended to place the custom calibration curve in its own directory, using \code{cc.dir} (see below).
#' @param postbomb Use \code{postbomb=TRUE} to get a postbomb calibration curve (default \code{postbomb=FALSE}). For monthly data, type e.g. \code{ccurve("sh1-2_monthly")}
#' @param cc.dir Directory of the calibration curves. Defaults to where the package's files are stored (system.file), but can be set to, e.g., \code{cc.dir="ccurves"}.
#' @param resample The IntCal curves come at a range of 'bin sizes'; every year from 0 to 5 kcal BP, then every 5 yr until 15 kcal BP, then every 10 yr until 25 kcal BP, and every 20 year thereafter. The curves can be resampled to constant bin sizes, e.g. \code{resample=5}. Defaults to FALSE. 
#' @param glue If a postbomb curve is requested, it can be 'glued' to the pre-bomb curve. This feature is currently disabled - please use \code{glue.ccurves} instead
#' @param as.F Return the F values, calculated from the C14 ages (columns 2 and 3). Defaults to \code{as.F=FALSE}.
#' @param as.pMC Return the pMC values, calculated from the C14 ages (columns 2 and 3). Defaults to \code{as.pMC=FALSE}.
#' @param as.D If loading a curve that contains 2 additional columns containing the D14C values, then these can be returned instead of the curve's C14 ages and errors. Defaults to \code{as.D=FALSE}.
#' @param decimals Number of decimals to report when as.F=TRUE. Defaults to 8.
#' @examples
#' intcal20 <- ccurve(1)
#' marine20 <- ccurve(2)
#' shcal20 <- ccurve(3)
#' marine98 <- ccurve("Marine98")
#' pb.sh3 <- ccurve("sh3")
#' @references
#' Emmenegger et al., 2024. ICOS ATC 14C Release analysed by ICOS CRL from Jungfraujoch (6.0 m), 2015-09-21–2023-10-02, ICOS RI, \url{https://hdl.handle.net/11676/6c_RZ7NHc2dnZv7d84BMY_YY}
#'
#' Hammer and Levin 2017, "Monthly mean atmospheric D14CO2 at Jungfraujoch and Schauinsland from 1986 to 2016", heiDATA: Heidelberg Research Data Repository V2 \doi{10.11588/data/10100} 
#'
#' Heaton et al. 2020 Marine20-the marine radiocarbon age calibration curve (0-55,000 cal BP). Radiocarbon 62, 779-820, \doi{10.1017/RDC.2020.68}
#'
#' Hogg et al. 2013 SHCal13 Southern Hemisphere Calibration, 0-50,000 Years cal BP. Radiocarbon 55, 1889-1903, \doi{10.2458/azu_js_rc.55.16783}
#'
#' Hogg et al. 2020 SHCal20 Southern Hemisphere calibration, 0-55,000 years cal BP. Radiocarbon 62, 759-778, \doi{10.1017/RDC.2020.59}
#'
#' Hua et al. 2013 Atmospheric radiocarbon for the period 1950-2010. Radiocarbon 55(4), \doi{10.2458/azu_js_rc.v55i2.16177}
#' 
#' Hua et al. 2022 Atmospheric radiocarbon for the period 1950-2019. Radiocarbon 64(4), 723-745, \doi{10.1017/RDC.2021.95}
#'
#' Levin and Kromer 2004 The tropospheric 14CO2 level in mid latitudes of the Northern Hemisphere. Radiocarbon 46, 1261-1272
#'
#' Reimer et al. 2004 IntCal04 terrestrial radiocarbon age calibration, 0-26 cal kyr BP. Radiocarbon 46, 1029-1058, \doi{10.1017/S0033822200032999}
#'
#' Reimer et al. 2009 IntCal09 and Marine09 radiocarbon age calibration curves, 0-50,000 years cal BP. Radiocarbon 51, 1111-1150, \doi{10.1017/S0033822200034202}
#'
#' Reimer et al. 2013 IntCal13 and Marine13 radiocarbon age calibration curves 0-50,000 years cal BP. Radiocarbon 55, 1869-1887, \doi{10.2458/azu_js_rc.55.16947}
#'
#' Reimer et al. 2020 The IntCal20 Northern Hemisphere radiocarbon age calibration curve (0-55 cal kBP). Radiocarbon 62, 725-757, \doi{10.1017/RDC.2020.41}
#'
#' Stuiver et al. 1998 INTCAL98 radiocarbon age calibration, 24,000-0 cal BP. Radiocarbon 40, 1041-1083, \doi{10.1017/S0033822200019123}
#' 
#' van der Plicht et al. 2004. NotCal04—Comparison/Calibration 14C Records 26–50 Cal Kyr BP. Radiocarbon 46, 1225-1238, \doi{10.1017/S0033822200033117}
#' @export
ccurve <- function(cc=1, postbomb=FALSE, cc.dir=NULL, resample=0, glue=FALSE, as.F=FALSE, as.pMC=FALSE, as.D=FALSE, decimals=8) {
  if(postbomb) {
    curves <- c(
      "nh1" = "postbomb_NH1.14C",
      "nh2" = "postbomb_NH2.14C",
      "nh3" = "postbomb_NH3.14C",
      "sh1-2" = "postbomb_SH1-2.14C",
      "sh3" = "postbomb_SH3.14C",
      "nh1_monthly" = "postbomb_NH1_monthly.14C",
      "nh2_monthly" = "postbomb_NH2_monthly.14C",
      "nh3_monthly" = "postbomb_NH3_monthly.14C",
      "sh1-2_monthly" = "postbomb_SH1-2_monthly.14C",
      "sh3_monthly" = "postbomb_SH3_monthly.14C",
      "kure" = "Kure.14C",
      "levinkromer" = "LevinKromer.14C",
      "santos" = "Santos.14C",
      "jungfraujoch" = "Jungfraujoch.14C")
    if(tolower(cc) %in% names(curves))
      fl <- curves[[cc]] else
        stop("cannot find this curve", call. = FALSE)
  } else {
      curves <- c(
        "1" = "intcal20.14c", "intcal20" = "intcal20.14c",
        "2" = "marine20.14c", "marine20" = "marine20.14c",
        "3" = "shcal20.14c", "shcal20" = "shcal20.14c",
        "4" = "mixed.14C",
        "mixed" = "mixed.14C",
        "intcal13" = "3Col_intcal13.14C",
        "marine13" = "3Col_marine13.14C",
        "shcal13" = "3Col_shcal13.14C",
        "intcal09" = "3Col_intcal09.14C",
        "marine09" = "3Col_marine09.14C",
        "intcal04" = "3Col_intcal04.14C",
        "marine04" = "3Col_marine04.14C",
        "intcal98" = "3Col_intcal98.14C",
        "marine98" = "3Col_marine98.14C",
        "nh1" = "postbomb_NH1.14C",
        "nh2" = "postbomb_NH2.14C",
        "nh3" = "postbomb_NH3.14C",
        "sh1-2" = "postbomb_SH1-2.14C",
        "sh3" = "postbomb_SH3.14C",
        "nh1_monthly" = "postbomb_NH1_monthly.14C",
        "nh2_monthly" = "postbomb_NH2_monthly.14C",
        "nh3_monthly" = "postbomb_NH3_monthly.14C",
        "sh1-2_monthly" = "postbomb_SH1-2_monthly.14C",
        "sh3_monthly" = "postbomb_SH3_monthly.14C",
        "kure" = "Kure.14C",
        "levinkromer" = "LevinKromer.14C",
        "santos" = "Santos.14C",
        "jungfraujoch" = "Jungfraujoch.14C")
      if(tolower(cc) %in% names(curves))
        fl <- curves[[as.character(tolower(cc))]] else
          stop("cannot find this curve", call. = FALSE)
  }

  if(length(cc.dir) == 0) # then look into the package's inst/extdata folder
    Cc <- system.file("extdata/", fl, package='rintcal') else
      Cc <- file.path(cc.dir, fl)

  if(fl %in% c("intcal20.14c", "marine20.14c", "shcal20.14c")) # skip first 11 lines and use commas 
    cc <- fastread(Cc, skip=11, sep=",") else
      cc <- fastread(Cc, skip=0, sep=" ")

  # the cal BP column should be increasing, i.e. most recent ages at the top 
  cc <- cc[order(cc[,1]),]

  # for files with D14C values (the 'official' intcal20 files ending in '14c', which have D14C and its error as their final two columns)
  if(as.D) 
    if(ncol(cc) == 5) 
      cc <- cbind(cc[, 1], cc[,4], cc[,5]) else
        stop("this does not seem to be a file with 5 columns, cannot return the D14C values")
  
  if(as.F || as.pMC)
    cc <- cc_C14toF14C(cc, decimals=decimals) else
      cc <- cbind(cc[,1:3])
  if(as.pMC)
    cc[,2:3] <- 100 * cc[,2:3]

  if(resample > 0) {
    yr <- seq(min(cc[,1]), max(cc[,1]), by=resample)
    mu <- approx(cc[,1], cc[,2], yr)$y
    er <- approx(cc[,1], cc[,3], yr)$y
    cc <- cbind(yr, mu, er)
  }
  invisible(cc)
}



#' @name mix.ccurves
#' @title Build a custom-made, mixed calibration curve.
#' @description If two curves need to be `mixed' to calibrate, e.g. for dates of mixed terrestrial and marine carbon sources, then this function can be used. The curve will be returned invisibly, or saved in a temporary directory together with the main calibration curves. This temporary directory then has to be specified in further commands, e.g. for rbacon: \code{Bacon(, cc.dir=tmpdr)} (see examples). It is advisable to make your own curves folder and have cc.dir point to that folder.
#' @details The proportional contribution of each of both calibration curves has to be set.
#'
#' @param proportion Proportion of the first calibration curve required. e.g., change to \code{proportion=0.7} if \code{cc1} should contribute 70\% (and \code{cc2} 30\%) to the mixed curve.
#' @param cc1 The first calibration curve to be mixed. Defaults to the northern hemisphere terrestrial curve IntCal20.
#' @param cc2 The second calibration curve to be mixed. Defaults to the marine curve IntCal20.
#' @param name Name of the new calibration curve.
#' @param cc.dir Name of the directory where to save the file. Since R does not allow automatic saving of files, this points to a temporary directory by default. Adapt to your own folder, e.g., \code{cc.dir="~/ccurves"} or in your current working directory, \code{cc.dir="."}.
#' @param thiscurve1 As an alternative to using curves that come with the package, a tailor-made curve can be provided for the first curve (as three columns: cal BP, C14 age, error).
#' @param thiscurve2 As an alternative to using curves that come with the package, a tailor-made curve can be provided for the second curve (as three columns: cal BP, C14 age, error).
#' @param postbomb1 Option to provide a postbomb curve for the first curve (defaults to FALSE).
#' @param postbomb2 Option to provide a postbomb curve for the second curve (defaults to FALSE).
#' @param as.F The curves can be returned as F14C values instead of the default C14. Make sure that if as.F=TRUE and you are using thiscurve1 and/or thiscurve2, that these curves are in F14C space already.
#' @param as.pMC The curves can be returned as pMC values instead of the default C14. Make sure that if as.pMC=TRUE and you are using thiscurve1 and/or thiscurve2, that these curves are in pMC space already.
#' @param save Save the curve in the folder specified by dir. Defaults to FALSE.
#' @param offset Any offset and error to be applied to \code{cc2} (default 0 +- 0). Entered as two columns (possibly of just one row), e.g. \code{offset=cbind(100,0)}
#' @param round The entries can be rounded to a specified amount of decimals. Defaults to no rounding.
#' @param sep Separator between fields (tab by default, "\\t")
#' @param decimals Number of decimals to report when as.F=TRUE. Defaults to 8.
#' @return A file containing the custom-made calibration curve, based on calibration curves \code{cc1} and \code{cc2}.
#' @examples
#' tmpdir <- tempdir()
#' new.ccdir(tmpdir)
#' mix.ccurves(cc.dir=tmpdir)
#' # now assume the offset is constant but its uncertainty increases over time:
#' cc <- ccurve()
#' offset <- cbind(rep(100, nrow(cc)),  seq(0, 1e3, length=nrow(cc)))
#  mix.ccurves(cc.dir=tmpdir, offset=offset)
#' # clean up:
#' unlink(tmpdir)
#' @export
mix.ccurves <- function(proportion=.5, cc1="IntCal20", cc2="Marine20", postbomb1=FALSE, postbomb2=FALSE, as.F=FALSE, as.pMC=FALSE, name="mixed.14C", cc.dir=c(), thiscurve1=c(), thiscurve2=c(), save=FALSE, offset=cbind(0,0), round=c(), sep=" ", decimals=8) {
  # place the IntCal curves within the same folder as the new curve:
  if(length(cc.dir) == 0)
    cc.dir <- tempdir()
  if(!dir.exists(cc.dir))
    dir.create(cc.dir)
  curves <- list.files(system.file("extdata", package='rintcal'), pattern="\\.14[Cc]$", full.names=TRUE)
  file.copy(curves, cc.dir)

  if(length(thiscurve1) == 0)
    cc1 <- ccurve(cc1, cc.dir=cc.dir, postbomb=postbomb1, as.F=as.F, as.pMC=as.pMC, decimals=decimals) else
      cc1 <- thiscurve1
  if(length(thiscurve2) == 0)
    cc2 <- ccurve(cc2, cc.dir=cc.dir, postbomb=postbomb2, as.F=as.F, as.pMC=as.pMC, decimals=decimals) else
      cc2 <- thiscurve2

  cc2.mu <- approx(cc2[,1], cc2[,2], cc1[,1], rule=2)$y + offset[,1] # interpolate cc2 to the calendar years of cc1
  cc2.error <- approx(cc2[,1], cc2[,3], cc1[,1], rule=2)$y
  cc2.error <- sqrt(cc2.error^2 + offset[,2]^2)
  mu <- proportion * cc1[,2] + (1-proportion) * cc2.mu
  # error <- proportion * cc1[,3] + (1-proportion) * cc2.error
  error <- sqrt(proportion^2 * cc1[,3]^2 + (1-proportion)^2 * cc2.error^2) # July '24

  mycc <- cbind(cc1[,1], mu, error)
  if(length(round) > 0)
    mycc <- round(mycc)

  if(save) {
    fastwrite(mycc, file.path(cc.dir, name), row.names=FALSE, col.names=FALSE, sep=sep)
    message(name, " saved in folder ", cc.dir)
  }
  invisible(mycc)
}



#' @name glue.ccurves
#' @title Glue calibration curves
#' @description Produce a custom curve by merging two calibration curves, e.g. a prebomb and a postbomb one for dates which straddle both curves.
#' @return The custom-made curve (invisibly)
#' @param prebomb The prebomb curve. Defaults to "IntCal20"
#' @param postbomb The postbomb curve. Defaults to "NH1" (Hua et al. 2013)
#' @param thisprebombcurve As an alternative to using existing curves, a tailor-made curve can be provided for the prebomb curve (as three columns: cal BP, C14 age, error)
#' @param thispostbombcurve As an alternative to using existing curves, a tailor-made curve can be provided for the postbomb curve (as three columns: cal BP, C14 age, error)
#' @param as.F The glued curve can be returned as F14C values instead of the default C14. Make sure that if as.F=TRUE and you are using thisprebombcurve and/or thispostbombcurve, that these curves are in F14C space already.
#' @param as.pMC The curves can be returned as pMC values instead of the default C14. Make sure that if as.pMC=TRUE and you are using thiscurve1 and/or thiscurve2, that these curves are in pMC space already.
#' @param cc.dir Directory of the calibration curves. Defaults to where the package's files are stored (system.file), but can be set to, e.g., \code{cc.dir="ccurves"}.
#' @param decimals Number of decimals to report when as.F=TRUE. Defaults to 5.
#' @examples
#' my.cc <- glue.ccurves()
#' @export
glue.ccurves <- function(prebomb="IntCal20", postbomb="NH1", thisprebombcurve=c(), thispostbombcurve=c(), as.F=FALSE, as.pMC=FALSE, cc.dir=c(), decimals=8) {
  if(length(thispostbombcurve) == 0)
    postbomb <- ccurve(postbomb, TRUE, cc.dir=cc.dir, as.F=as.F, as.pMC=as.pMC, decimals=decimals) else
      postbomb <- thispostbombcurve
  if(length(thisprebombcurve) == 0)
    prebomb <- ccurve(prebomb, FALSE, cc.dir=cc.dir, as.F=as.F, as.pMC=as.pMC, decimals=decimals) else
      prebomb <- thisprebombcurve

  glued <- rbind(postbomb, prebomb)
  glued <- glued[order(glued[,1]),]
  repeated <- which(diff(glued[,1]) == 0)
  if(length(repeated) > 0)
    invisible(glued[-repeated,]) else # remove any repeated years
      invisible(glued[order(glued[,1]),])
}



### making a selection of realm functions available locally (for intcal.data, glue.ccurves, mix.ccurves and ccurve), to avoid need for circular loading of `rice`

C14.F14C <- function(y, er, decimals=8, lambda=8033) {
  y <- as.matrix(y)
  er <- as.matrix(er)
  if(length(y) != length(er))
    stop("y and er must have the same length.")	
  fy <- exp(-y / lambda)
    
  er1 <- abs(fy - exp(-(y - er) / lambda))
  er2 <- abs(fy - exp(-(y + er) / lambda))
  sdev <- pmax(er1, er2)
  return(round(cbind(fy, sdev, deparse.level=0), decimals))
}



C14.pMC <- function(y, er, ratio=100, decimals=8, lambda=8033)
  return(100*C14.F14C(y, er, decimals=decimals, lambda=lambda))



F14C.D14C <- function(F14C, t)
  return( 1000 * ((F14C / exp(-t/8267)) - 1))



# internal function, adapted from the rice package. Expects y and er
cc_C14toF14C <- function(cc, decimals=8, lambda=8033) {
  y <- cc[,2]
  er <- cc[,3]
  if(length(y) != length(er))
    stop("y and er must have the same length.")	
  fy <- exp(-y / lambda)
 
  er1 <- abs(fy - exp(-(y - er) / lambda))
  er2 <- abs(fy - exp(-(y + er) / lambda))
  sdev <- pmax(er1, er2)
  return(round(cbind(cc[,1], fy, sdev, deparse.level=0), decimals))
}
