
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
  #'   lines(real.14C, contaminate(real.14C, c(), contam[i], 1, decimals=5), col=contam.col[i])
  #' contam.legend <- seq(0, .1, length=6)
  #' contam.col <- rainbow(length(contam.legend))
  #' text(52e3, contaminate(50e3, c(), contam.legend, 1), labels=contam.legend, col=contam.col, cex=.7)
  #' @export
  contaminate <- function(y, sdev=c(), fraction, F14C, F14C.er=0, decimals=5) {
    y.F <- as.data.frame(age.F14C(y, sdev, decimals))
    mn <- ((1-fraction)*y.F[,1]) + (fraction*F14C)
    if(length(sdev) == 0)
      return(F14C.age(mn, c(), decimals)) else {
        er <- sqrt(y.F[,2]^2 + F14C.er^2)
        return(F14C.age(mn, er, decimals))
      }
  }