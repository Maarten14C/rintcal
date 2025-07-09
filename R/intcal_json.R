# these functions were kindly contributed by Christopher Bronk Ramsey, University of Oxford, and modified by Maarten Blaauw, Queen's University Belfast

#' IntCal20 json file
#'
#' The IntCal20 calibration curves and their underpinning data. 
#' This is based on a json file produced by Prof. Christopher Bronk Ramsey, University of Oxford.
#' @format ## `intcal`
#' A list with six main entries:
#' \describe{
#'   \item{json_application}{IntChron project name}
#'   \item{records}{a list with 139 entries for each IntCal dataset}
#'   \item{project_series_list}{a list with 5 entries: IntCal20, Marine20, SHCal20, a list of the underlying datasets, and a GICC vs IntCal20 comparison}
#'   \item{parameters}{an empty list}
#'   \item{bibliography}{a list with 141 bibliography entries}
#'   \item{options}{a list of 17 options (not used)}
#' }
#' @source <https://intchron.org/archive/IntCal/IntCal20/index.json>
#'   
"intcal"



#' @name intcal.extract.record
#' @title Extract an IntCal20 record
#' @description Extract one of the 140 records contributing to the IntCal20 calibration curves 
#' @return The most relevant information for each record (where available, the record and site names, intcal series and division number, country, longitude, latitude, taxon, dois of publications, notes, calendar information (e.g., dendrochronology) and radiocarbon data)
#' @param i The IntCal record, in the order as they appear in the IntCal20 json file (140 entries). Must be a single integer between 1 and 140. 
#' @examples
#'  record_1 <- intcal.extract.record(1)
#' @export
intcal.extract.record <- function(i) {
  if(!"intcal" %in% ls(.GlobalEnv))
    intcal <- rintcal::intcal
  if(!exists('intcal'))
    stop("please load the intcal data first, with 'data(intcal)'")
  
  if(!is.numeric(i) || length(i) != 1 || i < 1 || i > 140)
    stop("Argument 'i' must be a single integer between 1 and 140.")
  
  dat <- intcal$records[[i]]
  data <- list(calendar=c(), radiocarbon=c())	
  data$record <- dat$record
  data$site <- dat$site
  data$country	<- dat$country
  data$longitude <- dat$longitude
  data$latitude <- dat$latitude
  data$taxon <- dat$taxon  
  data$intcal_set <- dat$file_data$series_list[[1]]$intcal_set
  data$intcal_division <- dat$file_data$series_list[[1]]$intcal_division
  data$doi <- dat$file_data$series_list[[1]]$refs
  data$notes <- dat$file_data$series_list[[1]]$notes 
  if(length(dat$file_data$series_list) > 1)
    data$calendar_notes <- dat$file_data$series_list[[2]]$notes else
      data$calendar_notes <- NA
  if(length(dat$file_data$series_list) > 2) {
    data$calendar <- dat$file_data$series_list[[3]]$data
    data$from <- dat$file_data$series_list[[3]]$t_from
    data$to <- dat$file_data$series_list[[3]]$t_to
    data$ring_count <- dat$file_data$series_list[[3]]$ring_count
  } else {
	  data$calendar <- NA
	  data$from <- NA	
	  data$to <- NA
	  data$ring_count <- NA	  	
    }
  data$radiocarbon <- dat$file_data$series_list[[1]]$data
  invisible(data)
}



#' @name intcal.plot.record
#' @title Plot an IntCal20 record
#' @description Plot the calendar and radiocarbon data of an IntCal20 record
#' @return A plot of the calendar and radiocarbon ages, indicating uncertainties (error bars) and age blocks (e.g, for trees where blocks of >1 rings were dated) where relevant.
#' @param i The IntCal record, in the order as they appear in the IntCal20 json file (140 entries). Must be a single integer between 1 and 140. 
#' @param col Colour of the symbols. Defaults to semi-transparent blue, \code{col=rgb(0,0,1,.5)}.
#' @param pch Symbol of the dates. Defaults to a filled circle, pch=20.
#' @param pch.cex Size of the symbol. Defaults to 0.3.
#' @param lwd Line width of the error bars. Defaults to 1.
#' @param lty Line type of the error bars. Defaults to continuous, 1. 
#' @param cal.lim Limits of the horizontal/calendar scale. Calculated automatically by default.
#' @param C14.lim Limits of the C-14 scale. Calculated automatically by default
#' @param add Make a new plot (default, \code{add=FALSE}). The alternative is to add to an existing plot.
#' @param cal.lab Label of the calendar axis. Defaults to 'cal BP' or 'kcal BP'.
#' @param C14.lab Label of the C-14 axis. Defaults to '14C BP' or '14C kBP'
#' @param ka Whether or not to use ka (thousands of years). Defaults to FALSE (i.e., cal BP).
#' @param as.F Return the F values, calculated from the C14 ages (columns 2 and 3). Defaults to \code{as.F=FALSE}.
#' @param as.pMC Return the pMC values, calculated from the C14 ages (columns 2 and 3). Defaults to \code{as.pMC=FALSE}.
#' @param draw.z Whether or not to plot the spread in calendar years of blocks of (mostly) tree rings. This is for tree-ring datasets where individual dates were taken on blocks of rings covering e.g. 10 or 20 years. 
#' @param draw.calsigma Whether or not to plot the calendar age uncertainties where available.
#' @param grid Whether or not to add a grid to the plot. 
#' @param grid.lty Line type of the grid.
#' @param grid.col Colour of the grid.
#' @param draw.cc Whether or not to also plot the calibration curve. Defaults to plotting the IntCal20 calibration curve, but can also be set to 2 (Marine20), 3 (SHCal20), or NA (none).
#' @param cc.col Colour of the calibration curve. Defaults to semi-transparent darkgreen, \code{cc.col=rgb(0,.5,0,.5)}.
#' @param legend.loc Location of the legend. Defaults to top right. Set to NA if you don't want to plot the legend. 
#' @param legend.cex Relative size of the font of the legend. Defaults to 0.5.
#' @seealso \code{\link{intcal.extract.record}}
#' @examples
#'  record_1 <- intcal.plot.record(1)
#'  record_10 <- intcal.plot.record(10, add=TRUE, col=rgb(1,0,0,.5), legend.loc="bottomright")
#' @export
intcal.plot.record <- function(i, col=rgb(0, 0, 1, .5), pch=19, pch.cex=.3, lwd=1, lty=1, cal.lim=c(), C14.lim=c(), add=FALSE, cal.lab=c(), C14.lab=c(), ka=FALSE, as.F=FALSE, as.pMC=FALSE, draw.z=TRUE, draw.calsigma=TRUE, grid=FALSE, grid.lty=2, grid.col=rgb(0,0,0,.5), draw.cc=1, cc.col=rgb(0,.5,0, .5), legend.loc="topleft", legend.cex=.5) {
  if(!"intcal" %in% ls(.GlobalEnv))
    intcal <- rintcal::intcal
  if(!exists('intcal'))
    stop("please load the intcal data first, with 'data(intcal)'")	

  dat <- intcal.extract.record(i)
  if(!is.na(draw.cc)) 
    if(draw.cc>0 && draw.cc<4)
      cc <- ccurve(draw.cc, as.F=as.F, as.pMC=as.pMC) else cc <- NA

  if(as.F || as.pMC) {
	translated <- C14.F14C(dat$radiocarbon$r_date, dat$radiocarbon$r_date_sigma)
	if(as.pMC)
      translated <- 100 * translated
	dat$radiocarbon$r_date <- translated[,1]
	dat$radiocarbon$r_date_sigma <- translated[,2]
  }

  if(length(cal.lim) == 0) {
    cal.lim <- range(dat$radiocarbon$calage-dat$radiocarbon$calage_range,
      dat$radiocarbon$calage+dat$radiocarbon$calage_range)
	C14.lim <- range(dat$radiocarbon$r_date-dat$radiocarbon$r_date_sigma,
      dat$radiocarbon$r_date+dat$radiocarbon$r_date_sigma)
	
    if(!is.na(draw.cc)) {
        within <- min(which(cc[,1] >= min(cal.lim))):max(which(cc[,1] <= max(cal.lim)))
        C14.lim <- range(C14.lim, cc[within,2]-cc[within,3], cc[within,2]+cc[within,3])	
    }
  }

  if(!add) {  
    if(length(cal.lab) == 0)
      if(ka)
        cal.lab <- "kcal BP" else
          cal.lab <- "cal BP"
    if(length(C14.lab) == 0)
      if(ka)
        C14.lab <- "C14 kBP" else
          C14.lab <- "C14 BP"			 	 
    plot(0, type="n", pch=pch, xlim=cal.lim, xlab=cal.lab, ylim=C14.lim, ylab=C14.lab)
    if(grid)
      grid(lty=grid.lty, col=grid.col)	 
  }	
  points(dat$radiocarbon$calage, dat$radiocarbon$r_date, pch=pch, col=col, cex=pch.cex)
  
  # check what uncertainties are present: could be z, sigma, or z and sigma
  
  has.z <- any(dat$radiocarbon$z_range > 0, na.rm=TRUE)
  has.calsigma <- any(dat$radiocarbon$calage_sigmaC > 0, na.rm=TRUE)
  
  if(draw.z)
    if(has.z) {
      these <- which(dat$radiocarbon$z_range > 0)
	  width <- dat$radiocarbon$z_range/2
      rect(dat$radiocarbon$calage[these]-width[these], 
        dat$radiocarbon$r_date[these]-dat$radiocarbon$r_date_sigma[these],
        dat$radiocarbon$calage[these]+width[these],
	    dat$radiocarbon$r_date[these]+dat$radiocarbon$r_date_sigma[these],
        col=col, border=NA)	
        if(length(these) < length(dat$radiocarbon$z_range))
          segments(dat$radiocarbon$calage[-these],
            dat$radiocarbon$r_date[-these]-dat$radiocarbon$r_date_sigma[-these], 
            dat$radiocarbon$calage[-these],
            dat$radiocarbon$r_date[-these]+dat$radiocarbon$r_date_sigma[-these], col=col)	
    } else {
      segments(dat$radiocarbon$calage, dat$radiocarbon$r_date-dat$radiocarbon$r_date_sigma,
        dat$radiocarbon$calage, dat$radiocarbon$r_date+dat$radiocarbon$r_date_sigma, 
        col=col, lwd=lwd, lty=lty)
    }

  if(draw.calsigma) 
    if(has.calsigma) {
      these <- which(dat$radiocarbon$calage_sigmaC > 0)
  	  if(length(these) > 0)
        segments(dat$radiocarbon$calage[these]-dat$radiocarbon$calage_sigmaC[these], 
          dat$radiocarbon$r_date, dat$radiocarbon$calage[these]+dat$radiocarbon$calage_sigmaC[these],
          dat$radiocarbon$r_date, col=col, lty=lty, lwd=lwd)	
    }

  if(!is.na(draw.cc)) {
    cc.pol <- cbind(c(cc[,1], rev(cc[,1])), c(cc[,2]-cc[,3], rev(cc[,2]+cc[,3])))
    polygon(cc.pol, col=cc.col, border=cc.col)  	
  }	
    
  if(!is.na(legend.loc)) {
    txt <- paste0(dat$record, ", ", dat$country, "\n",
	if(dat$taxon !="") {paste(dat$taxon[1], "\n")}, dat$intcal_set, "_", dat$intcal_division, "\n")
    legend(legend.loc, legend=txt, bty="n", cex=legend.cex, text.col=col)
  }

  invisible(dat)
}



#' @name intcal.read.data
#' @title Read data underlying the IntCal curves.
#' @description Download the json file that contains the IntCal20 radiocarbon calibration curves and the contributing data series.
#' @details The intcal curves consist of the IntCal20, SHCal20 and Marine20 calibration curves. The details of these curves can be loaded, as well as the underlying data such as tree-ring records.
#' @param from.jsonfile The name and location of the json file (if used). Defaults to FALSE, and then the data will be loaded from within the rintcal package
#' @param from.intchron.org Download the IntCal20 json file the inchron.org server. Defaults to FALSE, and then the data will be loaded from within the rintcal package
#' @examples
#'  intcal <- intcal.read.data()
#' @export
intcal.read.data <- function(from.intchron.org=FALSE, from.jsonfile=FALSE) {
  if(from.intchron.org) {
    json <- url('https://intchron.org/archive/IntCal/IntCal20/index.json') 
    intcal <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE) 
  } else
    if(from.jsonfile > 0) 
      intcal <- jsonlite::fromJSON(from.jsonfile, simplifyDataFrame = FALSE) else 
        intcal <- rintcal::intcal # then read rintcal/data/intcal.rda 

  # internal function, only called within intcal.read.data
  intcal.make.data.frame <- function(obj) {
    obj[['data']] <- data.frame(obj[['data']])
    obj[['refs']] <- I(obj[['refs']])
    return(obj)
  }
 
  if(length(intcal[['json_application']]) == 0)
    return(FALSE)
  json_appl <- strsplit(intcal[['json_application']],split='[.:]')
  if(json_appl[[1]][1] != 'INTCHRON'){return(FALSE) }
  if(json_appl[[1]][2] == 'Record') {
    s <- 1
    sl <- length(intcal[['series_list']])+1
    intcal[['refs']] <- I(intcal[['refs']])
    while(s < sl) {
      intcal[['series_list']][[s]] <- intcal.make.data.frame(intcal[['series_list']][[s]])
      s <- s+1
    }
    return(intcal)
  }

  if(json_appl[[1]][2] == 'Series') {
    intcal <- intcal.make.data.frame(intcal)
    return(intcal)
  }

  if(json_appl[[1]][2] != 'Project') 
    return(intcal)

  r <- 1
  while(r < (length(intcal[['records']])+1)) {
    s <- 1
    sl <- length(intcal[['records']][[r]][['file_data']][['series_list']])+1
    intcal[['records']][[r]][['file_data']][['refs']] <- I(intcal[['records']][[r]][['file_data']][['refs']])
    while(s < sl) {
      intcal[['records']][[r]][['file_data']][['series_list']][[s]] <- 
        intcal.make.data.frame(intcal[['records']][[r]][['file_data']][['series_list']][[s]])
     s <- s+1
    }
    r <- r+1
   }

  s <- 1
  sl <- length(intcal[['project_series_list']])+1
  while(s < sl) {
    intcal[['project_series_list']][[s]][['file_data']] <- intcal.make.data.frame(intcal[['project_series_list']][[s]][['file_data']])
    s <- s+1
  }

  p <- 1
  while(p < (length(intcal[['parameters']])+1)){
    intcal[['parameters']][[p]][['options']] <- I(intcal[['parameters']][[p]][['options']])
    intcal[['parameters']][[p]][['option_colors']] <- I(intcal[['parameters']][[p]][['option_colors']])
    p <- p+1
   }

  return(intcal)
}



#' @name intcal.write.data
#' @title Write intcal data to a file.
#' @description Write the intcal.json file that comes with the rintcal packages to somewhere local. This can be useful if you want to avoid repeatedly downloading the json file from intchron.org.
#' @param data intcal variable as obtained from intcal.read.data()
#' @param fname Name of the file to be written
#' @examples
#'  intcal <- intcal.read.data()
#'  myintcal <- tempfile()
#'  intcal.write.data(intcal, myintcal)
#' @export
intcal.write.data <- function(data, fname)
  write(jsonlite::toJSON(data, dataframe='columns', auto_unbox=TRUE, null = "null", na = "null"), fname)



#' @name intcal.data.frames
#' @title Extract from the intcal file
#' @description Extract items from the intcal json file.
#' @param obj Name of the object
#' @param ... Additional options can be provided, see examples
#' @examples
#'  intcal <- intcal.read.data()
#'  # all datasets from the Southern Hemisphere:
#'  sh.data <- intcal.data.frames(intcal, intcal_set_type='SH') 
#'  head(sh.data)
#'  Irish.oaks <- intcal.data.frames(intcal, intcal_set=3) 
#'  head(Irish.oaks[[2]]$data)
#' @export
intcal.data.frames <- function(obj, ...) {
  params <- list(...)
  print(params)

  rtn <- list()
  r <- 1
  while(r < (length(obj[['records']])+1)) {
    s <- 1
    sl <- length(obj[['records']][[r]][['file_data']][['series_list']])+1
    while(s < sl) {
      ll <- c(obj[['records']][[r]][['file_data']][['header']], obj[['records']][[r]][['file_data']][['series_list']][[s]])
      ll[['refs']] <- unique(c(ll[['refs']],obj[['records']][[r]][['file_data']][['refs']]))
      ok <- TRUE
      for(a in names(params)) {
        if(!length(ll[[a]])) {
          ok <- FALSE
          next
        }
        if(ll[[a]][1] != params[[a]][1])
          ok <- FALSE
      }
      if(ok)
        rtn <- append(rtn,list(ll))
      s <- s+1
    }
  r <- r+1
  }
  return(rtn)
}
