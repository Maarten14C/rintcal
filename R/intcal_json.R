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
