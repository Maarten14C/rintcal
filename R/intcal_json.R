# these functions were kindly contributed by Christopher Bronk Ramsey, University of Oxford



#' @name intcal.read.data
#' @title Read data underlying the IntCal curves.
#' @description Download the json file that contains the IntCal20 radiocarbon calibration curves and the contributing data series.
#' @details The intcal curves consist of the IntCal20, SHCal20 and Marine20 calibration curves. The details of these curves can be loaded, as well as the underlying data such as tree-ring records.
#' @param fname The name of the json file. By default this downloads the json file from the intchron.org site
#' @examples
#'  intcal <- intcal.read.data() # from intchron.org
#' @export
intcal.read.data <- function(fname=url('https://intchron.org/archive/IntCal/IntCal20/index.json')) {
  rtn <- jsonlite::fromJSON(fname, simplifyDataFrame = FALSE)
 
  # internal function, only called within intcal.read.data
  intcal.make.data.frame <- function(obj) {
    obj[['data']] <- data.frame(obj[['data']])
    obj[['refs']] <- I(obj[['refs']])
    return(obj)
  }
 
  if(length(rtn[['json_application']]) == 0)
    return(FALSE)
  json_appl <- strsplit(rtn[['json_application']],split='[.:]')
  if(json_appl[[1]][1] != 'INTCHRON'){return(FALSE) }
  if(json_appl[[1]][2] == 'Record') {
    s <- 1
    sl <- length(rtn[['series_list']])+1
    rtn[['refs']] <- I(rtn[['refs']])
    while(s < sl) {
      rtn[['series_list']][[s]] <- intcal.make.data.frame(rtn[['series_list']][[s]])
      s <- s+1
    }
    return(rtn)
  }

  if(json_appl[[1]][2] == 'Series') {
    rtn <- intcal.make.data.frame(rtn)
    return(rtn)
  }

  if(json_appl[[1]][2] != 'Project') 
    return(rtn)

  r <- 1
  rl <- length(rtn[['records']])+1
  while(r < rl) {
    s <- 1
    sl <- length(rtn[['records']][[r]][['file_data']][['series_list']])+1
    rtn[['records']][[r]][['file_data']][['refs']] <- I(rtn[['records']][[r]][['file_data']][['refs']])
    while(s < sl) {
      rtn[['records']][[r]][['file_data']][['series_list']][[s]] <- 
        intcal.make.data.frame(rtn[['records']][[r]][['file_data']][['series_list']][[s]])
     s <- s+1
    }
    r <- r+1
   }

  s <- 1
  sl <- length(rtn[['project_series_list']])+1
  while(s < sl) {
    rtn[['project_series_list']][[s]][['file_data']] <- intcal.make.data.frame(rtn[['project_series_list']][[s]][['file_data']])
    s <- s+1
  }

  p <- 1
  pl <- length(rtn[['parameters']])+1
  while(p < pl){
    rtn[['parameters']][[p]][['options']] <- I(rtn[['parameters']][[p]][['options']])
    rtn[['parameters']][[p]][['option_colors']] <- I(rtn[['parameters']][[p]][['option_colors']])
    p <- p+1
   }

  return(rtn)
}



#' @name intcal.write.data
#' @title Write intcal data to a file.
#' @description Write the intcal.json file that comes with the rintcal packages to somewhere local. This can be useful if you want to avoid repeatedly downloading the json file from intchron.org.
#' @param data intcal variable as obtained from intcal.read.data()
#' @param fname Name of the file to be written
#' @examples
#'  intcal <- intcal.read.data(url('https://intchron.org/archive/IntCal/IntCal20/index.json'))
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
#'  intcal <- intcal.read.data(url('https://intchron.org/archive/IntCal/IntCal20/index.json'))
#'  # all datasets from the Southern Hemisphere:
#'  sh.data <- intcal.data.frames(intcal, intcal_set_type='SH') 
#'  head(sh.data)
#' # read dataset 52
#'  intcal.52 <- intcal.data.frames(intcal, intcal_set=52) 
#'  head(intcal.52)
#' @export
intcal.data.frames <- function(obj, ...) {
  params <- list(...)
  print(params)

  rtn <- list()
  r <- 1
  rl <- length(obj[['records']])+1
  while(r < rl) {
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
