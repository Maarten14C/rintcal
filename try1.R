

intcal.data <- function(calmin, calmax, cc1="IntCal20", cc2=NA, BCAD=FALSE, cal.lab=NA, cal.lim=NA, cal.rev=FALSE, c14.lab=NA, c14.lim=NA, c14.rev=FALSE, ka=FALSE, cc1.col=rgb(0,0,1,.5), cc1.fill=rgb(0,0,1,.2), cc2.col=rgb(0,0,.5,.5), cc2.fill=rgb(0,0,.5,.2), data.cols=1:8, data.pch=c(1,2,5,6,15:19), pch.cex=.5, legend.loc="topleft", legend.ncol=2, legend.cex=0.7, cc.legend="bottomright", bty="l",  ...) {
  # find the data corresponding to the period of interest
  mindat <- dat$cal >= calmin
  maxdat <- dat$cal <= calmax
  dat <- dat[which( mindat * maxdat == 1),]

  # read and narrow down the calibration curve(s)
  if(tolower(cc1) == "intcal20")
    cc1 <- "3Col_intcal20.14C" else
    if(tolower(cc1) == "marine20")
      cc1 <- "3Col_marine20.14C" else
      if(tolower(cc1) == "shcal20")
        cc1 <- "3Col_shcal20.14C" else
        if(tolower(cc1) == "intcal13")
          cc1 <- "3Col_intcal13.14C" else
          if(tolower(cc1) == "marine13")
            cc1 <- "3Col_marine13.14C" else
            if(tolower(cc1) == "shcal13")
              cc1 <- "3Col_shcal13.14C" else
                message("I don't know which curve you mean")
  cc1 <- system.file("extdata/", cc1, package = "IntCal")
  cc1 <- read.table(cc1, header=FALSE)
  mindat <- cc1[,1] >= calmin
  maxdat <- cc1[,1] <= calmax
  cc1 <- cc1[which(mindat * maxdat == 1),]
  if(ka)
    cc1 <- cc1/1e3
  cc1.pol <- cbind(c(cc1[,1], rev(cc1[,1])), c(cc1[,2]-cc1[,3], rev(cc1[,2]+cc1[,3])))
  
  if(!is.na(cc2)) {
    if(tolower(cc2) == "intcal20")
      cc2 <- "3Col_intcal20.14C" else
      if(tolower(cc2) == "marine20")
        cc2 <- "3Col_marine20.14C" else
        if(tolower(cc2) == "shcal20")
          cc2 <- "3Col_shcal20.14C" else
          if(tolower(cc2) == "intcal13")
            cc2 <- "3Col_intcal13.14C" else
            if(tolower(cc2) == "marine13")
              cc2 <- "3Col_marine13.14C" else
              if(tolower(cc2) == "shcal13")
                cc2 <- "3Col_shcal13.14C" else
                  message("I don't know which curve you mean")
    cc2 <- system.file("extdata/", cc2, package = "IntCal")
    cc2 <- read.table(cc2, header=FALSE)
    mindat <- cc2[,1] >= calmin
    maxdat <- cc2[,1] <= calmax
    cc2 <- cc2[which(mindat * maxdat == 1),]
    if(ka)
      cc2 <- cc2/1e3
    cc2.pol <- cbind(c(cc2[,1], rev(cc2[,1])), c(cc2[,2]-cc2[,3], rev(cc2[,2]+cc2[,3])))
  }

  # read the data
  dat <- system.file("extdata/", "intcal20_data.txt", package = "IntCal")
  dat <- read.table(dat, header=TRUE, sep=" ")
  sourcesdat <- system.file("extdata/", "intcal20_data_sources.txt", package = "IntCal")
  sourcesdat <- read.table(sourcesdat, nrows=32, sep=",")

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

  if(is.na(cal.lim))
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
  lines(cc1[,1], cc1[,2]-cc1[,3], col=cc1.col)
  lines(cc1[,1], cc1[,2]+cc1[,3], col=cc1.col)
  if(!is.na(cc2)) {
    polygon(cc2.pol, col=cc2.fill, border=NA) # calibration curve
    lines(cc2[,1], cc2[,2]-cc2[,3], col=cc2.col)
    lines(cc2[,1], cc2[,2]+cc2[,3], col=cc2.col)	
  }

  # legend
  if(!is.na(legend.loc)) {
    set <- sourcesdat[which(sourcesdat[,1] %in% these.sets),2]
    set <<- set
    legend(legend.loc, set, col=these.cols, pch=these.pchs, cex=legend.cex, ncol=legend.ncol, text.col=these.cols, bty="n")
  }
  
  if(!is.na(cc.legend)) {
    legend(cc.legend, legend="IntCal20", text.col=cc1.col, cex=legend.cex, bty="n")
  }
  
}

intcal.data(100,200)

# now also a plot.curve function, and an add.curve one


