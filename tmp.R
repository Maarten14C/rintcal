tmpcalibrate <- function(age=2450, error=50, cc=1, postbomb=FALSE, reservoir=0, prob=0.95, BCAD=FALSE, ka=FALSE, cal.lab=c(), C14.lab=c(), cal.lim=c(), C14.lim=c(), cc.col=rgb(0,.5,0,0.7), cc.fill=rgb(0,.5,0,0.7), date.col="red", dist.col=rgb(0,0,0,0.2), dist.fill=rgb(0,0,0,0.2), hpd.fill=rgb(0,0,0,0.3), dist.height=0.3, dist.float=c(.01, .01), cal.rev=FALSE, yr.steps=FALSE, threshold=0.0005, edge=TRUE, normal=TRUE, t.a=3, t.b=4, rounded=1, extend.range=.05, legend.cex=0.8, legend1.loc="topleft", legend2.loc="topright", mgp=c(2,1,0), mar=c(3,3,1,1), xaxs="i", yaxs="i", bty="l", cc.dir=NULL, ...) {
  # read the data
  age <- age-reservoir[1]
  if(length(reservoir) > 1)
    error <- sqrt(error^2 + reservoir[2]^2)
  Cc <- ccurve(cc, postbomb, cc.dir)
  if(BCAD)
    Cc[,1] <- 1950 - Cc[,1]

  # warn/stop if the date lies (partly) beyond the calibration curve
  if(edge) {
    border <- 0
    if(age-2*error < min(Cc[,2]-2*Cc[,3]))
      if(age+2*error > min(Cc[,2]+2*Cc[,3]))
        border <- 1 else border <- 2
    if(age+2*error > max(Cc[,2]-2*Cc[,3]))
      if(age-2*error < min(Cc[,2]+2*Cc[,3]))
        border <- 1 else border <- 2
    if(border == 1)
      message("Date falls partly beyond calibration curve and will be truncated!")
    if(border == 2)
      stop("Cannot calibrate dates beyond calibration curve!")
  }

  # calculate the raw and calibrated distributions
  C14.dist <- caldist(age, error, cc=0, BCAD=FALSE) # just to draw a normal dist
  C14.hpd <- hpd(C14.dist, return.raw=TRUE)[[1]]
  C14.hpd <- C14.hpd[which(C14.hpd[,3] == 1),1:2] # extract only the values within the hpd
  C14.hpd <- rbind(c(C14.hpd[1,1],0), C14.hpd, c(C14.hpd[nrow(C14.hpd),1],0))
  dat <- caldist(age, error, cc=cc, yrsteps=yr.steps, threshold=threshold, 
    normal=normal, t.a=t.a, t.b=t.b, BCAD=FALSE, postbomb=postbomb, cc.dir=cc.dir)
  if(BCAD)
    dat <- caldist(age, error, cc=cc, yrsteps=yr.steps, threshold=threshold, 
  normal=normal, t.a=t.a, t.b=t.b, BCAD=TRUE, postbomb=postbomb, cc.dir=cc.dir) 

  cal.hpd <- hpd(dat, prob=prob, return.raw=TRUE, rounded=rounded)
  hpds <- cal.hpd[[2]]
  cal.hpd <- cal.hpd[[1]]
  cal.dist <- cal.hpd[,1:2]

  # copy entries at edges of calibrated hpds, to ensure rectangular polygons
  if(BCAD) {
      after <- rbind(cal.hpd[which(diff(c(0,cal.hpd[,3])) == 1),])
      before <- rbind(cal.hpd[which(diff(c(cal.hpd[,3])) == -1),])
  } else {
    before <- rbind(cal.hpd[which(diff(c(0,cal.hpd[,3])) == 1),])
    after <- rbind(cal.hpd[which(diff(c(cal.hpd[,3])) == -1),])
  }
  if(length(before) > 2) {
    before[,3] <- 0 # and set their hpds to 0
    cal.hpd <- rbind(before, cal.hpd)
    cal.hpd <- cal.hpd[order(cal.hpd[,1]),]
  }
  if(length(after) > 2) {
    after[,3] <- 0
    cal.hpd <- rbind(cal.hpd, after)
    cal.hpd <- cal.hpd[order(cal.hpd[,1]),]
  }

  # deal with drawing ages truncated by the calibration curve
  cal.hpd[which(cal.hpd[,3] == 0),2] <- 0 # outside hpd ranges
  if(cal.hpd[nrow(cal.hpd),1] == Cc[nrow(Cc),1])
    cal.hpd <- rbind(cal.hpd, c(cal.hpd[nrow(cal.hpd),1],0,0))
  if(cal.hpd[1,1] == Cc[1,1])
    cal.hpd <- rbind(c(cal.hpd[1,1],0,1), cal.hpd)
  if(cal.dist[nrow(cal.dist),1] == Cc[nrow(Cc),1])
    cal.dist <- rbind(cal.dist, c(cal.dist[nrow(cal.dist),1],0))
  cal.dist <- rbind(c(cal.dist[1,1],0), cal.dist)
  cal.dist <- rbind(cal.dist, c(cal.dist[nrow(cal.dist),1],0))

  # calculate limits
  if(length(cal.lim) == 0) {
    cal.lim <- range(dat[,1])
  lims <- cal.lim
  cal.lim <- rev(extendrange(cal.lim, f=extend.range))
  if(BCAD)
    cal.lim <- rev(cal.lim)
  if(cal.rev)
    cal.lim <- rev(cal.lim)
  } 

  if(length(C14.lim) == 0) {
    if(BCAD) {
      cc.min <- max(1, min(which(Cc[,1] <= max(cal.lim))))
      cc.max <- min(nrow(Cc), max(which(Cc[,1] >= min(cal.lim))))
    } else {  
        cc.min <- max(1, min(which(Cc[,1] >= min(cal.lim))))
        cc.max <- min(nrow(Cc), max(which(Cc[,1] <= max(cal.lim))))
      }
    # we don't need the entire calibration curve
    Cc <- Cc[cc.min:cc.max,]
    if(BCAD)
      cc.lim <- extendrange(c(Cc[,2]-Cc[,3], Cc[,2]+Cc[,3]), f=extend.range) else
        cc.lim <- extendrange(c(Cc[,2]-Cc[,3], Cc[,2]+Cc[,3], C14.dist[,1]), f=extend.range)
  } else {
     cc.min <- max(1, which(Cc[,2] >= min(C14.lim)))
     cc.max <- min(nrow(Cc), which(Cc[,2] <= max(C14.lim)))
     Cc <- Cc[cc.min:cc.max,]
     cc.lim <- range(C14.lim)
  }
  ccpol <- cbind(c(Cc[,1], rev(Cc[,1])), c(Cc[,2]-Cc[,3], rev(Cc[,2]+Cc[,3])))

  # transpose the probability distributions onto the age-scales
  prob2age <- function(prob, age1, age2, prob1=min(prob[,2]), prob2=max(prob[,2]), bcad=BCAD) {
        a <- (age2 - age1) / (prob2 - prob1)
        b <- age1 + (a * prob1)
        return(cbind(prob[,1], b + dist.height*a*prob[,2]))
  }
  if(length(dist.float) == 1)
    dist.float[2] <- dist.float[1] 
  callim <- cal.lim[1]+dist.float[2]*(cal.lim[2]-cal.lim[1])
  cclim <- cc.lim[1]+dist.float[1]*(cc.lim[2]-cc.lim[1])
  
  tC14.dist <- prob2age(C14.dist, callim, cal.lim[2])
  tC14.hpd <- prob2age(C14.hpd, callim, cal.lim[2])
  tcal.dist <- prob2age(cal.dist, cclim, cc.lim[2])
  tcal.hpd <- prob2age(cal.hpd, cclim, cc.lim[2])

  # adapt axis titles, labels and hpds if BCAD and/or ka
  if(length(cal.lab) == 0)
    if(ka) 
      cal.lab <- ifelse(BCAD, "k BC/AD", "kcal BP") else 
        cal.lab <- ifelse(BCAD, "BC/AD", "cal BP")
  if(length(C14.lab) == 0)
    C14.lab <- ifelse(ka, expression(""^14*C~kBP), expression(""^14*C~BP))
  xaxt <- ifelse(BCAD || ka, "n", "s")
  yaxt <- ifelse(ka, "n", "s")

  plot(0, type="n", xlim=cal.lim, ylim=cc.lim, xlab=cal.lab, ylab=C14.lab, xaxt="n", yaxt="n", xaxs=xaxs, yaxs=yaxs, bty=bty, mgp=mgp, mar=mar)
  if(ka) {
    axis(1, pretty(cal.lim), labels=pretty(cal.lim/1e3))
    axis(2, pretty(cc.lim), labels=pretty(cc.lim/1e3))
    cal.dist[,1] <- cal.dist[,1]/1e3
    hpds[,1:2] <- hpds[,1:2]/1e3
  } else {
     axis(1)
     axis(2)
  }

  # draw the data
  polygon(ccpol, border=cc.col, col=cc.fill)
  polygon(tC14.dist[,2:1], border=dist.col, col=dist.fill)
  polygon(tC14.hpd[,2:1], border=NA, col=hpd.fill)
  polygon(tcal.dist, border=dist.col, col=dist.fill)
  polygon(tcal.hpd, border=dist.col, col=hpd.fill)
  dot <- ifelse(cal.rev, min(lims), callim)
  points(dot, age, col=date.col, pch=20)
  segments(dot, age-error, dot, age+error, col=date.col)

  # legends
  if(cc == 1)
    cc <- "IntCal20 " else
    if(cc == 2)
      cc <- "Marine20 " else
        if(cc == 3)
          cc <- "SHCal20 "
  legend(legend1.loc, legend=c(cc, paste(age, "\u00B1", error)), text.col=c(cc.col, 1),  ncol=1, bty="n", cex=legend.cex)
  legend(legend2.loc, legend=rbind(c("from", "to", "%"), cbind(hpds)), ncol=3, bty="n", cex=legend.cex)

  invisible(list(cal.dist, hpds))
}
