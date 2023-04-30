pkgname <- "clam"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('clam')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add.dates")
### * add.dates

flush(stderr()); flush(stdout())

### Name: add.dates
### Title: Add dates to age-depth plots
### Aliases: add.dates

### ** Examples

  clam(coredir=tempfile())
  add.dates(5000, 100, 60)



cleanEx()
nameEx("calib.t")
### * calib.t

flush(stderr()); flush(stdout())

### Name: calib.t
### Title: Comparison dates calibrated using both the t distribution
###   (Christen and Perez 2009) and the normal distribution.
### Aliases: calib.t

### ** Examples

calib.t() 




cleanEx()
nameEx("clam")
### * clam

flush(stderr()); flush(stdout())

### Name: clam
### Title: clam
### Aliases: clam

### ** Examples

 clam(, coredir=tempdir()) # Create the example in Cores/Example folder
 clam(, coredir=tempdir(), extradates=470) 




cleanEx()
nameEx("deptime.age")
### * deptime.age

flush(stderr()); flush(stdout())

### Name: deptime.age
### Title: Calculates the slope of a straight curve at the desired age.
### Aliases: deptime.age

### ** Examples

  clam(coredir=tempdir(), storedat=TRUE)
  dp <- deptime.age(5000)
  summary(dp)
  deptime.age(5000, yrcm=FALSE) # to calculate sedimentation times in cm/yr, so accumulation rates



cleanEx()
nameEx("deptime.depth")
### * deptime.depth

flush(stderr()); flush(stdout())

### Name: deptime.depth
### Title: Calculates *for each iteration* the slope of a straight curve
###   between depths just above and below the desired point.
### Aliases: deptime.depth

### ** Examples

  clam(coredir=tempdir(), storedat=TRUE) 
  dp <- deptime.depth(20)
  summary(dp)
  deptime.depth(20, FALSE) # to calculate accumulation rates in cm/yr




cleanEx()
nameEx("plot_proxies")
### * plot_proxies

flush(stderr()); flush(stdout())

### Name: plot_proxies
### Title: Produce a plot of proxy values against calendar age.
### Aliases: plot_proxies

### ** Examples

clam(coredir=tempdir(), proxies=TRUE)
plot_proxies(3)
plot_proxies(3, revyr=FALSE)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
