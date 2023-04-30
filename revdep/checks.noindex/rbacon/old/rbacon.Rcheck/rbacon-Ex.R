pkgname <- "rbacon"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('rbacon')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("AgesOfEvents")
### * AgesOfEvents

flush(stderr()); flush(stdout())

### Name: AgesOfEvents
### Title: Event probabilities against calendar age
### Aliases: AgesOfEvents

### ** Examples




cleanEx()
nameEx("Bacon.Age.d")
### * Bacon.Age.d

flush(stderr()); flush(stdout())

### Name: Bacon.Age.d
### Title: Output all ages for a single depth.
### Aliases: Bacon.Age.d

### ** Examples

## Not run: 
##D   Bacon(run=FALSE, coredir=tempfile())
##D   agedepth(age.res=50, d.res=50, d.by=10)
##D   ages.d20 = Bacon.Age.d(20)
##D   mean(ages.d20)
## End(Not run)



cleanEx()
nameEx("Bacon")
### * Bacon

flush(stderr()); flush(stdout())

### Name: Bacon
### Title: Main age-depth modelling function
### Aliases: Bacon

### ** Examples

## Don't show: 
  Bacon(run=FALSE, coredir=tempfile())
## End(Don't show)



cleanEx()
nameEx("Bacon.cleanup")
### * Bacon.cleanup

flush(stderr()); flush(stdout())

### Name: Bacon.cleanup
### Title: Remove files made to produce the current core's age-depth model.
### Aliases: Bacon.cleanup

### ** Examples

  Bacon(run=FALSE, coredir=tempfile())
  Bacon.cleanup()



cleanEx()
nameEx("Bacon.d.Age")
### * Bacon.d.Age

flush(stderr()); flush(stdout())

### Name: Bacon.d.Age
### Title: Output all depths for a single age.
### Aliases: Bacon.d.Age

### ** Examples

## Not run: 
##D   Bacon(run=FALSE, coredir=tempfile())
##D   agedepth(age.res=50, d.res=50, d.by=10)
##D   ages.d20 = Bacon.Age.d(20)
##D   mean(ages.d20)
## End(Not run)



cleanEx()
nameEx("Bacon.hist")
### * Bacon.hist

flush(stderr()); flush(stdout())

### Name: Bacon.hist
### Title: Calculate age distributions of depths.
### Aliases: Bacon.hist

### ** Examples

## Not run: 
##D   Bacon(run=FALSE, coredir=tempfile())
##D   agedepth(age.res=50, d.res=50, d.by=10)
##D   Bacon.hist(20)
##D   Bacon.hist(20:30)
## End(Not run)



cleanEx()
nameEx("Baconvergence")
### * Baconvergence

flush(stderr()); flush(stdout())

### Name: Baconvergence
### Title: Test to identify poorly mixed MCMC runs.
### Aliases: Baconvergence

### ** Examples




cleanEx()
nameEx("accrate.age")
### * accrate.age

flush(stderr()); flush(stdout())

### Name: accrate.age
### Title: Obtain estimated accumulation rates for any age of a core.
### Aliases: accrate.age

### ** Examples

## Not run: 
##D   Bacon(run=FALSE, coredir=tempfile())
##D   agedepth(yr.res=50, d.res=50, d.by=10)
##D   accrate.a5000 = accrate.age(5000)
##D   plot(accrate.a5000, pch='.')
##D   hist(accrate.a5000)
## End(Not run)



cleanEx()
nameEx("accrate.age.ghost")
### * accrate.age.ghost

flush(stderr()); flush(stdout())

### Name: accrate.age.ghost
### Title: Plot a core's accumulation rates against calendar time.
### Aliases: accrate.age.ghost

### ** Examples

## Not run: 
##D   Bacon(run=FALSE, coredir=tempfile())
##D   agedepth(age.res=20, d.res=20, d.by=10)
##D   layout(1)
##D   tmp <- accrate.age.ghost(age.res=200, acc.res=100)
##D   head(tmp)
## End(Not run)



cleanEx()
nameEx("accrate.depth")
### * accrate.depth

flush(stderr()); flush(stdout())

### Name: accrate.depth
### Title: Obtain estimated accumulation rates as for any depth of a core.
### Aliases: accrate.depth

### ** Examples

## Not run: 
##D   Bacon(run=FALSE, coredir=tempfile())
##D   agedepth(yr.res=50, d.res=50, d.by=10)
##D   d20 <- accrate.depth(20)
##D   hist(d20)
##D   d20 <- accrate.depth(20, cmyr=TRUE) # to calculate accumulation rates in cm/yr
##D   mean(d20)
## End(Not run)



cleanEx()
nameEx("accrate.depth.ghost")
### * accrate.depth.ghost

flush(stderr()); flush(stdout())

### Name: accrate.depth.ghost
### Title: Plot modelled accumulation rates against the depths of a core.
### Aliases: accrate.depth.ghost

### ** Examples

## Not run: 
##D   Bacon(run=FALSE, coredir=tempfile())
##D   agedepth(yr.res=50, d.res=50, d.by=10)
##D   layout(1)
##D   tmp <- accrate.depth.ghost()
##D   head(tmp)
## End(Not run)



cleanEx()
nameEx("add.dates")
### * add.dates

flush(stderr()); flush(stdout())

### Name: add.dates
### Title: Add dates to age-depth plots
### Aliases: add.dates

### ** Examples




cleanEx()
nameEx("agedepth")
### * agedepth

flush(stderr()); flush(stdout())

### Name: agedepth
### Title: Plot an age-depth model
### Aliases: agedepth

### ** Examples

## Don't show: 
  Bacon(run=FALSE, ask=FALSE, coredir=tempfile())
  agedepth(yr.res=50, d.res=50, d.by=10)
 
## End(Don't show)



cleanEx()
nameEx("agemodel.it")
### * agemodel.it

flush(stderr()); flush(stdout())

### Name: agemodel.it
### Title: Extract one age-model iteration
### Aliases: agemodel.it

### ** Examples

## Not run: 
##D   Bacon(run=FALSE, coredir=tempfile())
##D   agedepth(age.res=50, d.res=50, d.by=10)
##D   lines(agemodel.it(5), col="red")
## End(Not run)



cleanEx()
nameEx("bacon2clam")
### * bacon2clam

flush(stderr()); flush(stdout())

### Name: bacon2clam
### Title: Translate Bacon .csv files to clam .csv files.
### Aliases: bacon2clam

### ** Examples

{
}



cleanEx()
nameEx("calib.plot")
### * calib.plot

flush(stderr()); flush(stdout())

### Name: calib.plot
### Title: Plot the dates
### Aliases: calib.plot

### ** Examples

  Bacon(run=FALSE, coredir=tempfile())
  calib.plot()



cleanEx()
nameEx("flux.age.ghost")
### * flux.age.ghost

flush(stderr()); flush(stdout())

### Name: flux.age.ghost
### Title: Plot flux rates for proxies.
### Aliases: flux.age.ghost

### ** Examples

## Not run: 
##D   Bacon(run=FALSE, coredir=tempfile())
##D   agedepth(yr.res=50)
##D   flux.age.ghost(1)
## End(Not run)



cleanEx()
nameEx("proxy.ghost")
### * proxy.ghost

flush(stderr()); flush(stdout())

### Name: proxy.ghost
### Title: Proxies analysed along the depths of a core can be plotted as
###   'proxy-ghost' graphs against calendar time while taking into account
###   chronological uncertainties. Here darker grey indicates more likely
###   calendar ages for specific proxy values.
### Aliases: proxy.ghost

### ** Examples




cleanEx()
nameEx("scissors")
### * scissors

flush(stderr()); flush(stdout())

### Name: scissors
### Title: Remove the first n iterations.
### Aliases: scissors

### ** Examples

## Don't show: 
  Bacon(run=FALSE, coredir=tempfile())
  scissors(100)
  agedepth(d.res=50, age.res=50, d.by=10)
## End(Don't show)
## Not run: 
##D   Bacon(ask=FALSE, coredir=tempfile())
##D   scissors(100)
##D   agedepth()
## End(Not run)




cleanEx()
nameEx("thinner")
### * thinner

flush(stderr()); flush(stdout())

### Name: thinner
### Title: Thin iterations.
### Aliases: thinner

### ** Examples

## Don't show: 
  Bacon(run=FALSE, coredir=tempfile())
  thinner(.1)
  agedepth(d.res=50, age.res=50, d.by=10)
## End(Don't show)
## Not run: 
##D   Bacon(ask=FALSE, coredir=tempfile())
##D   thinner(.2)
##D   agedepth()
## End(Not run)




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
