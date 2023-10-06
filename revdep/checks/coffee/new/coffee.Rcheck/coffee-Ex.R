pkgname <- "coffee"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('coffee')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("draw.rings")
### * draw.rings

flush(stderr()); flush(stdout())

### Name: draw.rings
### Title: plot the dates and model of a wiggle-match dated tree
### Aliases: draw.rings

### ** Examples

  treedir <- tempdir()
  rings("Ulandryk4", tree.dir=treedir, draw=FALSE)
  draw.rings("Ulandryk4", tree.dir=treedir)



cleanEx()
nameEx("rings")
### * rings

flush(stderr()); flush(stdout())

### Name: rings
### Title: wiggle-match C-14 dating of a tree
### Aliases: rings

### ** Examples

  rings("Ulandryk4", tree.dir=tempdir())
  rings("mytree", tree.dir=tempdir())



cleanEx()
nameEx("sim.rings")
### * sim.rings

flush(stderr()); flush(stdout())

### Name: sim.rings
### Title: Simulate the radiocarbon dating of tree-rings
### Aliases: sim.rings

### ** Examples

  treedir <- tempdir()
  sim.rings("manyrings", age.start=1000, length=400, gaps=10, tree.dir=treedir)
  rings("manyrings", tree.dir=treedir)



cleanEx()
nameEx("sim.strat")
### * sim.strat

flush(stderr()); flush(stdout())

### Name: sim.strat
### Title: Simulate the radiocarbon dating of random depths of a sediment
###   which has accumulated over time.
### Aliases: sim.strat

### ** Examples

  stratdir <- tempdir()
  sim.strat("ordered.mud", age.min=1000, length=5000, n=10, strat.dir=stratdir)



cleanEx()
nameEx("strat")
### * strat

flush(stderr()); flush(stdout())

### Name: strat
### Title: Model chronologically ordered dates
### Aliases: strat

### ** Examples

## Not run: 
##D tmp <- tempdir()
##D strat(, strat.dir=tmp, its=1000, thinning=1, internal.thinning=1)
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
