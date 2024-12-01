# rintcal 1.1.1
* added an option as.F to `ccurve`, to read in curves as F14C instead of as C14. Can only be used using the 'official' intcal20 .14c files
* now reads the 'official' intcal20 .14c files instead of the 3Col_ derivatives
* when using `ccurve`, curves will be returned with cal BP years increasing (the default 14c files return then decreasing)

# rintcal 1.1.0
* updated the glue.ccurves and mix.ccurves functions so that tailor-made curves can be provided
* added NOTCal04 (van der Plicht et al. 2004) as legacy curve. This curve was released 20 years ago as a warning NOT to calibrate older radiocarbon ages, because at the time the datasets were too much in disagreement - however, some did go ahead and used NOTCal04 as a calibration curve...
* added the original plain-text files intcal20.14c, marine20.14c and shcal20.14c. These comma-separated-value files include D14C values (the IntCal20 curves were modelled in D14C space).

# rintcal 1.0.0
* most functions have been transferred into the new R package `rice`. The `rintcal` package will become a data package with only very few functions remaining (related to loading/combining curves, and plotting/querying the intcal data)
* in mix.ccurves, errors are now calculated correctly
* intcal.data can now also plot the C14 data in the 'realms' of F14C, pMC and D14C

# rintcal 0.6.4
* mix.ccurves now doesn't fail if a provided cc.dir folder does not yet exist
* mix.ccurves now saves the calibration curves with values separated by a single space, not by a tab
* as per Kurt Hornik's e-mail, added a sentinal "_PACKAGE" to the documentation
* as per Prof. Brian Ripley's e-mail, removed permille sign of draw.D14C function as graphics devices such as of OSX do not always draw them
* some minor changes to how postbomb dates are treated and plotted

# rintcal 0.6.3
* further enhancements to functions ccurves, calibrate and caldist, especially for young dates close to the prebomb limits
* updated vignette

# rintcal 0.6.2
* a bug in ccurves causing problems with plotting and cc=4 has been corrected

# rintcal 0.6.1
* calibration of dates close to 0 14C BP now uses both prebomb and postbomb curves by default (if postbomb is provided)

# rintcal 0.6.0
* the files containing the NH and SH postbomb curves are now printed with the youngest cal BP years at the top 
* 0 BC/AD (which does not exist) is now dealt with better
* added a function draw.contamination
* the calibrate function now deals better with BC/AD (e.g., new items can be added afterward in the expected locations) 
* mix.ccurves now handles varying offsets (mean and/or uncertainty)
* intcal.data now has more colours, returns datasets (invisibly) and can plot selected datasets

# rintcal 0.5.3
* added a function draw.D14C to draw the D14C data together with the calibration curve.
* added an option draw.base to draw.dates

# rintcal 0.5.2
* the IntCal20 curves and their underpinning data are now available through a json file (code kindly contributed by Christopher Bronk Ramsey, University of Oxford)
* calibrate now plots better when working in BC/AD

# rintcal 0.5.0
* added a function point.estimates to calculate the mode, median, weighted mean and midpoint of a calibrated distribution (after a suggestion by Alexandra Rouillard)
* reading and writing of large files should now go faster if the data.table R package is installed (which doesn't seem to work well on some Macs)
* added functions F14C.D14C, D14C.F14C, age.F14C, F14C.age and contaminate
* draw.ccurve now adds a legend if more than one calibration curve is plotted
* draw.dates should behave a bit faster and draw more consistently sized distributions
* added legacy 'calibration' data from Arnold & Libby 1951 (Science 113, 111-120)
* Calibrated dates of postbomb dates are now drawn better

# rintcal 0.4.2
* made draw.dates more flexible, e.g. can now rotate axes

# rintcal 0.4.1
* updated the documentation to include value entries for each function
* ensured that no data is written into userspace by default, as per CRAN policies
* enhanced functions to set par values more gracefully

# rintcal 0.4.0
* The C14.lim option to specify axis limits in the calibrate() function now works as expected
* renamed the package from IntCal to rintcal, to highlight the fact that this package is not officially affiliated with the IntCal Working Group
* added a function l.calib to calibrate multiple dates or calendar years
* enhanced the options of the t model (Christen and Perez 2016) as alternative for the normal distribution

# IntCal 0.3.1
* added an option ccdir to provide alternative locations of the calibration curves
* added a function new.ccdir(), which copies the package's calibration curves into a specified folder
* C14.lim now works if 0 is included as minimum 14C age

# IntCal 0.3.0
* updated to the updated postbomb curves (now both yearly and monthly), published by Hua et al. 2021
* repaired problems with depths and calheights in draw.dates
* draw.ccurve now plots correct label when BCAD
* c14.lim now estimated correctly when a second curve is added

# IntCal 0.2.3
* draw.ccurve now plots the correct label when using BCAD, and plots depths at the expected heights
* draw.dates now plots multiple dates at the expected heights (with more precise dates peaking higher)
* corrected the Rmd files which had erroneous formatting and some confusing examples

# IntCal 0.2.2
* in mix.curves(), calibration curves are now written to a temporary directory by default, as per CRAN policies
* calibrate function now deals better with postbomb dates
* added clam's function calBP.14C to find IntCal C14 ages corresponding to cal BP ages
* added function glue.ccurves for gluing prebomb and postbomb calibration curves
* added a warning/error to calibrate() for dates truncated by or outside the calibration curve
* updated the vignettes

# IntCal 0.2.1
* added new function list.curves
* Shortened the functions copyCalibrationCurve and draw.CalibrationCurves to the shorter and more consistent ccurve and draw.ccurve
* ccurve is now more flexible with the names of the calibration curves
* draw.ccurve can now plot another curve on its own righthand axis scale (handy when mixing pre- and postbomb curves)
* new functions calibrate, caldist and hpd, copied and modified from the clam R package

# IntCal 0.2.0
* added a function intcal.data to plot the data underlying the IntCal curves
* copyCalibrationCurve can now copy more of the curves
* new function draw.calibration curve to draw the calibration curves (one or two), or to add curves to an existing plot)

# IntCal 0.1.3
* fixed issues with the DESCRIPTION file

# IntCal 0.1.2
* Added Rd files 
* Added some of the most relevant calibrated-related functions of the rbacon package (so that these can in time be deleted from that and other packages)

# IntCal 0.1.1
* fixed some errors in the description file

# IntCal 0.1.0
* first iteration of this R data package (originally put on github)
