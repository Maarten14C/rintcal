# rintcal 0.4.3
* added a function point.estimates to calculate the mode, median, weighted mean and midpoint of a calibrated distribution (after a suggestion by Alexandra Rouillard)
* reading and writing of large files should now go faster if the data.table R package is installed (which doesn't seem to work well on some Macs)
* added functions F14C.D14C, D14C.F14C, age.F14C, F14C.age and contaminate
* draw.ccurve now adds a legend if more than one calibration curve is plotted
* draw.dates should behave a bit faster

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
