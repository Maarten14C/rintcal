# IntCal 0.2.4
* now includes the updated postbomb curves, published by Hua et al. 2021
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
* added clam's function calBP.14C to find IntCal C14 ages corresponding to cal BP ages. 
* added function glue.ccurves for gluing prebomb and postbomb calibration curves.
* added a warning/error to calibrate() for dates truncated by or outside the calibration curve
* updated the vignettes

# IntCal 0.2.1
* added new function list.curves
* Shortened the functions copyCalibrationCurve and draw.CalibrationCurves to the shorter and more consistent ccurve and draw.ccurve
* ccurve is now more flexible with the names of the calibration curves
* draw.ccurve can now plot another curve on its own righthand axis scale (handy when mixing pre- and postbomb curves).
* new functions calibrate, caldist and hpd, copied and modified from the clam R package

# IntCal 0.2.0
* added a function intcal.data to plot the data underlying the IntCal curves.
* copyCalibrationCurve can now copy more of the curves.
* new function draw.calibration curve to draw the calibration curves (1 or 2), or to add curves to an existing plot)

# IntCal 0.1.3
* fixed issues with the DESCRIPTION file

# IntCal 0.1.2
* Added Rd files 
* Added some of the most relevant calibrated-related functions of the rbacon package (so that these can in time be deleted from that and other packages)

# IntCal 0.1.1
* fixed some errors in the description file

# IntCal 0.1.0
* first iteration of this R data package (originally put on github)
