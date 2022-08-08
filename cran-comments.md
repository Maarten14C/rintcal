## Test environments
* local Fedora install, R 4.1.3
* rhub win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 Notes

* The note (on rhub's win_devel) related to possibly misspelled words. I can confirm that these words are not misspelled.

* I'd like to change the package name from IntCal to rintcal please, so that it becomes clearer that this package is not officially affiliated with the IntCal Working Group (IWG). IWG has made their data available freely on intcal.org and this package aims to provide these data in a readable format, and to provide these data to other R packages, thus requiring less replication and updating of data. 

* If CRAN accepts the name change, I will then proceed with updating the packages with reverse dependencies to the IntCal package (rbacon, clam, coffee) accordingly.
