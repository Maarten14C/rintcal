## Test environments
* local Fedora install, R 4.4.1
* rhub win-builder (devel and release)

## R CMD check results

0 Errors | 0 Warnings | 1 Note

The note about UTF-8 strings relates to international characters within data/intcal.rda. This is a json object with a number of international characters for, e.g., sites and author names, which would best be kept as-is in order to enable users to cite sites/papers correctly.
