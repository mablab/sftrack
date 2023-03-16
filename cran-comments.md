## Resubmission

This is a resubmission. Relevant change in this version:

  * Fixed update of `sf` with regards to CRS.
  
This solves the issue mentioned by CRAN maintainers on 2023-03-08 and
2023-03-12.


## Test environments

* local: Ubuntu 22.04.2 LTS, R version 4.2.2 Patched (2022-11-10 r83330)
* GitHub Actions: windows-latest (R-release), macOS-latest (R-release), and
  ubuntu-latest (R-release, R-oldrelease, and R-devel)
* win-builder: R-release (R-4.2.2), R-oldrelease (R-4.1.3), R-devel (to be
  R-4.3.0)


## R CMD check results

* local: 

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


* win-builder (R-release):

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* win-builder (R-oldrelease):

0 errors ✔ | 0 warnings ✔ | 1 note ×

- checking CRAN incoming feasibility ... NOTE
Maintainer: 'Mathieu Basille <mathieu@basille.org>'

Possibly mis-spelled words in DESCRIPTION:
  Calenge (34:46)
  Turchin (34:5)
  al (34:57)
  et (34:54)

--> These are all fine words (including authors' names).


## Downstream dependencies

There are currently no downstream dependencies for this package.
