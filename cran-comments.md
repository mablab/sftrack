## Resubmission

This is a resubmission. Relevant changes in this version:

  * DESCRIPTION: Removed the space within the doi specification to
    make it clickable. 
  * DESCRIPTION and LICENSE: Removed Calenge from contributors on his
    request.
  * README.md: Added HTTPS to problematic URL.
  * plot_tj.R: Removed all calls to graphics::par that change user's
    options.


## Test environments

* local Debian GNU/Linux 9 (stretch), R 3.3.3
* Travis CI Ubuntu 16.04.6 LTS (xenial), R 4.0.2
* win-builder, R 4.0.3 (release) +  (devel)


## R CMD check results

There were no ERRORs or WARNINGs. 

There was one NOTE (win-builder):

    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Mathieu Basille <basille@ufl.edu>'
    
    New submission
    
    Possibly mis-spelled words in DESCRIPTION:
      Calenge (34:46)
      Turchin (34:5)
      al (34:57)
      et (34:54)

→ This is a resubmission of an unreleased package, with Mathieu
Basille as maintainer  (also all highlighted words are correctly
spelled).


## Downstream dependencies

There are currently no downstream dependencies for this package.
