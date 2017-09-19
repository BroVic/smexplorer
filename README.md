
<!-- README.md is generated from README.Rmd. Please edit that file -->
SMExplorer [![Travis-CI Build Status](https://travis-ci.org/BroVic/SMExplorer.svg?branch=master)](https://travis-ci.org/BroVic/SMExplorer)
==========================================================================================================================================

*An R package for the exploratory analysis of social media data*

Installation
------------

This is accomplished by running these 2 lines in the R console. If you need to install R, get it [**here**](https://cloud.r-project.org):

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("NESREA/SMExplorer")
```

Note the following:

-   First-time installion of `SMExplorer` on a **shiny, brand-new** R platform takes time, due to concomitant installation of several dependencies.
-   If installation breaks off due to network problems, rerun `devtools::install_github("NESREA/SMExplorer")`
-   You need [Java Runtime Environment](http://www.java.com/download) (v. 1.8 or greater) to use `SMExplorer`. Run `java -version` in the shell (e.g. *Command Prompt* for Windows, *Terminal* for OS X) to check.

Upon installing `SMExplorer`, further instructions for its use can be obtained with

``` r
vignette("smexplorer")
```

[**Email**](mailto:victor.ordu@nesrea.gov.ng)
