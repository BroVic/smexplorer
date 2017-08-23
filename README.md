
<!-- README.md is generated from README.Rmd. Please edit that file -->
SMExplorer
==========

*An R package for the exploratory analysis of social media metrics*  
[![Travis-CI Build Status](https://travis-ci.org/BroVic/SMExplorer.svg?branch=master)](https://travis-ci.org/BroVic/SMExplorer)

> SMExplorer is currently in beta

Installation instructions:
--------------------------

To get started do the following:

### 1. Install R

R is tailored for statistical computing. Visit <https://cloud.r-project.org> to get the current version, if you don't already have it. This application depends on version 3.3.3 and above.

### 2. Install SMExplorer

R can automate this process for you or you can clone this repository to build it yourself.

#### 2.1 From inside R (easy method)

1.  Install the *devtools* package

2.  Install *SMExplorer*

This is accomplished by running 3 lines in the R console:

``` r
install.packages("devtools")
library(devtools)
install_github("NESREA/SMExplorer")
```

*Note*: Installing it with a **brand-new R** takes time, due to installation of dependencies. Feel free to walk away from the computer for a while.

#### 2.2 To build your own copy (for advanced users)

1.  `git clone` this repo (*Git.exe* required).
2.  In the shell, navigate to the folder containing the clone.
3.  Run `R CMD build SMExplorer`
4.  If successful, run `R CMD INSTALL SMExplorer_x.x.x.9xxx.tar.gz`.

### 3. Other system requirements

Java Runtime Environment (version 1.8 or greater) is required. If not sure whether you have it, run `java -version` in the shell. If needed, download and install from <http://www.java.com/download>.

### 4. Run the application  
#### 4.1 Launch *SMExplorer*
The app currently uses only one function, `explore()`. To lauch in the default browser, run the following lines:

``` r
library(SMExplorer)
explore()
```

#### 4.2 Login

On first use, you ***will*** need to supply the in-built OAuth credentials. Follow these steps:
+ Click on *"Register new session"*
+ Go back to R and enter "1" (i.e. for "*YES*")
+ Return to the application window to continue use.

#### 4.3 Make an entry

You will to see a second error message saying *"Bad HTTP"*. This means you have not supplied a search term (see figure below).

![](error-badrequest.PNG)
To begin your analysis, make an entry in the *Search* field and click on **Go!**.

**Note:** If you get error messages like *"Timeout"* or *"...set\_up\_twitter\_oauth..."* it is due to a poor network connection; just click **"Go"** again until it works.

Please report issues **[here](https://github.com/NESREA/SMExplorer/issues)** or send us an [email](mailto:victor.ordu@nesrea.gov.ng).
