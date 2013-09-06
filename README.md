# ShinyLight - Chemometrics with Shiny

ShinyLight is a chemometric toolbox in the form of a desktop or web app based on the [R software](http://cran.r-project.org/) and [Shiny](http://www.rstudio.com/shiny/) package that allows to perform analyses of spectrosopic data. It features most of the usual steps in chemometric analysis:

  * Data I/O
  * Data transform and vizualisation
  * Spectral pre-processing
  * Exploratory analysis (PCA and cluster analysis) 
  * Outlier detection (TODO!)
  * Selection of training/test samples for multivariate calibration
  * Multivariate calibration (based on the [caret](http://cran.r-project.org/web/packages/caret/index.html) package) 
  * Prediction of new observations

## Motivation

Initially I just wanted to develop a demonstrator of the capabilities of the [prospectr package](http://antoinestevens.github.io/prospectr/) of which I am maintainer. Shiny allows to port R code into a web app than a user can easily interact with, play with parameters and quickly see the result. Finally, with a little bit of work I decided to add exploratory analyses and multivariate calibration tools to propose a fully featured toolbox for chemometric analyses (there are still things to do though). Analyzing spectroscopic data involves the use of complex statistical tools and requires high-performance computing facilities to store, process and analyze very large datasets. What is nice with Shiny is that you can potentially host your app in a server well equipped in memory and cpu and the user needs only an internet access and a standard web browser. New algorithms and development in chemometrics that appear in the R software can also be easily added to the toolbox and be quickly available to users with limited knowledge in programming.

## How to install the app locally

The following softwares are required to run the app:

  * [R](http://cran.r-project.org/) 3.0.1
  * [Shiny](http://cran.r-project.org/web/packages/shiny/index.html) R package 0.7
  * A bunch of R packages that will be downloaded and installed automatically when running the app:
      + I/O: R.matlab, xlsx
      + graphics: ggplot2, lattice, hexbin, RColorBrewer, gridExtra
      + multivariate analysis: pls, caret, kernlab, randomForest, Cubist, earth, prospectr
      + data reshape and summary: data.table, psych, reshape2, plyr
      + misc: tools, markdown, R.utils, foreach, Rcpp, RcppArmadillo
  * An internet browser (IE is not supported)

Start R and run the following code:

```
install.packages('shiny')
library(shiny)
runGitHub('antoinestevens','shinylight')
```

This will download the app from [GitHub](https://github.com/antoinestevens/shinylight/archive/master.zip) in a temporary folder and open it in the default browser. The app can be also downloaded in a user-defined folder (for later use) and run with following piece of code:

```
filePath <- "path_to_download_file_to"
download("https://github.com/antoinestevens/shinylight/archive/master.zip",filePath)
dirName <- "path_to_directory_to_extract_files_to" # Will be created if necessary
unzip(filePath, exdir = dirName)
install.packages('shiny') # Only if not installed yet
library(shiny)
runApp(dirName)        
```

A Shiny web application can be also deployed over the web using a Linux server. See [here](https://github.com/rstudio/shiny-server) so that a user would need only a web browser to play with the app.

## BUG REPORTS

The app is in beta stage and I guess full of bugs. The preferred way to report bugs is by opening an [issue on the GitHub repo](https://github.com/antoinestevens/shinylight/issues).

## LICENCE

The general layout, data view, transform and vizualisations panels are greatly inspired (if not copy-pasted) from the [radyant marketing app](https://github.com/mostly-harmless/radyant).

ShinyLight is free and open source and licensed under GPL-v3.