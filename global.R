# avoid breaks in R-output print 
options(width = 125)
options(shiny.maxRequestSize=100*1024^2) # max upload size to 100 Mo

options(repos = "http://cran.rstudio.com/")
libs <- c("shiny", "pls", "caret", "tools", "ggplot2", "psych","data.table","foreach","R.matlab","rJava",
	        "gridExtra", "reshape2", "plyr", "markdown", "R.utils","kernlab","prospectr","xlsx",
          "lattice","hexbin","RColorBrewer","randomForest","Cubist","earth","kernlab","Rcpp","RcppArmadillo")

available <- suppressWarnings(suppressPackageStartupMessages(sapply(libs, require, character.only=TRUE)))

inst.libs <- libs[available == FALSE]
if(length(inst.libs) != 0) {
	install.packages(inst.libs, dependencies = TRUE)
	suppressWarnings(suppressPackageStartupMessages(sapply(inst.libs, require, character.only=TRUE)))
}


# Our datasets can change over time (i.e. the changedata function). Therefore,
# these need to be reactive values; otherwise, the other reactive functions
# and outputs that depend on these datasets won't know that they are changed.

# Note that we never get or assign the "original" copies 
# This way, all user sessions are independent from each other 
library(pls);library(prospectr)
data(NIRsoil)
data(yarn)
# NIRsoil <- data.frame(NIRsoil,NIRsoil$spc,check.names=F)
NIRsoil$cc <- cut(NIRsoil$Ciso,5)
values <- reactiveValues()
values$NIRsoil <- NIRsoil
values$yarn <- yarn

datasets <- c("NIRsoil", "yarn")
lastLoaded <- "" 		