.libPaths("/srv/shiny-server/david/libs")
library(shiny)

if(!require(RDAVIDWebService)){
  source("http://bioconductor.org/biocLite.R")
  biocLite("RDAVIDWebService")
  library(RDAVIDWebService)
}

if(!require(xlsx)){
  install.packages("xlsx", dependencies = TRUE)
  library(xlsx)
}

quit(save="no")