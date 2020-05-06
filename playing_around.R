setwd('/srv/shiny-server/david/')
.libPaths("/srv/shiny-server/david/libs")
library(shiny)
library(xlsx)
library(RDAVIDWebService)

options(java.parameters = "-Xmx1000m")


target.genes = read.xlsx('david_example_mut3_both_up_down.geneIDs.xlsx', sheetIndex = 1)
target.genes = target.genes[['target_genes']]

target.genes <-target.genes[!is.na(target.genes)]

getcats = c('GOTERM_BP_FAT')


david<-DAVIDWebService$new(email='franziska.metge@age.mpg.de',
                           url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")

categories<-getcats

setAnnotationCategories(david, categories)

