### R code from vignette source 'RDavidWS-vignette.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: General R options for Sweave
###################################################
options(prompt="R> ", continue="+  ", width=70, useFancyQuotes=FALSE, digits=4)


###################################################
### code chunk number 2: Loading library
###################################################
suppressMessages(library("RDAVIDWebService"))


###################################################
### code chunk number 3: TermCluster1
###################################################
library("RDAVIDWebService")
fileName<-system.file("files/termClusterReport1.tab.tar.gz",
  package="RDAVIDWebService")
untar(fileName)
termCluster<-DAVIDTermCluster(untar(fileName, list=TRUE))
termCluster
head(summary(termCluster))


###################################################
### code chunk number 4: plot2Dview
###################################################
clustNumber<-2
plot2D(termCluster, clustNumber)


###################################################
### code chunk number 5: plotGO
###################################################
davidGODag<-DAVIDGODag(members(termCluster)[[clustNumber]], 
  pvalueCutoff=0.1, "CC")
plotGOTermGraph(g=goDag(davidGODag),
  r=davidGODag, max.nchar=40, node.shape="ellipse")


###################################################
### code chunk number 6: Session Info
###################################################
sessionInfo()


