## ----style, eval=TRUE, echo=FALSE, results='asis'--------------------------
BiocStyle::latex()

## ----FileDemo--------------------------------------------------------------
library(RSQLite)
library(AnnotationForge)
read.table(system.file("extdata", "hcg110_ID",
                       package="AnnotationDbi"),
           sep = "\t", header = FALSE, as.is = TRUE)[1:5,]

## ----availableDB0s---------------------------------------------------------
  available.db0pkgs()  

## ----GetIntermedDB, eval=FALSE---------------------------------------------
#  source("http://bioconductor.org/biocLite.R")
#  biocLite("human.db0")

## ----checkforhumandb0, echo=FALSE------------------------------------------
require("human.db0")

## ----GetOrg.db, eval=FALSE-------------------------------------------------
#  biocLite("org.Hs.eg.db")

## ----list Schemas----------------------------------------------------------
available.dbschemas()

## ----SQLForge, tidy=FALSE, results='hide'----------------------------------
hcg110_IDs = system.file("extdata",
                          "hcg110_ID",
                          package="AnnotationDbi")

tmpout = tempdir()

makeDBPackage("HUMANCHIP_DB",
              affy=FALSE,            
              prefix="hcg110",
              fileName=hcg110_IDs,
              baseMapType="gb",
              outputDir = tmpout,
              version="1.0.0",
              manufacturer = "Affymetrix",
              chipName = "Human Cancer G110 Array",
              manufacturerUrl = "http://www.affymetrix.com")

## ----cleanup2, echo=FALSE, results='hide'----------------------------------
file.remove(file.path(tmpout, "hcg110.sqlite"))
file.rename(file.path(tmpout, "hcg110.db"),file.path(tmpout, "foo.db"))

## ----install, eval=FALSE---------------------------------------------------
#  install.packages("packageNameAndPath", repos=NULL, type="source")

## ----createSimpleMapping, tidy=FALSE---------------------------------------
library(hgu95av2.db)
hgu95av2NAMESYMBOL <- createSimpleBimap("gene_info",
                                        "gene_name",
                                        "symbol",
                                        hgu95av2.db:::datacache,
                                        "NAMESYMBOL",
                                        "hgu95av2.db")
##What is the mapping we just made?
hgu95av2NAMESYMBOL
##Display the 1st 4 relationships in this new mapping
as.list(hgu95av2NAMESYMBOL)[1:4]

## ----SessionInfo, echo=FALSE-----------------------------------------------
sessionInfo()

