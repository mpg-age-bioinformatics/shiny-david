## ----style, eval=TRUE, echo=FALSE, results='asis'--------------------------
BiocStyle::latex()

## ----startup,results='hide'------------------------------------------------
library("AnnotationForge")

## ----makeprobepackage------------------------------------------------------
filename <- system.file("extdata", "HG-U95Av2_probe_tab.gz", 
                          package="AnnotationForge")
outdir   <- tempdir()
me       <- "Wolfgang Huber <w.huber@dkfz.de>"
species  <- "Homo_sapiens"
makeProbePackage("HG-U95Av2",
                 datafile   = gzfile(filename, open="r"),
                 outdir     = outdir,
                 maintainer = me,
                 species    = species,
                 version    = "0.0.1")
dir(outdir)

