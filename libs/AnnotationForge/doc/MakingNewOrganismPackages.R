## ----setup, echo=FALSE--------------------------------------------------------
library(knitr)
options(width=80)

## ----wrap-hook, echo=FALSE----------------------------------------------------
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_output(x, options)
})

## ----makeOrgPackageFromNCBI, eval=FALSE---------------------------------------
#  library(AnnotationForge)
#  makeOrgPackageFromNCBI(version = "0.1",
#                         author = "Some One <so@someplace.org>",
#                         maintainer = "Some One <so@someplace.org>",
#                         outputDir = ".",
#                         tax_id = "59729",
#                         genus = "Taeniopygia",
#                         species = "guttata")

## ----makeOrgPackage, eval=FALSE-----------------------------------------------
#  ## Makes an organism package for Zebra Finch data.frames:
#  finchFile <- system.file("extdata","finch_info.txt",
#  		         package="AnnotationForge")
#  finch <- read.table(finchFile,sep="\t")
#  
#  ## Now prepare some data.frames
#  fSym <- finch[,c(2,3,9)]
#  fSym <- fSym[fSym[,2]!="-",]
#  fSym <- fSym[fSym[,3]!="-",]
#  colnames(fSym) <- c("GID","SYMBOL","GENENAME")
#  
#  fChr <- finch[,c(2,7)]
#  fChr <- fChr[fChr[,2]!="-",]
#  colnames(fChr) <- c("GID","CHROMOSOME")
#  
#  finchGOFile <- system.file("extdata","GO_finch.txt",
#     			   package="AnnotationForge")
#  fGO <- read.table(finchGOFile,sep="\t")
#  fGO <- fGO[fGO[,2]!="",]
#  fGO <- fGO[fGO[,3]!="",]
#  colnames(fGO) <- c("GID","GO","EVIDENCE")
#  
#  ## Then call the function
#  makeOrgPackage(gene_info=fSym, chromosome=fChr, go=fGO,
#                 version="0.1",
#                 maintainer="Some One <so@someplace.org>",
#                 author="Some One <so@someplace.org>",
#                 outputDir = ".",
#                 tax_id="59729",
#                 genus="Taeniopygia",
#                 species="guttata",
#                 goTable="go")
#  
#  ## then you can call install.packages based on the return value
#  install.packages("./org.Tguttata.eg.db", repos=NULL)

