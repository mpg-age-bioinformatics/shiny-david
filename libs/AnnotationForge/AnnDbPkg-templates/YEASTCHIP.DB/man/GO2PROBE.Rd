\name{@ANNOBJPREFIX@GO2PROBE}
\alias{@ANNOBJPREFIX@GO2PROBE}
\title{Map between Gene Ontology (GO) and Manufacturer Identifiers}
\description{
   @ANNOBJPREFIX@GO2PROBE is an R object that provides mappings between
   GO identifiers and manufacturer identifiers.
}
\details{
  Each GO term maps to a named vector of manufacturer identifiers. The name
  associated with each manufacturer identifier corresponds to the evidence code
  for that GO identifier. The evidence code indicates what kind of evidence
  supports the association between the GO and Entrez Gene identifiers. Evidence
  codes currently in use include:
  
  IMP - inferred from mutant phenotype
  
  IGI - inferred from genetic interaction

  IPI - inferred from physical interaction  

  ISS - inferred from sequence similarity  

  IDA - inferred from direct assay  

  IEP - inferred from expression pattern  

  IEA - inferred from electronic annotation  

  TAS - traceable author statement  

  NAS - non-traceable author statement  

  ND - no biological data available  

  IC - inferred by curator
  
  A GO identifier may be mapped to the same probe identifier more than once but
  the evidence code can be different. Mappings between Gene Ontology identifiers
  an Gene Ontology terms and other information are available in a
  separate data package named GO. 
  
  Mappings were based on data provided by:

  @GO2PROBESOURCE@

}

\references{
\url{ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/}
}

\seealso{
  \itemize{
    \item \code{\link[AnnotationDbi]{AnnotationDb-class}} for use of
          the \code{select()} interface.
  }
}

\examples{
## select() interface:
## Objects in this package can be accessed using the select() interface
## from the AnnotationDbi package. See ?select for details.

## Bimap interface:
# Convert to a list
xx <- as.list(@ANNOBJPREFIX@GO2PROBE)
if(length(xx) > 0){
    # Gets the probe identifiers for the top 2nd and 3nd GO identifiers
    goids <- xx[2:3]
    # Gets the probe identifiers for the first element of goids
    goids[[1]]
    # Evidence code for the mappings
    names(goids[[1]])
}
}

\keyword{datasets}

