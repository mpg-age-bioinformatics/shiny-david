## ----style, eval=TRUE, echo=FALSE, results='asis'--------------------------
BiocStyle::latex()

## ----include=FALSE---------------------------------------------------------
library(knitr)
opts_chunk$set(tidy=FALSE)

## ----Homo.sapiens----------------------------------------------------------
library(RSQLite)
library(Homo.sapiens)
columns(Homo.sapiens)

## ----Homo.sapiens2---------------------------------------------------------
keytypes(Homo.sapiens) 

## ----Homo.sapiens3---------------------------------------------------------
k <- head(keys(Homo.sapiens,keytype="ENTREZID"))
k

## ----Homo.sapiens4---------------------------------------------------------
result <- select(Homo.sapiens, keys=k, 
                 columns=c("TXNAME","TXSTART","TXSTRAND"), 
                 keytype="ENTREZID")
head(result)

## ----URI Example-----------------------------------------------------------
   uri <- 'http://www.uniprot.org/uniprot/?query='
   ids <- c('P13368', 'Q6GZX4')
   idStr <- paste(ids, collapse="+or+")
   format <- '&format=tab'
   fullUri <- paste0(uri,idStr,format)
   read.delim(fullUri)

## ----web service code------------------------------------------------------
getUniprotGoodies  <- function(query, columns)
{
    ## query and columns start as a character vectors
    qstring <- paste(query, collapse="+or+")
    cstring <- paste(columns, collapse=",")
    uri <- 'http://www.uniprot.org/uniprot/?query='
    fullUri <- paste0(uri,qstring,'&format=tab&columns=',cstring)
    dat <- read.delim(fullUri, stringsAsFactors=FALSE)
    ## now remove things that were not in the specific original query...
    dat <- dat[dat[,1] %in% query,]
    dat
}

## ----xml_tree--------------------------------------------------------------
library(XML)
uri <- "http://www.uniprot.org/uniprot/?query=P13368+or+Q6GZX4&format=xml"
xml <- xmlTreeParse(uri, useInternalNodes=TRUE)

## ----xml_namespace---------------------------------------------------------
defs <- xmlNamespaceDefinitions(xml, recurisve=TRUE)
defs

## ----xml_namespace_struct--------------------------------------------------
ns <- structure(sapply(defs, function(x) x$uri), names=names(defs))

## ----xml_namespace2--------------------------------------------------------
entry <- getNodeSet(xml, "//ns:entry", "ns")
xmlSize(entry)

## ----xml_xmlAttrs----------------------------------------------------------
nms <- xpathSApply(xml, "//ns:entry/ns:name", xmlValue, namespaces="ns")
attrs <- xpathApply(xml, "//ns:entry", xmlAttrs, namespaces="ns")
names(attrs) <- nms
attrs

## ----xml_xmlChildren-------------------------------------------------------
fun1 <- function(elt) unique(names(xmlChildren(elt)))
xpathApply(xml, "//ns:entry", fun1, namespaces="ns")

## ----xml_feature_type------------------------------------------------------
Q6GZX4 <- "//ns:entry[ns:accession='Q6GZX4']/ns:feature"
xmlSize(getNodeSet(xml, Q6GZX4, namespaces="ns"))

P13368 <- "//ns:entry[ns:accession='P13368']/ns:feature"
xmlSize(getNodeSet(xml, P13368, namespaces="ns"))

## ----xml_feature_type2-----------------------------------------------------
path <- "//ns:feature"
unique(xpathSApply(xml, path, xmlGetAttr, "type", namespaces="ns"))

## ----xml_feature_type_P13368-----------------------------------------------
path <- "//ns:entry[ns:accession='P13368']/ns:feature[@type='sequence conflict']"
data.frame(t(xpathSApply(xml, path, xmlAttrs, namespaces="ns")))

## ----xml_sequence----------------------------------------------------------
library(Biostrings)
path <- "//ns:entry/ns:sequence"
seqs <- xpathSApply(xml, path, xmlValue, namespaces="ns")
aa <- AAStringSet(unlist(lapply(seqs, function(elt) gsub("\n", "", elt)),
    use.names=FALSE))
names(aa) <- nms
aa

## ----WebServiceObject------------------------------------------------------
setClass("uniprot", representation(name="character"),
         prototype(name="uniprot"))

## ----makeInstanceWebServiceObj---------------------------------------------
    uniprot <- new("uniprot")

## ----onLoad2,eval=FALSE----------------------------------------------------
#  .onLoad <- function(libname, pkgname)
#  {
#      ns <- asNamespace(pkgname)
#      uniprot <- new("uniprot")
#      assign("uniprot", uniprot, envir=ns)
#      namespaceExport(ns, "uniprot")
#  }

## ----keytypeUniprot--------------------------------------------------------
setMethod("keytypes", "uniprot",function(x){return("UNIPROT")})
uniprot <- new("uniprot")
keytypes(uniprot)

## ----keytypeUniprot2-------------------------------------------------------
setMethod("columns", "uniprot", 
          function(x){return(c("ID", "SEQUENCE", "ORGANISM"))})
columns(uniprot)

## ----webServiceSelect------------------------------------------------------
.select <- function(x, keys, columns){
    colsTranslate <- c(id='ID', sequence='SEQUENCE', organism='ORGANISM')
    columns <- names(colsTranslate)[colsTranslate %in% columns]
    getUniprotGoodies(query=keys, columns=columns)
}
setMethod("select", "uniprot", 
    function(x, keys, columns, keytype)
{
    .select(keys=keys, columns=columns)
})

## ----webServiceSelect2, eval=FALSE-----------------------------------------
#  select(uniprot, keys=c("P13368","P20806"), columns=c("ID","ORGANISM"))

## ----classicConn,results='hide'--------------------------------------------
drv <- SQLite()
library("org.Hs.eg.db")
con_hs <- dbConnect(drv, dbname=system.file("extdata", "org.Hs.eg.sqlite",
                        package = "org.Hs.eg.db"))
con_hs
dbDisconnect(con_hs)

## ----ourConn---------------------------------------------------------------
require(hom.Hs.inp.db)
str(hom.Hs.inp.db)

## ----ourConn2--------------------------------------------------------------
hom.Hs.inp.db$conn
## or better we can use a helper function to wrap this:
AnnotationDbi::dbconn(hom.Hs.inp.db)
## or we can just call the provided convenience function 
## from when this package loads:
hom.Hs.inp_dbconn()

## ----dbListTables----------------------------------------------------------
con <- AnnotationDbi::dbconn(hom.Hs.inp.db)
head(dbListTables(con))
dbListFields(con, "Mus_musculus")

## ----dbGetQuery------------------------------------------------------------
dbGetQuery(con, "SELECT * FROM metadata")

## ----dbListTables2---------------------------------------------------------
head(dbListTables(con))

## ----dbListFields2---------------------------------------------------------
dbListFields(con, "Apis_mellifera")

## ----dbGetQuery2-----------------------------------------------------------
head(dbGetQuery(con, "SELECT * FROM Apis_mellifera"))

## ----Anopheles,eval=FALSE--------------------------------------------------
#  head(dbGetQuery(con, "SELECT * FROM Anopheles_gambiae"))
#  ## Then only retrieve human records
#  ## Query: SELECT * FROM Anopheles_gambiae WHERE species='HOMSA'
#  head(dbGetQuery(con, "SELECT * FROM Anopheles_gambiae WHERE species='HOMSA'"))
#  dbDisconnect(con)

## ----getMetadata, echo=FALSE-----------------------------------------------
library(hom.Hs.inp.db)
hom.Hs.inp_dbInfo()

## ----referenceClass,eval=FALSE---------------------------------------------
#  .InparanoidDb <-
#      setRefClass("InparanoidDb", contains="AnnotationDb")

## ----onLoad,eval=FALSE-----------------------------------------------------
#  sPkgname <- sub(".db$","",pkgname)
#  db <- loadDb(system.file("extdata", paste(sPkgname,
#                 ".sqlite",sep=""), package=pkgname, lib.loc=libname),
#                 packageName=pkgname)
#  dbNewname <- AnnotationDbi:::dbObjectName(pkgname,"InparanoidDb")
#  ns <- asNamespace(pkgname)
#  assign(dbNewname, db, envir=ns)
#  namespaceExport(ns, dbNewname)

## ----columns,eval=FALSE----------------------------------------------------
#  .cols <- function(x)
#  {
#      con <- AnnotationDbi::dbconn(x)
#      list <- dbListTables(con)
#      ## drop unwanted tables
#      unwanted <- c("map_counts","map_metadata","metadata")
#      list <- list[!list %in% unwanted]
#      ## Then just to format things in the usual way
#      list <- toupper(list)
#      dbDisconnect(con)
#      list
#  }
#  
#  ## Then make this into a method
#  setMethod("columns", "InparanoidDb", .cols(x))
#  ## Then we can call it
#  columns(hom.Hs.inp.db)

## ----keytypes,eval=FALSE---------------------------------------------------
#  setMethod("keytypes", "InparanoidDb", .cols(x))
#  ## Then we can call it
#  keytypes(hom.Hs.inp.db)
#  
#  ## refactor of .cols
#  .getLCcolnames <- function(x)
#  {
#      con <- AnnotationDbi::dbconn(x)
#      list <- dbListTables(con)
#      ## drop unwanted tables
#      unwanted <- c("map_counts","map_metadata","metadata")
#      list <- list[!list %in% unwanted]
#      dbDisconnect(con)
#      list
#  }
#  .cols <- function(x)
#  {
#      list <- .getLCcolnames(x)
#      ## Then just to format things in the usual way
#      toupper(list)
#  }
#  ## Test:
#  columns(hom.Hs.inp.db)
#  
#  ## new helper function:
#  .getTableNames <- function(x)
#  {
#      LC <- .getLCcolnames(x)
#      UC <- .cols(x)
#      names(UC) <- LC
#      UC
#  }
#  .getTableNames(hom.Hs.inp.db)

## ----keys,eval=FALSE-------------------------------------------------------
#  .keys <- function(x, keytype)
#  {
#      ## translate keytype back to table name
#      tabNames <- .getTableNames(x)
#      lckeytype <- names(tabNames[tabNames %in% keytype])
#      ## get a connection
#      con <- AnnotationDbi::dbconn(x)
#      sql <- paste("SELECT inp_id FROM",lckeytype, "WHERE species!='HOMSA'")
#      res <- dbGetQuery(con, sql)
#      res <- as.vector(t(res))
#      dbDisconnect(con)
#      res
#  }
#  
#  setMethod("keys", "InparanoidDb", .keys(x, keytype))
#  ## Then we can call it
#  keys(hom.Hs.inp.db, "TRICHOPLAX_ADHAERENS")

## ----dbDisconnect----------------------------------------------------------
   dbDisconnect(con)

## ----makeNewDb-------------------------------------------------------------
drv <- dbDriver("SQLite")
dbname <- file.path(tempdir(), "myNewDb.sqlite")
con <- dbConnect(drv, dbname=dbname)

## ----exampleFrame----------------------------------------------------------
data = data.frame(id=c(1,2,9),
                  string=c("Blue",
                           "Red",
                           "Green"),
                  stringsAsFactors=FALSE)

## ----exercise2-------------------------------------------------------------
dbGetQuery(con, "CREATE Table genePheno (id INTEGER, string TEXT)")

## ----LabelledPreparedQueries-----------------------------------------------
names(data) <- c("id","string")
sql <- "INSERT INTO genePheno VALUES ($id, $string)"
dbBegin(con)
res <- dbSendQuery(con,sql)
dbBind(res, data)
dbFetch(res)
dbClearResult(res)
dbCommit(con)

## ----ATTACH----------------------------------------------------------------
db <- system.file("extdata", "TxDb.Hsapiens.UCSC.hg19.knownGene.sqlite", 
                  package="TxDb.Hsapiens.UCSC.hg19.knownGene")
dbGetQuery(con, sprintf("ATTACH '%s' AS db",db))

## ----ATTACHJoin------------------------------------------------------------
  sql <- "SELECT * FROM db.gene AS dbg, 
          genePheno AS gp WHERE dbg.gene_id=gp.id"
  res <- dbGetQuery(con, sql)
  res

## ----SessionInfo, echo=FALSE-----------------------------------------------
sessionInfo()

