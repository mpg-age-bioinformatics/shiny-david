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

### development area

.libPaths("/srv/shiny-server/david/libs")
library("RDAVIDWebService")


# https://david.ncifcrf.gov/webservice/register.htm

ltypes<-c('AFFYMETRIX_3PRIME_IVT_ID', 'AFFYMETRIX_EXON_GENE_ID',
          'AFFYMETRIX_SNP_ID', 'AGILENT_CHIP_ID',
          'AGILENT_ID', 'AGILENT_OLIGO_ID',
          'ENSEMBL_GENE_ID', 'ENSEMBL_TRANSCRIPT_ID',
          'ENTREZ_GENE_ID', 'FLYBASE_GENE_ID',
          'FLYBASE_TRANSCRIPT_ID','GENBANK_ACCESSION',
          'GENPEPT_ACCESSION', 'GENOMIC_GI_ACCESSION',
          'PROTEIN_GI_ACCESSION', 'ILLUMINA_ID',
          'IPI_ID', 'MGI_ID', 'GENE_SYMBOL', 'PFAM_ID',
          'PIR_ACCESSION','PIR_ID','PIR_NREF_ID', 'REFSEQ_GENOMIC',
          'REFSEQ_MRNA','REFSEQ_PROTEIN','REFSEQ_RNA','RGD_ID',
          'SGD_ID','TAIR_ID','UCSC_GENE_ID','UNIGENE',
          'UNIPROT_ACCESSION','UNIPROT_ID','UNIREF100_ID','WORMBASE_GENE_ID',
          'WORMPEP_ID','ZFIN_ID')

gene_ontology<-c(NULL, 'GOTERM_BP_1', 'GOTERM_BP_2', 'GOTERM_BP_3', 'GOTERM_BP_4',
                 'GOTERM_BP_5', 'GOTERM_BP_ALL', 'GOTERM_BP_FAT', 'GOTERM_CC_1',
                 'GOTERM_CC_2', 'GOTERM_CC_3', 'GOTERM_CC_4', 'GOTERM_CC_5',
                 'GOTERM_CC_ALL', 'GOTERM_CC_FAT', 'GOTERM_MF_1', 'GOTERM_MF_2',
                 'GOTERM_MF_3', 'GOTERM_MF_4', 'GOTERM_MF_5', 'GOTERM_MF_ALL',
                 'GOTERM_MF_FAT')

protein_domains<-c(NULL, 'BLOCKS_ID', 'COG', 'INTERPRO', 'PDB_ID',
                   'PFAM', 'PIR_ALN','PIR_HOMOLOGY_DOMAIN', 'PIR_SUPERFAMILY',
                   'PRINTS', 'PRODOM', 'PROSITE', 'SCOP_ID',
                   'SMART', 'TIGRFAMS')

pathways<-c(NULL, 'BBID', 'BIOCARTA', 'EC_NUMBER', 'KEGG_COMPOUND', 'KEGG_PATHWAY','KEGG_REACTION')

general_annotations<-c( NULL, 'ALIAS_GENE_SYMBOL', 'CHROMOSOME', 'CYTOBAND', 'GENE', 'GENE_SYMBOL', 
                        'HOMOLOGOUS_GENE', 'LL_SUMMARY', 'OMIM_ID', 'PIR_SUMMARY', 'PROTEIN_MW',
                        'REFSEQ_PRODUCT', 'SEQUENCE_LENGTH')
functional_categories<-c( NULL, 'CGAP_EST_QUARTILE', 'CGAP_EST_RANK', 'COG_ONTOLOGY', 
                          'PIR_SEQ_FEATURE', 'SP_COMMENT_TYPE', 'SP_PIR_KEYWORDS')

protein_protein_interactions<-c( NULL, 'BIND', 'DIP', 'HIV_INTERACTION_CATEGORY', 
                                 'HIV_INTERACTION', 'MINT', 'NCICB_CAPATHWAY')

literature<-c( NULL, 'GENERIF_SUMMARY','HIV_INTERACTION_PUBMED_ID','PUBMED_ID')

disease<-c( NULL, 'GENETIC_ASSOCIATION_DB_DISEASE', 'OMIM_DISEASE')

listTypes<-c("Gene", "Background")

david<-DAVIDWebService$new(email="Jorge.Boucas@age.mpg.de",url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
data(demoList1)

setAnnotationCategories(david, c("GOTERM_BP_ALL","GOTERM_MF_ALL", "GOTERM_CC_ALL"))

result<-addList(david, demoList1,
                idType="AFFYMETRIX_3PRIME_IVT_ID",
                listName="demoList1", listType="Gene")

result<-addList(david, head(demoList1,n=-20),
                idType="AFFYMETRIX_3PRIME_IVT_ID",
                listName="demoList1", listType="Gene")
result<-addList(david, demoList1,
                idType="AFFYMETRIX_3PRIME_IVT_ID",
                listName="demoList1", listType="Background")

termCluster<-getClusterReport(david, type="Gene")
getClusterReportFile(david, type="Term", fileName="/srv/shiny-server/david/DAVID.gene.tsv")

termCluster<-getClusterReport(david, type="Term")
getClusterReportFile(david, type="Term", fileName="/srv/shiny-server/david/DAVID.term.tsv")

getFunctionalAnnotationChartFile(david, fileName="/srv/shiny-server/david/DAVID.func.term.tsv")

write.table(df, "/srv/shiny-server/david/df.tsv", sep="\t", row.names=FALSE)


result

help(addList)