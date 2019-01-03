.libPaths("/srv/shiny-server/david/libs")
library(shiny)

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

# Define UI for application that draws a histogram
shinyUI( fluidPage(
  br(),
  titlePanel("DAVID web services"),
  br(),
  fluidRow(
    column(4,
      fileInput("file1", "Choose File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".tsv",
                           ".xlsx")
      ),
      radioButtons("filetype", 'Please select the input file type', choices = c('auto' = 'auto', 
                                                                                "excel" = 'xlsx',  
                                                                                'tab-separated' = '\t', 
                                                                                'comma-seperated' = ',', 
                                                                                'semicolon-separated' = ';'), inline = TRUE),
      checkboxInput("header", "Header", TRUE),
      a(href = "https://github.com/mpg-age-bioinformatics/shiny/raw/master/david/genes.xlsx", "Example input"),
      br(),br(),
      textInput("outfile", "Output file name", value="DAVIDws")
      ),
    column(4,
      selectInput("genes_list", "Select Target Genes Column", choices = NULL),
      selectInput("genes_list_id", "Target Genes ID type", choices = ltypes),
      selectInput("background_list", "Select Background Genes Column (optional)", choices = NULL),
      selectInput("background_list_id", "Background Genes ID type (optional)", choices = c("none",ltypes)) ,
      hr(),
      textInput("registeredmail", "Registered email", value=NULL),
      helpText("Please make sure you've registered your email ", a(href = "https://david.ncifcrf.gov/webservice/register.htm", "here" ),"." )
      ),
    column(4,
      selectInput("gene_ontology_in", "Gene ontology", choices = c('none',gene_ontology), multiple = TRUE, selected='GOTERM_BP_FAT'),
      selectInput("protein_domains_in", "Protein domains", choices = c('none',protein_domains), multiple = TRUE, selected='none'),
      selectInput("pathways_in", "Pathways", choices = c('none',pathways), multiple = TRUE, selected='none'),
      selectInput("general_annotations_in", "General annotations", choices = c('none',general_annotations), multiple = TRUE, selected='none'),
      selectInput("functional_categories_in", "Functional categories", choices = c('none',functional_categories), multiple = TRUE, selected='none'),
      selectInput("protein_protein_interactions_in", "Protein-protein interactions", choices = c('none',protein_protein_interactions), multiple = TRUE, selected='none'),
      selectInput("literature_in", "Literature", choices = c('none',literature), multiple = TRUE, selected='none'),
      selectInput("disease_in", "Disease", choices = c('none',disease), multiple = TRUE, selected='none')
      ),
    column(12,
           div(style="display:inline-block",submitButton('Submit'),  style="float:right")
           )
    ),
    mainPanel(
      br(),
      htmlOutput('mapped'),
      br(),
      div(style="display:inline-block", downloadButton('downloadTable', 'Cluster Report'), downloadButton('FunctionalAnnotation', 'Functional Annotation'), style="float:right"),
      br(),
      p("This App uses the", code('RDAVIDWebService'), " package. For more information read the respective documentation in ",
        a("cran", href = "http://bioconductor.org/packages/release/bioc/html/RDAVIDWebService.html"),
        "and visit ", a("DAVID Web Services page",href="https://david.ncifcrf.gov/content.jsp?file=WS.html"),".",
        "Please cite 'Fresno C, Fernández EA (2013). ",
        "RDAVIDWebService: a versatile R interface to DAVID. Bioinformatics, 29(21), 2810–2811.'",
        "."
        ),
      p("Please keep the version tag on all downloaded files."),
      htmlOutput('appversion')
  )
)
)
