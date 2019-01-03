.libPaths("/srv/shiny-server/david/libs")
gitversion <- function(){ 
  git<-read.csv("/srv/shiny-server/.git/refs/heads/master", header=FALSE)
  git<-git$V1
  git<-toString(git[1])
  git<-substr(git, 1, 7)
  return(git)
}
library(shiny)
library(xlsx)
library(RDAVIDWebService)

#     genes_list genes_list_id background_list background_list_id registeredmail gene_ontology_in protein_domains_in pathways_in general_annotations_in functional_categories_in protein_protein_interactions_in literature_in disease_in
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # reformat input data
  target.genes <- reactive({
    inFile <- input$file1
    filetype <- input$filetype
    req(inFile)
    
    filetype_map <- c("xlsx" = 'xlsx',  'tsv' = '\t', 'csv' = ',', 'txt'=" ")
    if(filetype == 'auto'){
      file_extension =  unlist(strsplit(inFile$datapath, '[.]'))[length(unlist(strsplit(inFile$datapath, '[.]')))]
      if(file_extension %in% names(filetype_map)){
        filetype <- filetype_map[file_extension]
        names(filetype) <- NULL
        
      } else {
        print(paste("wrong file format", file_extension))
        return(NULL)
      }
    }
    
    if(filetype == 'xlsx'){
      D <- read.xlsx(inFile$datapath, sheetIndex = 1, header = input$header)
    } else {
      D <- read.csv(inFile$datapath, header = input$header, sep = filetype)
    }
    varstarget <- names(D)
    updateSelectInput(session, "genes_list", "Select Target Genes Column", choices = varstarget)
    updateSelectInput(session, "background_list", "Select Background Genes Column (optional)", choices = c("none",varstarget)) 
    req(input$genes_list)
    
    D[D == ''] <- NA
    
    target.genes.tmp <- D[[input$genes_list]]
    target.genes.tmp<-target.genes.tmp[!is.na(target.genes.tmp)]
    return(target.genes.tmp)
  })

  background <- reactive({
    inFile <- input$file1
    filetype <- input$filetype
    req(inFile)
    
    filetype_map <- c("xlsx" = 'xlsx',  'tsv' = '\t', 'csv' = ',', 'txt'=" ")
    if(filetype == 'auto'){
      file_extension =  unlist(strsplit(inFile$datapath, '[.]'))[length(unlist(strsplit(inFile$datapath, '[.]')))]
      if(file_extension %in% names(filetype_map)){
        filetype <- filetype_map[file_extension]
        names(filetype) <- NULL
        
      } else {
        print(paste("wrong file format", file_extension))
        return(NULL)
      }
    }
    
    if(filetype == 'xlsx'){
      D <- read.xlsx(inFile$datapath, sheetIndex = 1, header = input$header)
    } else {
      D <- read.csv(inFile$datapath, header = input$header, sep = filetype)
    }
    vars <- names(D)
    if (toString(input$background_list) != "none") {
      updateSelectInput(session, "background_list_id", "Background Genes ID type")
      req(input$background_list_id)
    }
    
    D[D == ''] <- NA
    
    if (input$background_list!="none"){
      background.genes <- D[[input$background_list]]
      background.genes<-background.genes[!is.na(background.genes)]
    } else {
      background.genes<-NULL
    }
    return(background.genes)
  })
  
  getcats <- reactive({
    req(input$gene_ontology_in)
    req(input$protein_domains_in)
    req(input$pathways_in)
    req(input$general_annotations_in)
    req(input$functional_categories_in)
    req(input$protein_protein_interactions_in)
    req(input$literature_in)
    req(input$disease_in)
    
    categories<-c(input$gene_ontology_in,input$protein_domains_in,input$pathways_in,
                  input$general_annotations_in,input$functional_categories_in,
                  input$protein_protein_interactions_in,input$literature_in,input$disease_in)
    categories<-categories[categories!="none"]
    return(categories)
  })
  
  output$mapped <- renderText ({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    tmp<-target.genes()
    tmp<-background()
    
    req(input$registeredmail)
    
    david<-DAVIDWebService$new(email=input$registeredmail,url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
    
    categories<-getcats()
    
    setAnnotationCategories(david, categories)
    
    result<-addList(david, target.genes(),
                    idType=input$genes_list_id,
                    listName="target_genes", listType="Gene")
    
    targets_mapped<-result$inDavid
    targets_notmapped<-paste(result$unmappedIds, collapse = ', ')
    
    rep=paste0("Percentage of target genes mapped: ", targets_mapped*100.00, "%",
               "<br>",
               "Not mapped target genes: ", targets_notmapped,"<br>")
    
    if (!is.null(background())){
      if (toString(input$background_list_id)=="none")
        return(NULL)
      david<-DAVIDWebService$new(email=input$registeredmail,url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
      result<-addList(david, background(),
                      idType=input$background_list_id,
                      listName="background_genes", listType="Background")
      background_mapped<-result$inDavid
      background_unmapped<-paste(result$unmappedIds, collapse = ", ")
      
      david<-DAVIDWebService$new(email=input$registeredmail,url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
      result<-addList(david, target.genes(),
                      idType=input$genes_list_id,
                      listName="target_genes", listType="Gene")
      result<-addList(david, background(),
                      idType=input$background_list_id,
                      listName="background_genes", listType="Background")
      
      rep<-paste0(rep,
                  "<br>",
                  "Percentage of background genes mapped: ", background_mapped*100.00, "%",
                  "<br>",
                  "Not mapped background genes: ", background_unmapped)
      
    }
    return(rep)
  })
    
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste(input$outfile,".ClusterReport.",gitversion(),".csv", sep = "")
    },
    content = function(file) {
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)

      david<-DAVIDWebService$new(email=input$registeredmail,url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
      categories<-getcats()

      setAnnotationCategories(david, categories)

      result<-addList(david, target.genes(),
                      idType=input$genes_list_id,
                      listName="target_genes", listType="Gene")

      if (!is.null(background())){
        if (toString(input$background_list_id)=="none")
         return(NULL)
        david<-DAVIDWebService$new(email=input$registeredmail,url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
        result<-addList(david, background(),
                idType=input$background_list_id,
                listName="background_genes", listType="Background")
        
        david<-DAVIDWebService$new(email=input$registeredmail,url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
        result<-addList(david, target.genes(),
                       idType=input$genes_list_id,
                        listName="target_genes", listType="Gene")
        result<-addList(david, background(),
                        idType=input$background_list_id,
                        listName="background_genes", listType="Background")
      }

      getClusterReportFile(david, type="Term", fileName=file)
      #getFunctionalAnnotationChartFile(david, fileName=file)
      
    }
  )
  
  output$FunctionalAnnotation <- downloadHandler(
    filename = function() {
      paste(input$outfile,".FunctionalAnnotation.",gitversion(),".csv", sep = "")
    },
    content = function(file) {
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      david<-DAVIDWebService$new(email=input$registeredmail,url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
      categories<-getcats()
      
      setAnnotationCategories(david, categories)
      
      result<-addList(david, target.genes(),
                      idType=input$genes_list_id,
                      listName="target_genes", listType="Gene")
      
      if (!is.null(background())){
        if (toString(input$background_list_id)=="none")
          return(NULL)
        david<-DAVIDWebService$new(email=input$registeredmail,url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
        result<-addList(david, background(),
                        idType=input$background_list_id,
                        listName="background_genes", listType="Background")
        
        david<-DAVIDWebService$new(email=input$registeredmail,url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
        result<-addList(david, target.genes(),
                        idType=input$genes_list_id,
                        listName="target_genes", listType="Gene")
        result<-addList(david, background(),
                        idType=input$background_list_id,
                        listName="background_genes", listType="Background")
      }
      
      #getClusterReportFile(david, type="Term", fileName=file)
      getFunctionalAnnotationChartFile(david, fileName=file)
    }
  )
  
  
  output$appversion <- renderText ({ 
    paste0('App version: <b>',gitversion(),'</b>')
  }
  )
})