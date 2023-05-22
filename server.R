library(stringr)
library(tools)
library(Matrix)
library(shiny)
library(DT)
library(reshape) 
library(ggplot2)
library(cicerone)
library(shinyjs)
library(shinytoastr)
library(shinylogs)
library(visNetwork)
#library(webshot)
library(preprocessCore)
#library(patchwork)
library(withr)
library(htmlwidgets)
library(rjson)
library(clipr)
library(writexl)
library(pracma)
library(limma, include.only = 'squeezeVar')
library(statmod)
library(plotly)
library(cicerone)
library(hrbrthemes)
library(viridis)
library("ggthemes")

options(shiny.sanitize.errors = FALSE)

library(ids)

source("current_version.R")
source("src/common_util.R")
source("src/ui_util.R")
source("src/compute_pvalues.R")
source("src/t2zstatistic.R")

tryCatch({
  library(rjson)
  email_credentials = fromJSON(file = "email/credentials.json")
  options(EMAIL_AVAILABLE = TRUE)
}, 
error = function(e){options(EMAIL_AVAILABLE = FALSE)},
warning = function(e){}
)
EMAIL_AVAILABLE = getOption("EMAIL_AVAILABLE")
if(is.null(EMAIL_AVAILABLE)){
  EMAIL_AVAILABLE = FALSE
}


# Define server logic required to draw a histogram
function(input, output, session) {
  observe_helpers(withMathJax = TRUE, help_dir = "helpfiles")
  cache = list()
  
  session_id <- reactiveVal(random_id(n = 1, byte = 8))
  network_value <- reactiveVal("uniprot.human")
  upload_name <- reactiveVal("")
  upload_name_metadata <- reactiveVal("")
  myvalue <- reactiveVal("sample")
  initialized <- reactiveVal(TRUE)
  upload_data_ready <- reactiveVal(FALSE)
  upload_expression_data_ready <- reactiveVal(FALSE)
  upload_metadata_ready <- reactiveVal(FALSE)
  metadata_ready <- reactiveVal(FALSE)
  analyze_group_differences <- reactiveVal(FALSE)
  
  source(file = "src/server_00_misc_functions.R", local=TRUE)
  source(file = "src/server_00_matlab_helpers.R", local=TRUE)
  source(file = "src/server_00_optionset.R", local=TRUE)
  source(file = "src/server_00_logging_main.R", local=TRUE)
  source(file = "src/server_00_ui_links.R", local=TRUE)
  source(file = "src/server_00_leave_feedback.R", local=TRUE)
  source(file = "src/server_00_util.R", local=TRUE)
  source(file = "src/server_00_interactive_tutorial.R", local=TRUE)
  
  observeEvent(input$initialized, {
    if(!dir.exists("logs/")){
      dir.create("logs/")
    }
    main_logging("Session Initialized")
  })
  
  refProteomeValue <- reactive({
    switch(input$refproteome, 
           "Uniprot Human" = "uniprot.human",
           "Uniprot Mouse" = "uniprot.mouse",
           "Uniprot Rat" = "uniprot.rat")
  })
  
  # Comment the following to change mapping beyond data upload
  observeEvent(input$refproteome, {
    network_value(refProteomeValue())
  })
  
  source(file = "src/server_reactive_network.R", local=TRUE)
  source(file = "src/server_sample_data.R", local=TRUE)
  source(file = "src/server_upload_data.R", local=TRUE)
  
  source(file = "src/server_reactive_dataset.R", local=TRUE)
  source(file = "src/server_parse_current_dataset.R", local=TRUE)
  
  source(file = "src/server_select_subgroups.R", local=TRUE)
  source(file = "src/server_select_group_differences.R", local=TRUE)
  
  foFilterDataset <- function(ds, validSamples){
    ds$Ts <- ds$Ts[, validSamples]
    ds$Ts_raw <- ds$Ts_raw[, validSamples]
    if(!is.null(ds$Ts2)){
      ds$Ts2 = ds$Ts2[, validSamples]
    }
    return(ds)
  }
  
  filtered_dataset <- reactive({
    req(subgroup_samples())
    req(current_dataset_mapped())
    validSamples <- subgroup_samples()
    ds <- current_dataset_mapped()
    ds = foFilterDataset(ds, validSamples)
    return(ds)
  })
  
  foFilterMetadata <- function(x, validSamples){
    x$nSample <- nnzero(validSamples)
    x$caseSamples <- x$caseSamples[validSamples]
    x$Tsample_metadata <- x$Tsample_metadata[, validSamples]
    if(analyze_group_differences()){
      gd <- selected_group_differences()
      x$samplesA <- gd$samplesA[validSamples]
      x$samplesB <- gd$samplesB[validSamples]
    }
    return(x)
  }
  
  filtered_metadata <- reactive({
    req(subgroup_samples())
    validSamples <- subgroup_samples()
    x <- current_metadata()
    x = foFilterMetadata(x, validSamples)
    return(x)
  })
  
  source(file = "src/server_process_data_main.R", local=TRUE)
  source(file = "src/server_process_data_bysample.R", local=TRUE)
  source(file = "src/server_process_site_tables.R", local=TRUE)
  source(file = "src/server_process_go_enrichment_tables.R", local=TRUE)
  source(file = "src/server_process_pathway_targets_table.R", local=TRUE)
  
  source(file = "src/server_barplot_main.R", local=TRUE)
  source(file = "src/server_heatmap_main.R", local=TRUE)
  source(file = "src/server_barplot_samplewise.R", local=TRUE)
  
  source(file = "src/server_volcanoplots.R", local=TRUE)
  source(file = "src/server_table_outputs.R", local=TRUE)
  
  source(file = "src/server_site_barplots.R", local=TRUE)
  # source(file = "src/server_site_heatmaps.R", local=TRUE)
  source(file = "src/server_protexpression_heatmaps.R", local=TRUE)
  
  source(file = "src/server_modalbox_main.R", local=TRUE)
  source(file = "src/server_modalbox_barplots.R", local=TRUE)
  
  source(file = "src/server_table_outputs_enrichment.R", local=TRUE)
  source(file = "src/server_table_outputs_targets.R", local=TRUE)
  
  source(file = "src/server_report_generator.R", local=TRUE)
}

