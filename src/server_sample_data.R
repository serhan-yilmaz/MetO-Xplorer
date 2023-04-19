# observeEvent(input$buttonSampleData, {
#   foLoadSampleData()
#   main_logging("Sample Data")
#   a <- guide$get_next()
#   
#   if(!is.null(a) && guide$get_next()$highlighted == "optionbox_filter_by_subgroup_wrapper"){
#     #message("abcd")
#     delay(100, guide$move_forward())
#   }
#   
# })

observeEvent(input$selected_dataset, {
  if(identical(input$selected_dataset, "Upload Data")){
    myvalue("upload")
  } else {
    myvalue("sample")
    foLoadSampleData()
  }
})

reactive_selected_data_refproteome <- reactive({
  ## TODO: Switch-Case Statement to set refproteome based on input$selected_dataset
  refproteome = "uniprot.mouse"
  switch(input$selected_dataset,
    "CWRU-AD-9Month" = refproteome <- "uniprot.mouse"
  )
  
  return(refproteome)
})

reactive_selected_data <- reactive({
  validate(need(identical(myvalue(), "sample"), ""))
  
  folder = "data/"
  
  ## TODO: Switch-Case Statement to set data_name based on input$selected_dataset
  # data_name = "cwru_alz_9mo"
  switch(input$selected_dataset, 
    "CWRU-AD-9Month" = data_name <- "cwru_alz_9mo",
    stop("Selected dataset is not found!")
  )
  
  Toxidation <- read.csv(paste(folder, data_name, "_oxidation.csv", sep=""))
  Tunmodified <- read.csv(paste(folder, data_name, "_unmodified.csv", sep=""))
  Tmetadata <- read.csv(paste(folder, data_name, "_metadata.csv", sep=""))

  out = list()
  out$Toxidation = Toxidation
  out$Tunmodified = Tunmodified
  out$Tmetadata = Tmetadata
  return(out)
})

foLoadSampleData <- function(){
  shinyWidgets::updatePickerInput(session, "refproteome", selected = "Uniprot Mouse");
  if(input$mainTabset == "About"){
    updateTabsetPanel(session, "mainTabset", "Phosphosite")
  }
  network_value(reactive_selected_data_refproteome())
  myvalue("sample")
  reset('file1')
  reset('file1_expression')
  reset('file2')
  upload_data_ready(FALSE)
  upload_expression_data_ready(FALSE)
  upload_metadata_ready(FALSE)
  a = current_dataset()
  b = current_metadata()
  # c = current_dataset_mapped()
}