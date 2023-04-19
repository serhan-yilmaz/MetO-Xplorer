foUpdateTabset_Results <- function(){
  if(identical(myvalue(), "upload")){
    if(input$mainTabset == "About"){
      updateTabsetPanel(session, "mainTabset", "MetO Results")
    }
  }
}

reactive_dataset <- reactive({
  req(initialized())
  if(myvalue() == "upload"){
    validate(
      need(upload_data_ready(), "Waiting for data upload...")
    )
  }
  switch (myvalue(),
          "sample" = D <- reactive_selected_data()$Toxidation,
          "upload" = D <- upload_dataset(),
          "deploymentdata" = D <- Tdeployment_data,
          validate(
            need(FALSE, "Waiting for data...")
          )
  )
  return (D)
})

reactive_expression_dataset <- reactive({
  req(initialized())
  if(myvalue() == "upload"){
    if(!upload_expression_data_ready()){
      return(NULL)
    }
  }
  switch (myvalue(),
          "sample" = D <- reactive_selected_data()$Tunmodified,
          "upload" = D <- upload_expression_dataset(),
          "deploymentdata" = D <- Tdeployment_expression_data,
          validate(
            need(FALSE, "Waiting for data...")
          )
  )
  return (D)
})

# reactive_expression_dataset <- reactive({
#   folder = "data/"
#   Tsample <- read.csv(paste0(folder, "pexpression_alz_explorer_data_withoutpos.csv"))
#   # NULL
# })

reactive_metadata <- reactive({
  req(initialized())
  metadata_ready(FALSE)
  if(myvalue() == "upload"){
    validate(
      need(upload_metadata_ready(), "Waiting for metadata upload...")
    )
  }
  switch (myvalue(),
          "sample" = D <- reactive_selected_data()$Tmetadata,
          "upload" = D <- upload_metadata(),
          "deploymentdata" = D <- Tdeployment_metadata,
          validate(
            need(FALSE, "Waiting for data...")
          )
  )
  # foOnMetadataUpdate()
  return (D)
})

# observe({
#   if(metadata_ready() == TRUE){
#     isolate(foOnMetadataUpdate())
#   }
# })
# 
# foOnMetadataUpdate <- function(){
#   uiOutput("site_heatmap_select_group_ui")
# }