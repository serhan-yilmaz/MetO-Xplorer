## Protein expression samplewise barplot
modal_protexpression_samplewise_barplot <- reactive({
  req(modal_box_selection_mapped())
  # validate(need(input$mbox_site_plot_samples_case_control, ""))
  mds <- modal_box_selection_mapped()
  validate(need(mds$isProtein, ""))
  req(processed_expression_data_bysample_unfiltered())
  ds <- processed_expression_data_bysample_unfiltered()
  
  ds$ST$Identifier = ds$ST$ProteinName
  
  groupings = input$mbox_site_plot_select_group
  # case_control_option = input$mbox_site_plot_samples_case_control
  # case_control_option = "Case samples"
  # case_control_option = "xx"
  fill_txt = NULL
  if(identical(input$options_transformation, "Ratio of Spectral Counts")){
    fill_txt = "Spectral Count"
  }
  if(identical(input$options_transformation, "Ratio of Percentages")){
    fill_txt = "Percentage"
  }
  if(identical(input$options_transformation, "Odds Ratio")){
    fill_txt = "Percentage"
  }
  optx = list()
  optx$QColName = "Oxidation"
  
  if(identical(input$mbox_plotting_style, "Barplot")){
    case_control_option = "Case samples"
    plot_option = "barplot"
    yaxisText = "Log2-FC"
    optx$QColName = "Log2FC"
  } else {
    case_control_option = ""
    plot_option = "boxplot"
    yaxisText = fill_txt
    optx$QColName = fill_txt
  }
  
  showSampleNames = T
  # showSampleNames = input$mbox_site_plot_show_samples

  barplot_samplewise(ds, ds$ST, mds, groupings, "Protein", case_control_option, 
                     showSampleNames = showSampleNames, optx, plot_option = plot_option, 
                     yaxisText = yaxisText)
})

modal_protein_samplewise_barplot <- reactive({
  modal_protexpression_samplewise_barplot()
  # if(is_protein_modalbox_plot_showing_expression()){
  #   modal_protexpression_samplewise_barplot()
  # } else {
  #   modal_phosphoprotein_samplewise_barplot()
  # }
})

output$modal_protein_samplewise_barplot <- renderPlot({
  modal_protein_samplewise_barplot()$plot
})

## Kinase samplewise barplot
modal_kinase_samplewise_barplot <- reactive({
  req(modal_box_selection_mapped())
  validate(need(input$mbox_site_plot_samples_case_control, ""))
  mds <- modal_box_selection_mapped()
  validate(need(mds$isKinase, ""))
  req(processed_kinase_data_bysample_unfiltered())
  
  ds <- processed_kinase_data_bysample_unfiltered()
  groupings = input$mbox_site_plot_select_group
  case_control_option = input$mbox_site_plot_samples_case_control
  showSampleNames = input$mbox_site_plot_show_samples
  optx = list()
  optx$QColName = "Activity"
  barplot_samplewise(ds, ds$KT, mds, groupings, "Kinase", case_control_option, 
                     showSampleNames = showSampleNames, optx)
})

output$modal_kinase_samplewise_barplot <- renderPlot({
  modal_kinase_samplewise_barplot()$plot
})

## Goterm samplewise barplot
modal_goenrichment_samplewise_barplot <- reactive({
  req(modal_box_selection_mapped())
  # validate(need(input$mbox_site_plot_show_samples, ""))
  mds <- modal_box_selection_mapped()
  validate(need(mds$isGOTerm, ""))
  req(processed_go_enrichment_bysample_unfiltered())
  
  ds <- processed_go_enrichment_bysample_unfiltered()
  groupings = input$mbox_site_plot_select_group
  case_control_option = "Case samples"
  showSampleNames = input$mbox_site_plot_show_samples
  optx = list()
  optx$QColName = "Enrichment"
  barplot_samplewise(ds, ds$GO, mds, groupings, "GOTerm", case_control_option, 
                     showSampleNames = showSampleNames, optx)
})

output$modal_goenrichment_samplewise_barplot <- renderPlot({
  modal_goenrichment_samplewise_barplot()$plot
})



# modal_current_samplewise_barplot <- reactive({
#   req(modal_box_selection())
#   selection <- modal_box_selection()
#   if(selection$isSite == TRUE){
#     return(modal_site_samplewise_barplot)
#   }
#   if(selection$isProtein == TRUE){
#     return(modal_protein_samplewise_barplot)
#   }
#   if(selection$isKinase == TRUE){
#     return(modal_kinase_samplewise_barplot)
#   }
#   return(NULL)
# })

modal_current_samplewise_plot_info <- reactive({
  req(modal_box_selection())
  selection <- modal_box_selection()
  if(selection$isSite == TRUE){
    return(modal_site_samplewise_barplot())
  }
  if(selection$isProtein == TRUE){
    return(modal_protein_samplewise_barplot())
  }
  if(selection$isKinase == TRUE){
    return(modal_kinase_samplewise_barplot())
  }
  if(selection$isGOTerm == TRUE){
    return(modal_goenrichment_samplewise_barplot())
  }
  return(NULL)
})

modal_current_samplewise_barplot <- reactive({
  return(modal_current_samplewise_plot_info()$plot)
})

modal_current_samplewise_barplot_data <- reactive({
  return(modal_current_samplewise_plot_info()$plotdata)
})

foBarplotFilename <- function(name_ext){
  selection <- modal_box_selection()
  # protein_txt <- ""
  # if(selection$isProtein){
  #   if(is_protein_modalbox_plot_showing_expression()){
  #     protein_txt <- "-expression"
  #   } else {
  #     protein_txt <- "-phosphorylation"
  #   }
  # }
  protein_txt <- "-oxidation"
  paste0(selection$main_identifier, protein_txt, "_", name_ext);
}

modal_barplot_filename <- reactive({
  req(modal_box_selection())
  if(identical(input$mbox_plotting_style, "Barplot")){
    fname = "barplot"
  } else {
    fname = "boxplot"
  }
  foBarplotFilename(fname)
})

modal_barplot_data_filename <- reactive({
  req(modal_box_selection())
  if(identical(input$mbox_plotting_style, "Barplot")){
    fname = "barplot"
  } else {
    fname = "boxplot"
  }
  foBarplotFilename(paste0(fname, "_data"))
})

modalBoxDownloadPlotDLHandler <- function(file_type){
  downloadPlotDLHandler(
    modal_current_samplewise_barplot, 
    file_name = modal_barplot_filename, 
    file_type = file_type, 
    w_mult = 1.5,
    h_mult = 1
  )
}

downloadExcelFileHandler <- function(data_file, file_name, sheet_name = "Sheet1"){
  downloadHandler(
    filename = function() { 
      if(is.reactive(file_name)){
        file_name = file_name()
      }
      file_name = gsub(":", "", file_name)
      paste0(file_name, '.xlsx'); 
      },
    content = function(file) {
      if(is.reactive(data_file)){
        data_file = data_file()
      }
      if(is.reactive(sheet_name)){
        sheet_name = sheet_name()
      }
      xl = list()
      xl[[sheet_name]] = data_file
      write_xlsx(xl, file)
    }
  )
}

modalBoxPlotDataDLHandler <- function(){
  downloadExcelFileHandler(
    modal_current_samplewise_barplot_data, 
    file_name = modal_barplot_data_filename, 
    sheet_name = "Plot Data"
  )
}

output$modalbox_barplot_downloadPlotPNG <- modalBoxDownloadPlotDLHandler("png")
output$modalbox_barplot_downloadPlotPDF <- modalBoxDownloadPlotDLHandler("pdf")
output$modalbox_barplot_downloadPlotDataExcel <- modalBoxPlotDataDLHandler()

