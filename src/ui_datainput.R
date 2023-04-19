upload_data_helper <- function(el, helper_file = "input_data_format", 
                               tooltip = "Click to learn the format."){
  tooltip_id = paste0(helper_file, "_tooltip_icon")
  tags$span(
    style = "display:inline;", 
    helper(el, 
           id = tooltip_id,
           type = "markdown", 
           content = helper_file),
    tippy_this(tooltip_id, paste0("<span style='font-size:14px; margin: 0px;'>", tooltip, "<span>"), allowHTML = TRUE)
  )
}

upload_data_combined_helper <- function(el, helper_file = "input_data_format_combined"){
  tags$span(
    style = "display:inline;", 
    helper(el, 
           id = "upload_data_tooltip_icon",
           type = "markdown", 
           content = helper_file),
    tippy_this("upload_data_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Click to learn the file format.<span>", allowHTML = TRUE)
  )
}

deploymentDataDownloadDiv <- function(use_expression_data = FALSE){
  if(use_expression_data){
    expression_download <- downloadButton('buttonDownloadDeploymentExpressionData', 'Expression Data')
    helper_file = "input_data_format_combined_withexpression"
  } else {
    expression_download <- tags$span()
    helper_file = "input_data_format_combined"
  }
  
  upload_data_combined_helper(tags$div(
    style = "margin-bottom:8px", 
    tags$b("Download Data: ", style = "margin-right: 10px;"),
    tags$div(
      style = "margin-top: 2px; display:inline-block;", 
      #  style = "border-style: inset; padding: 2px;", 
      tags$b(style = "margin-left: 2px; margin-right: 2px;"), 
      # tags$div(style = "margin-top: 4px;", 
      # tags$b("Download Sample Data: ", style = "margin-right: 10px;"),
      tags$div(style = "display:inline-block;", 
               style = "margin-top: 3px;;",
               style = "border-style: inset; padding: 3px; border-width:1px;", 
               #   border-width: 3px;
               downloadButton('buttonDownloadDeploymentData', 'Phosphorylation Data'),
               expression_download, 
               downloadButton('buttonDownloadDeploymentMetadata', 'Metadata'),
      )
      #  )
    )
  ), helper_file = helper_file)
}

data_list = c("CWRU-AD-9Month")
selected_data_ = data_list[1]

data_list = c(data_list, "Upload Data")

dataselector <- multiChoicePicker("selected_dataset", 
                                  "Select Dataset to Load", 
                                  data_list, selected = selected_data_, 
                                  isInline = "T", multiple = F, max_opts = 99, 
                                  width = "100%", style_label = "font-size:14.5px;", 
                                  style = "display:flex;flex-direction: column; margin-bottom:6px; align-items: center;", 
                                  style_choices = "text-align:center;", 
                                  picker_inline = F, 
                                  class_names = "abc multiChoicePicker-centeredchoices", 
                                  tooltip = "Select a dataset to load.")

sampleDataDiv <- tags$div(
  class = "inline-block", id = "sample_data_div", 
  style = "margin-bottom:10px;",
  tags$div(style = "margin-top:-8px;", dataselector),
  # tags$b("Select Data: ", style = "margin-right: 10px;"),
  # tags$span(
  #   
  #   # style = "margin-top: 2px;", 
  #   #  style = "border-style: inset; padding: 2px;", 
  #   tags$span(id = "load_sample_data_div", actionButton("buttonSampleData", "Load")),
  #   tags$b(style = "margin-left: 2px; margin-right: 2px;"), 
  #   # tags$div(style = "display:inline-block;", 
  #   #          style = "margin-top: 4px;;",
  #   #          style = "border-style: inset; padding: 3px; border-width:1px;", 
  #   #          #   border-width: 3px;
  #   #          downloadButton('buttonDownloadSampleData', 'Phosphorylation Data'),
  #   #          downloadButton('buttonDownloadSampleExpressionData', 'Expression Data'),
  #   #          downloadButton('buttonDownloadSampleMetaData', 'Metadata'),
  #   # )
  # )
)

dataInputDiv <- tags$div(
  tags$div(style = "margin: 0px; margin-top: 8px;", id = "data_input_div", 
           sampleDataDiv, 
           optionBox(title = "Upload Data", collapsed = F, id = "optionbox_data_input", 
                     # tags$hr(style = "margin-top:12px; margin-bottom:12px;"), 
                     tags$div(id = "upload_data_div", 
                     tags$div(id = "upload_data_div2", 
                     upload_data_helper(
                       tags$div(
                         id = "phospho_data_div", 
                         fileInput("file1", "Upload Oxidation Data:", accept = c(".csv")),
                         tags$style(".shiny-input-container {margin-bottom: 0px} #file1_progress { margin-bottom: 3px } .checkbox { margin-top: 0px}"),
                         tags$style(".checkbox {margin-bottom: 0px;}"),
                       )
                     ),
                     upload_data_helper(tags$div(id = "expression_data_div", 
                       style = "margin-top: 0px; margin-bottom: -8px;",
                       fileInput("file1_expression", "Upload Unmodified Data:", accept = c(".csv")),
                       tags$style(".shiny-input-container {margin-bottom: 0px} #file1_progress { margin-bottom: 3px } .checkbox { margin-top: 0px}"),
                       tags$style(".checkbox {margin-bottom: 0px;}"),
                     ), helper_file = "input_expression_data_format", tooltip = "Unmodified spectral counts. Click to learn the format."),
                     tags$span(helper(
                       tags$div(
                         id = "metadata_upload_div", 
                         style = "margin-top: 0px;", 
                         fileInput("file2", "Upload Metadata:", accept = c(".csv")),
                         tags$style(".shiny-input-container {margin-bottom: 0px} #file2_progress { margin-bottom: 3px } .checkbox { margin-top: 0px}"),
                         tags$style(".checkbox {margin-bottom: 0px;}"),
                       ), type = "markdown", id = "upload_metadata_tooltip_icon",
                       content = "input_metadata_format"
                     )),
                     tippy_this("upload_metadata_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Metadata on samples. Click to learn the format.<span>", allowHTML = TRUE), 
                     tags$div(style = "margin-top: 0px;",
                        multiChoicePicker("refproteome", "Reference Proteome:", c("Uniprot Human", "Uniprot Mouse", "Uniprot Rat"), selected = "Uniprot Mouse"),
                     )
                     ))
           )
  )
)