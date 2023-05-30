guide <- Cicerone$
  new()$ 
  step("about_tutorial_main_div", 
       "Welcome",
       "This is a quick tutorial to help you get started. Click next to continue."
  )$
  step("main_control_div", 
       "Input & Options",
       "This is main area to specify the options for the analysis. ", 
       position = "right"
  )$
  step("main_control_div2", 
       "Input & Options",
       "We will quickly walk through each of them.",
       position = "right"
  )$
  step(
    "sample_data_div",
    "Dataset Selection",
    "Use this area to select one of the datasets among the listed or choose 'Upload Data' to load your own dataset.", 
  )$
  step(
    "upload_data_div",
    "Upload Data",
    "Use this area to upload an input data. For this purpose, you will need three data files: <br> - Oxidation Data: Spectral counts for each sample and protein. <br> - Unmodified Data: Total spectral count of all peptides for each sample and protein. <br> - Metadata: Specifies the groups of each sample (e.g., Case/Control)",
  )$
  step(
    "phospho_data_div",
    "Oxidation Data Format",
    "The oxidation data is a csv file with the following columns: <br> - <b>Protein</b>: Uniprot ID of the protein. <br> - <b>Samples (multiple columns)</b> Spectral counts for each sample. ",
  )$
  step(
    "expression_data_div",
    "Unmodified Protein Data Format",
    "The protein spectral count data is a csv file with the following columns: <br> - <b>Protein</b>: Uniprot ID of the protein. <br> - <b>Samples (multiple columns)</b> Spectral counts for each sample. ",
  )$
  step(
    "metadata_upload_div",
    "Metadata Format",
    "The metadata is a csv file with the following rows and columns: <br> - <b>RowName (first column):</b> The name of the group specifier. <br> - <b>Samples (multiple columns)</b> The group identities for each sample. <br> - <b>Group (first row):</b> Main group specifying the <em>Case</em>/<em>Control</em> status of the samples. <br> - <b>Other Groups (multiple rows):</b> Optional rows specifying other groups.",
  )$
  step(
    "refproteome_div", 
    "Reference Proteome",
    "Make sure to select the correct reference proteome before uploading the data."
  )$
  step("optionbox_filter_by_subgroup_wrapper", 
       "Subgroup Analysis",
       "You can use this area to filter the samples to focus on particular subgroup. The group variables from metadata will appear here."
  )$
  step("optionbox_filter_by_subgroup_wrapper2", 
       "Grouping Variables",
       "The grouping variables from sample data appears here. There are three for sample data: <br> - Timepoint, Gender, and Replicate <br> You can choose any combination of them to customize the analysis.",
       position = "right", 
  )$
  step("optionbox_subgroup_differences_wrapper", 
       "Subgroup Differences",
       "You can use this area to further customize the analysis, specifying two subgroups to identify proteins that exhibit the largest difference between those subgroups. ",
       position = "right", 
  )$
  step("optionbox_options_wrapper", 
       "(Optional) Analysis Options",
       "You can use this area to modify the options regarding data pre-processing and statistical inference."
  )$
  step("main_output_div", 
       "Analysis Results",
       "The analysis results are displayed in this section. Each tab contains the results of a different analysis module: <br> - MetO Results: Identify proteins with significant changes in methionine oxidation <br> - Enrichment: Perform enrichment analysis on gene ontology (GO) terms based on significantly oxidized proteins. <br> - Report Generator: Export the results as formatted, visually appealing Excel files.",
       class = "analysis_results_tutorial_cicerone1", 
       position = "bottom", 
  )$
  step("main_output_div2", 
       "Oxidation Analysis",
       "The results of the oxidation analysis are presented in the form of five types of views: Volcano plots, Bar plots, Heatmaps, and Tables.",
       class = "analysis_results_tutorial_cicerone1", 
       position = "bottom", 
  )$
  step(
    "[data-value='Volcano Plot']",
    "Volcano plot tab",
    "Select this tab to view the volcano plots.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_volcanoplot', {priority: 'event'});}", 
    is_id = FALSE
  )$
  step("protexpression_volcano_wrapper_div", 
       "Volcano plots",
       "Volcano plots are the first view for each analysis, providing an overview of the results and about the significance of the findings.",
  )$
  step("protexpression_volcano_analysisopts_div", 
       "Analysis Specific Options",
       "Volcano plot tab also harbors a panel containing the analysis specific options, such as cutoffs on p-value or other filtering options. These options also affect the results presented in other views.",
  )$
  step(
    "[data-value='Bar Plot']",
    "Bar plot tab",
    "Select this tab to view the bar plots.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_barplot', {priority: 'event'});}", 
    is_id = FALSE
  )$
  step(
    el = "protexpression_barplot_wrapper_div",
    title = "Bar Plot",
    description = "This view with the bar plot focuses on visualizing the top findings. You can use the options below to further customize the plot and download it as an image or PDF."
    #on_highlight_started = "Shiny.setInputValue('foo2', 'qfds', {priority: 'event'});",
    #tab = "Plot",
    #tab_id = "mainTabset"
  )$
  step(
    "[data-value='Heatmap']",
    "Heatmap tab",
    "Select this tab to view the heatmaps.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_heatmap', {priority: 'event'});}", 
    is_id = FALSE
  )$
  step(
    el = "protexpression_heatmap_wrapper_div",
    title = "Heatmap",
    description = "This view with the heatmap again focuses on the top findings, but displays more detailed, sample-specific information. Similar to bar plots, you can use the options below to further customize the plot and download it as an image or PDF.",
    position = "bottom", 
  )$
  step(
    "[data-value='Table']",
    "Table tab",
    "Select this tab to view the results in the form of a table.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_table', {priority: 'event'});}", 
    is_id = FALSE,
  )$
  step(
    el = "protexpression_table_div",
    title = "Oxidation Table",
    description = "The table displays detailed information about the statistical analysis and the significance, sorted by proteins exhibiting highest dysregulation to the least. Using the button on the top, you can export this table to Excel or as a CSV file. ",
    class = "analysis_results_tutorial_cicerone", 
    position = "bottom", 
  )$
  step(
    el = "protExpressionTabset",
    title = "Views Summary",
    description = "To summarize, use <b> volcano plots</b> to determine the significant items, <b> bar plots</b> and <b>heatmaps</b> to visualize top findings, use <b>tables</b> for detailed view and to export the results. Next, we will focus on Enrichment analysis.",
    class = "analysis_results_tutorial_cicerone", 
    position = "bottom", 
  )$step(
    "[data-value='Enrichment']",
    "Enrichment Tab",
    "Select this tab to view the enrichment results.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_enrichment', {priority: 'event'});}", 
    is_id = FALSE,
  )$
  step(
    el = "enrichmentTabset",
    title = "Views for Enrichment Analysis",
    description = "The enrichment analysis provides a subset of the same views: <b> Volcano plot</b>, <b>Table</b> for displaying and downloading the analysis results, and <b>Targets</b> to display information about the proteins related to the enrichment term. In addition, <b>Settings</b> tab contains various options on how the enrichment analysis should be performed, as well as based on which data source (i.e., phosphosites/phosphoproteins/protein expression).",
    class = "analysis_results_tutorial_cicerone", 
    position = "bottom", 
  )$
  step(
    "optionbox_enrichment_inclusion_criteria",
    "Enrichment Settings - Inclusion Criteria",
    "The options in this panel determines the inclusion criteria for the enrichment terms. This can be based on category of the terms, number of identified proteins related to a term, or the ratio of the observed proteins related to a term. Additionally, it includes filtering options exclude highly similar terms from the analysis.",
    class = "analysis_results_tutorial_cicerone1", 
  )$
  step(
    "optionbox_enrichment_background_set_proteins",
    "Enrichment Settings - Background Set",
    "The options in this panel determines the background set of proteins deemed as significant based on various cutoffs. When an enrichment term includes a significant protein in its set, it is considered a <em>Hit</em>, otherwise it is considered a <em>Miss</em>.",
    class = "analysis_results_tutorial_cicerone1", 
  )$step(
    "[data-value='Report Generator']",
    "Report Generator Tab",
    "Select this tab to display the report generator.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_reportgenerator', {priority: 'event'});}", 
    is_id = FALSE,
  )$step(
    "report_generator_div",
    "Report Generator",
    "The Report Generator in MetO-Xplorer simplifies the process of analyzing data for multiple subgroups and exporting the results as formatted excel tables. Whether you want to investigate the impact of variables like gender or tissue of the sample, this feature enables you to perform separate analyses for each subgroup effortlessly.",
    class = "analysis_results_tutorial_cicerone1", 
  )$step(
    "report_generator_options",
    "Report Generator Options",
    "Use this area to choose analysis type (Methionine Oxidation or Enrichment) and define grouping variables for subgroup analysis.",
  )$step(
    "report_generator_buttons",
    "Exporting the results",
    "Once you set the desired options for report generator, click 'Run' and download the generated Excel report.",
  )$
  step(
    "[data-value='About']",
    "End of Tutorial",
    "This is the end of the tutorial. Hope you enjoyed it! Click on the About tab to return to the home page.",
    is_id = FALSE,
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_about_end', {priority: 'event'});}", 
  )

observeEvent(input$interactiveTutorialLink, {
  main_logging("Interactive Tutorial")
  guide$init()$start()
})

observeEvent(guide$get_next(), {
  a <- guide$get_next()
  if(!is.null(a)){
    b <- a$highlighted
    if(b == "load_sample_data_div"){
      foLoadSampleData()
    }
    if((b == "main_output_div") || (b == "main_output_div2")){
      updateTabsetPanel(session, "mainTabset", "MetO Results");
      updateTabsetPanel(session, "protExpressionTabset", "Volcano Plot");
    }
    if(b == "main_control_div2"){
      foUncollapseBoxIfNeeeded("optionbox_data_input", nullval = F)
      foUncollapseBoxIfNeeeded("optionbox_filter_by_subgroup")
      foUncollapseBoxIfNeeeded("optionbox_subgroup_differences")
      foUncollapseBoxIfNeeeded("optionbox_options")
      foUncollapseBoxIfNeeeded("config_optionbox")
      # val = input[["mortgage_optionbox_collapse"]]
      # if(!is.null(val)){
      #   js$collapse("mortgage_optionbox")
      # }
    }
    if(b == "config_optionbox_wrapper"){
      foUncollapseBoxIfNeeeded("optionbox_filter_by_subgroup", collapse = T)
      foUncollapseBoxIfNeeeded("optionbox_subgroup_differences", collapse = T)
      foUncollapseBoxIfNeeeded("optionbox_options", collapse = T)
      foUncollapseBoxIfNeeeded("config_optionbox", collapse = T)
    }
  }
})

observeEvent(input$foo2, {
  switch(input$foo2,
         "step_volcanoplot" = {
           updateTabsetPanel(session, "mainTabset", "MetO Results");
           updateTabsetPanel(session, "protExpressionTabset", "Volcano Plot")},
         "step_barplot" = {
           updateTabsetPanel(session, "mainTabset", "MetO Results");
           updateTabsetPanel(session, "protExpressionTabset", "Bar Plot")},
         "step_heatmap" = {
           updateTabsetPanel(session, "mainTabset", "MetO Results");
           updateTabsetPanel(session, "protExpressionTabset", "Heatmap")},
         "step_table" = {
           updateTabsetPanel(session, "mainTabset", "MetO Results");
           updateTabsetPanel(session, "protExpressionTabset", "Table")},
         "step_enrichment" = {
           updateTabsetPanel(session, "mainTabset", "Enrichment");
           updateTabsetPanel(session, "enrichmentTabset", "Settings")},
         "step_reportgenerator" = {
           updateTabsetPanel(session, "mainTabset", "Report Generator");
         },
         "step_about_end" = {
           updateTabsetPanel(session, "mainTabset", "About");
           updateTabsetPanel(session, "aboutTabset", "Welcome")}
  )
  #updateTabsetPanel(session, "aboutTabset", "How to cite us?")
  #message(paste("xyzds - ", input$foo2, sep = ""))
})