current_dataset <- reactive({
  req(reactive_dataset())
  library(tidyverse)
  Tx <- reactive_dataset()
  
  Ts <- as.matrix(Tx %>% select(2:ncol(Tx)))
  
  ## Convert to Numeric - Capture warning (possibly)
  cnames = colnames(Ts)
  numcols = ncol(Ts)
  Ts <- matrix(as.numeric(Ts), ncol = numcols)
  colnames(Ts) <- cnames
  
  ST <- (Tx %>% select(1:1))
  ST$Position = ""
  # ST$Type = "phosphorylation"
  
  #T$ID = paste(T$Protein, T$Position, sep="_")
  return (list("Ts" = Ts, "ST" = ST))
})

is.nill <- function(x){
  if(is.null(x)){
    return(TRUE);
  }
  if(length(x) == 0){
    return(TRUE);
  }
  if(is_scalar_atomic(x)){
    if(is.na(x)){
      return(TRUE);
    }
  }
  if(identical(x, "")){
    return(TRUE);
  }
  return(FALSE);
}

current_expression_dataset <- reactive({
  req(reactive_expression_dataset())
  library(tidyverse)
  if(is.nill(reactive_expression_dataset())){
    return(NULL)
  }
  Tx <- reactive_expression_dataset()
  
  Ts <- as.matrix(Tx %>% select(2:ncol(Tx)))
  
  ## Convert to Numeric - Capture warning (possibly)
  cnames = colnames(Ts)
  numcols = ncol(Ts)
  Ts <- matrix(as.numeric(Ts), ncol = numcols)
  colnames(Ts) <- cnames
  
  ST <- (Tx %>% select(1:1))
  ST$Position = ""
  # ST$Type = "expression"
  return (list("Ts" = Ts, "ST" = ST))
})

current_metadata <- reactive({
  req(reactive_metadata())
  library(tidyverse)
  T_metadata <- reactive_metadata()
  
  colNames <- colnames(T_metadata)
  hasRowNames <- colNames[1] == "RowName"
  validate(
    need(hasRowNames, "The first column of the metadata should be the row names.")
  )
  rownames(T_metadata) <- T_metadata$RowName
  T_metadata <- T_metadata %>% select(2:ncol(T_metadata))
  rowNames <- rownames(T_metadata)
  hasGroup <- rowNames[1] == "Group"
  
  validate(
    need(hasGroup, "The first row of the metadata should be 'Group' indicating the Case/Control status of the samples.")
  )
  
  group <- tolower(as.character(T_metadata["Group", ]))
  group_vals <- tolower(unique(group))
  nCase = sum(group == "case")
  nControl = sum(group == "control")
  nSample = ncol(T_metadata)
  
  validate(
    need(nCase > 0, "There should be at least one case sample in metadata."),
    need(nControl > 0, "There should be at least one control sample in metadata."), 
    need((nCase + nControl), "The group of each sample should be either case or control.")
  )
  
  caseSamples <- group == "case"
  
  x <- list()
  x$nSample <- nSample
  x$caseSamples <- caseSamples
  if(nrow(T_metadata) >= 2){
    x$Tsample_metadata <- T_metadata[2:nrow(T_metadata), ]
  } else {
    x$Tsample_metadata <- T_metadata[0, ]
  }
  
  metadata_ready(TRUE)
  
  return(x)
})

current_dataset_mapped <- reactive({
  req(current_dataset())
  req(metadata_ready())
  
  ds <- current_dataset()
  x <- current_metadata()
  
  validate(
    need(x$nSample == ncol(ds$Ts), 
         "The number of samples in the data should be the same as in the metadata."),
  )
  
  validate(
    need(sum(is.na(match(colnames(x$Tsample_metadata), colnames(ds$Ts)))) == 0, 
         "The samples in the data should match the metadata.")
  )
  
  ## Match the columns to metadata
  indices = match(colnames(x$Tsample_metadata), colnames(ds$Ts))
  ds$Ts = ds$Ts[, indices]
  
  ## Match the expression data if available
  if(!is.nill(current_expression_dataset())){
    ds2 <- current_expression_dataset()
    
    validate(
      need(ncol(ds2$Ts) == ncol(ds$Ts), 
           "The number of samples in the unmodified data should be the same as in the oxidation data."),
    )
    
    validate(
      need(sum(is.na(match(colnames(ds2$Ts), colnames(ds$Ts)))) == 0, 
           "The samples in the oxidation data should match the unmodified data")
    )
    
    indices = match(colnames(ds$Ts), colnames(ds2$Ts))
    
    row_indices = match(ds$ST$Protein, ds2$ST$Protein)
    valid_rows = !is.na(row_indices)
    
    validate(
      need(sum(valid_rows) > 0, 
           "The proteins in oxidation data should match the unmodified data")
    )
    ds$Ts = ds$Ts[valid_rows, ]
    ds$ST = ds$ST[valid_rows, ]
    row_indices = row_indices[valid_rows]
    
    if(!identical(input$options_transformation, "Ratio of Spectral Counts")){
      ds$Ts = ds$Ts / ds2$Ts[row_indices, indices]
      ds$Ts[is.infinite(ds$Ts)] = NA
      ds$Ts[ds$Ts <= 0] = NA
      ds$Ts[ds$Ts > 1] = NA
    }
    
    # ds$Ts = rbind(ds$Ts, ds2$Ts[row_indices, indices])
    # ds$ST = rbind(ds$ST, ds2$ST)
    
  }
  
  req(reactive_network())
  NetworkData <- reactive_network()
  
  proteins = ds$ST$Protein
  
  indices = match(proteins, NetworkData$UniprotGene$ID)
  ds$ST$ProteinName <- NetworkData$UniprotGene$Gene[indices]
  ds$ST$Position = ""
  
  nameX = ds$ST$ProteinName
  nameX[is.na(nameX)] = ds$ST$Protein[is.na(nameX)]
  ds$ST$Identifier = nameX
  
  # ds$ST$Identifier <- str_c(nameX, ds$ST$Position, sep = "-")
  # pos = gsub('\\D+','', ds$ST$Position)
  # ids = str_c(ds$ST$Protein, pos, sep = "_")
  # ds$ST$ID = ids
  ds$ST$ID = ds$ST$Protein
  
  ### TODO: Change the error check/mapping style from sites to proteins
  validate(
    need(nnzero(!is.na(indices))>0, "Input mapping failed. Please check if the correct reference proteome is selected.")
  )
  
  if(identical(myvalue(), "upload")){
    main_logging("Uploaded dataset successfully parsed")
  }
  
  return(ds)
})

is_expression_data_available <- reactive({
  req(current_dataset_mapped())
  ds <- current_dataset_mapped()
  return(!is.na(match("expression", ds$ST$Type)))
})

