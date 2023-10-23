LoadData <- function(
  data.path, # TODO: replace with name and year, and then query meta data
  metadata.path, # TODO: this should come from meta data
  select.columns, # strings with exact boolean expressions eg topic==mentale gezondheid seperated by semicolons
  select.rows
){
  # required packages and prerequisite functions
  library(haven)
  library(tidyverse)
  library(jsonlite)
  library(rrapply)
  # load data and load+shape metadata
  local_data <- read_sav(data.path)
  meta_data <- read_json(metadata.path)[[1]] %>% fromJSON %>% rrapply(how = "melt") # alternatief is bind
  
  # select columns
  selections <- strsplit(select.columns, ";")
  selections <- strsplit(selections[[1]], "==")
  selected_columns <- NULL
  for(i in 1:length(selections)){
    my_column <- which.max(colSums(meta_data == selections[[i]][1]))
    found_columns <- grepl(selections[[i]][1], meta_data[, my_column], T) & grepl(selections[[i]][2], meta_data$value, T)
    found_columns_ids <- meta_data[found_columns,]$L1
    found_varcodes <- meta_data$value[meta_data$L1 %in% found_columns_ids & meta_data$L2 == "varcode"]
    if(length(found_varcodes) > 0){
      selected_columns <- c(selected_columns, unlist(found_varcodes))
      cat("adding columns: ", paste(found_varcodes, collapse = ","), "\nfor selection ", selections[[i]][1], "==", selections[[i]][2])
    }
  }
  
  # select rows
  selected_rows <- 1:nrow(local_data) # placeholder
  
  selected_data <- local_data[, selected_columns]
  
  return(selected_data)
}