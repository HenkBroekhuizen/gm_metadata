GenerateMetadata <- function(data.path, id.prefix, metadata.excel.path = "", target.path = ""){
  # 0 preparation
  # load required packages
  message("Step 0: load packages and functions")
  library(haven)
  library(jsonlite)
  library(readxl)
  # pre-requisite utility functions
  GetAttributeFromExcel <- function(metadata.excel, code, attribute){
    retreived_attribute <- unname(metadata.excel[metadata.excel$varcode == code, attribute])
    return(retreived_attribute)
  }
  
  # 1 load data
  message("Step 1: loading data and metadata excel")
  local_data <- haven::read_sav(data.path)
  metadata.excel <- read_xlsx(metadata.excel.path)
    cat(" Metadata available for ", round(100 * mean(colnames(local_data) %in% metadata.excel$varcode)), "% of data\n")
  
  # 2 initialize list that will be outputted
  out <- NULL
  
  # 3 per column in the dataset, get relevant meta data and store in the list
  # some metadata is retrieved from the .sav file
  # other metadata is retrieved from the metadata excel file
  message("Step 2: retrieving data and metadata")
  pb <- txtProgressBar(1, ncol(local_data))
  for(i in 1:ncol(local_data)){
    setTxtProgressBar(pb, i)
    local_out <- list(
      list(
        varcode = colnames(local_data)[[i]], # column name
        label = attr(local_data[[i]], "label"), # original question in survey
        questionset = GetAttributeFromExcel(metadata.excel, colnames(local_data)[[i]], "questionset"), # NL for nederland or lokaal (eg TW for Twente)
        topic = GetAttributeFromExcel(metadata.excel, colnames(local_data)[[i]], "topic"), # 
        values = unname(attr(local_data[[i]], "labels")), # values of categories, eg '1, 2, 3'
        valuelabels = names(attr(local_data[[i]], "labels")), # label that correspond to values, eg 'yes, no, other'
        missingvalues = GetAttributeFromExcel(metadata.excel, colnames(local_data)[[i]], "value_missings"), # which value if missing, eg '9'
        nvtvalues = GetAttributeFromExcel(metadata.excel, colnames(local_data)[[i]], "value_nvt"), # which value indicated not applicable; meaning respondents did not see the question. eg non-smokers are not asked how many cigs a day they smoke
        column_type = GetAttributeFromExcel(metadata.excel, colnames(local_data)[[i]], "variable_type"), # type of variable, eg proces, indicator, source
        dependency = list(
          conditional_on = GetAttributeFromExcel(metadata.excel, colnames(local_data)[[i]], "conditional_on"), # was the question only asked depending if another question was asked?
          conditional_response = GetAttributeFromExcel(metadata.excel, colnames(local_data)[[i]], "conditional_response")# was the question only asked depending if a particular answer was given to another question?
        ),
        data_type = tail(attr(local_data[[i]], "class"), 1) # technical information about type, eg string or double
      )
    )
    names(local_out) <- paste0(id.prefix, "_", colnames(local_data)[[i]])
    out <- c(out, local_out)
  }
  close(pb)
  
  #  4 convert list to json file
  message("Step 3: converting metadata to JSON")
  out_json <- toJSON(out)
  
  #  5 write json file
  out_path <- paste0(target.path, "/", id.prefix, "_metadata")
  message("Step 4: writing Metadata to ", out_path)
  write_json(out_json, out_path)
  message("Done")
}
