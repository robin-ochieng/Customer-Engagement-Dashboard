# data_processing.R

read_and_process_data_motor <- function(filepath) {
  Data <- read_excel(filepath, col_types = c("date", "text", "text", "text", "text", "text", "text", "text", "text"))

  
  return(Data)
}




read_and_process_data_medical <- function(filepath) {
  Data <- read_excel(filepath, col_types = c("date",  "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text"))

  
  return(Data)
}


