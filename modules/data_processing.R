# data_processing.R

read_and_process_data_motor <- function(filepath) {
  Data <- read_excel(filepath, col_types = c("date", "text", "text", "text", "text", "text", "text", "text", "text"))

  
  return(Data)
}




read_and_process_data_medical <- function(filepath) {
  Data <- read_excel(filepath, col_types = c("date",  "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text"))

  
  return(Data)
}


read_and_process_data_drysalesmotor <- function(filepath) {
  Data <- read_excel(filepath, sheet = "MOTOR", col_types = c("date", "text", "text", "text", "numeric", "numeric", "numeric", "text", "text", "text"))

  
  return(Data)
}



read_and_process_data_drysalesmedical <- function(filepath) {
  Data <- read_excel(filepath, sheet = "MEDICAL", col_types = c("date", "text", "text", "text", "numeric", "numeric", "numeric", "text", "text", "text"))

  
  return(Data)
}




read_and_process_data_claims <- function(filepath) {
  Data <- read_csv(filepath, show_col_types = FALSE)

  
  return(Data)
}


