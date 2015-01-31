#'Generic xml page parsing function
#'
#' @param url The url of the page to parse in the DHIS api, as a character string. The
#' function is made to parse xml pages, so input url should be an xml adress or a
#' generic web adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
parse_page <- function(url, userID, password){
  ## we want to read the xml version of pages
  ## hypothesis based on my use is we will feed either a xml adress or a generic adress
  ## just checking on this and adding the extension if needed
  nchar_url <- nchar(url)
  if(substr(url , nchar_url-3 , nchar_url) != '.xml'){
    url <- paste(url , '.xml' , sep = '')
  }
  print(url)
  userpwd <- paste(userID, password , sep = ':')

  response <- getURL(url, userpwd = userpwd, httpauth = 1L,
                     header=FALSE, ssl.verifypeer = FALSE)
  print('Page reached')

  parsed_page <- xmlTreeParse(response)
  print('Page parsed')

  root <- xmlRoot(parsed_page)
  root
}


#' Extracting the list of datasets in DHIS
#'
#' \code{extract_dhis_datasets} goes to a specific DHIS2 implementation, and extracts
#' its full list of data sets
#'
#' @param url The url of the datasets list in the DHIS web api, as a character string.
#' The function is made to parse xml pages, so input url should be an xml adress or a
#' generic web adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each dataset as a line and for each data set, its
#' unique ID, its name and its url.
extract_dhis_datasets <- function(url , userID , password){
  root <- parse_page(url , userID , password)

  dataset_ID <- xmlSApply(root[['dataSets']] , xmlGetAttr , 'id')
  dataset_name <- xmlSApply(root[['dataSets']] , xmlGetAttr , 'name')
  dataset_url <- xmlSApply(root[['dataSets']] , xmlGetAttr , 'href')

  output <- data.frame(dataset_ID , dataset_name , dataset_url)

  output
}

#'Extract the list of data elements in a DHIS data set
#'
#' \code{extract_data_elements} extracts the data elements recorded in a given dataset.
#'
#' @param url The url of the dataset page in the DHIS api, from which we want to
#' extract the data elements. The function is made to parse xml pages, so input url
#' should be an xml adress or a generic web adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each data element as a line and for each data
#' element, its unique ID, its name and its url.
extract_data_elements <- function(dataset_url, userID, password){
  root <- parse_page(dataset_url , userID , password)
  if (!is.null(root[['dataElements']])){
    data_element_ID <- xmlSApply(root[['dataElements']] , xmlGetAttr , 'id')
    data_element_name <- xmlSApply(root[['dataElements']] , xmlGetAttr , 'name')
    data_element_url <- xmlSApply(root[['dataElements']] , xmlGetAttr , 'href')
  }
  out <- data.frame(data_element_ID , data_element_name , data_element_url)
  out
}




