#' Extracting the list of datasets in DHIS
#'
#' \code{extract_dhis_datasets} goes to a specific DHIS2 implementation, and extracts
#' its full list of data sets
#'
#' @param url The url of the datasets list in the DHIS web api, as a character string
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password you password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each dataset as a line and for each data set, its
#' unique ID, its name and its url.
extract_dhis_datasets <- function(url , userID , password){
  userpwd <- paste(userID, password , sep = ':')
  response <- getURL(url, userpwd = userpwd, httpauth = 1L,
                     header=FALSE, ssl.verifypeer = FALSE)
  print('Page reached')

  parsed_page <- xmlParse(response)
  print('Page parsed')

  root <- xmlRoot(parsed_page)

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
#' #'extract the data elements.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password you password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each data element as a line and for each data
#' element, its unique ID, its name and its url.
extract_data_elements <- function(dataset_url, userID, password){
  userpwd <- paste(userID, password, sep = ':')
  url <- paste(dataset_url, '.xml', sep = '')
  print(url)

  response <- getURL(url,userpwd = userpwd,
                   httpauth = 1L, header = FALSE,ssl.verifypeer = FALSE)
  print('Page reached')

  ParsedPage <- xmlParse(response)
  print('Page parsed')

  root <- xmlRoot(ParsedPage)
  if (!is.null(root[['dataElements']])){
    data_element_ID <- xmlSApply(root[['dataElements']] , xmlGetAttr , 'id')
    data_element_name <- xmlSApply(root[['dataElements']] , xmlGetAttr , 'name')
    data_element_url <- xmlSApply(root[['dataElements']] , xmlGetAttr , 'href')
  }

  out <- data.frame(data_element_ID , data_element_name , data_element_url)
}
