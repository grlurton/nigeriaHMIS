#' Extracting the list of datasets in DHIS
#'
#' \code{extract_dhis_datasets} goes to a specific DHIS2 implementation, and extracts
#' its full list of data sets
#' This should be the first function to run to explore a DHIS2 instance
#'
#' @param url The url of the datasets list in the DHIS web api, as a character string
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password you password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each dataset as a line and for each data set, its
#' name and url.
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

