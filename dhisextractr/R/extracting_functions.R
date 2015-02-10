#'Generic xml page parsing function
#'
#' @param url The url of the page to parse in the DHIS api, as a character string. The
#' function is made to parse xml pages, so input url should be an xml adress or a
#' generic web adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @param xml wheter the url should end with '.xml'. This has been added to add some
#' flexibility in the kind of parsable urls.
parse_page <- function(url, userID, password , xml = TRUE){
  ## we want to read the xml version of pagse
  ## hypothesis based on my use is we will feed either a xml adress or a generic adress
  ## just checking on this and adding the extension if needed
  url <- as.character(url)
  nchar_url <- nchar(url)
  if(substr(url , nchar_url-3 , nchar_url) != '.xml' && xml == TRUE){
    url <- paste(url , '.xml' , sep = '')
  }
  userpwd <- paste(userID, password , sep = ':')

  response <- getURL(url, userpwd = userpwd, httpauth = 1L,
                     header=FALSE, ssl.verifypeer = FALSE)

  parsed_page <- xmlTreeParse(response)

  root <- xmlRoot(parsed_page)
  root
}


#'Generic function to extract relevant nodes in DHIS element
#' \code{extract_info} goes to a specific DHIS2 element url, and extracts attributes
#' name , id and href. It can extract elements that span on multiple pages
#'
#' @param url_page The default url of the elemtns to parse in the DHIS api, as a
#' character string function is made to parse xml pages, so input url should be an xml
#' adress or a generic web adress without extension.
#' @param root root of this page, as extracted by \code{\link{parse_page}}
#' @param node_name the name of the name we wish to extract
#' @param out an empty dataframe in which to return the output (there are more elegant ways to do it for sure, see it later).
extract_info <- function(url_page , root , node_name , out , userID , password , monitor = FALSE){
  NPages <- as.numeric(xmlValue(root[['pager' ]][['pageCount']]))
  NPages[is.na(NPages)] <- 1
  for (page in 1:NPages){
    ID <- name <- url <- ''
    if(monitor == TRUE){
      print(paste('Parsing page' , page , 'out of' , NPages , sep = ' '))
    }
    url_read <- paste(url_page , '.xml?page=' , page , sep = '')
    root <- parse_page(url_read , userID , password , xml = FALSE)
    if (!is.null(root[[node_name]]) & length(root[[node_name]]) > 0){
      ID <- xmlSApply(root[[node_name]] , xmlGetAttr , 'id')
      name <- xmlSApply(root[[node_name]] , xmlGetAttr , 'name')
      url <- xmlSApply(root[[node_name]] , xmlGetAttr , 'href')
    }
    loop_out <- data.frame(ID , name , url)
    colnames(loop_out) <- colnames(out)
    out <- rbind(out , loop_out)
  }
  out
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
  out <- data.frame(datasets_ID = character() ,
                    datasets_name = character()  ,
                    datasets_url = character() )
  extract_info(url , root , 'dataSets' , out , userID , password)
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
  out <- data.frame(data_element_ID = character() ,
                    data_element_name = character()  ,
                    data_element_url = character() )
  extract_info(dataset_url , root , 'dataElements' , out , userID , password)
}

#'Extract the list of Organisation Units in the DHIS setting
#'
#' \code{extract_orgunits_list} extracts the list of Organisation Units recorded in a
#' DHIS setting
#'
#' @param url The url of the organisation units page in the DHIS api. The function is
#' made to parse xml pages, so input url should be an xml adress or a generic web
#' adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each organisation unit as a line and for each
#' organisation unit, its unique ID, its name and its url.
extract_orgunits_list <- function(org_unit_page_url, userID, password){
  out <- data.frame(org_unit_ID = character() ,
                    org_unit_name = character()  ,
                    org_unit_url = character() )
  root <- parse_page(org_unit_page_url , userID , password)
  extract_info(org_unit_page_url , root , 'organisationUnits' , out , userID , password , TRUE)
}


#'Extract information about an Orgunit
#'
#' \code{extract_org_unit} extracts all the information about
#'
#' @param url The url of the organisation unit for which we want to extract
#' information. The function is made to parse xml pages, so input url should be an xml
#' adress or a generic web adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a list with three elements :
#' * __Metadata__ For each organization unit, includes its geolocalization and
#' reference to parent unit
#'
#' * __Group __ Groups in which the organization unit is included. This is where the
#' type of organization unit is stored
#'
#' * __Datasets__ Datasets for which the organisation unit should communicate data
extract_org_unit <- function(org_unit_url, userID, password){
  root <- parse_page(org_unit_url , userID , password)

  ##Extraction of org units metadata
    parent_id <- parent_name <- parent_url <- NA

    id <- xmlAttrs(root)[['id']]
    coordinates <- xmlValue(root[['coordinates']])
    opening_date <- xmlValue(root[['openingDate']])
    name <- xmlValue(root[['displayName']])
    active <- xmlValue(root[['active']])
  if (!is.null(root[['parent']])){
    parent_id <- xmlAttrs(root[['parent']])[['id']]
    parent_name <- xmlAttrs(root[['parent']])[['name']]
    parent_url <- xmlAttrs(root[['parent']])[['href']]
  }
  org_unit_metadata <- data.frame(id , coordinates , opening_date , name ,
                                  active , parent_id , parent_name , parent_url)

  ##Extraction of org units groups
  org_unit_group <- data.frame(group_ID = character() , group_name = character() ,
                               group_url = character())
  if (!is.null(root[['organisationUnitGroups']])){
    Groups <- root[['organisationUnitGroups']]
    group_ID <- xmlSApply(Groups , xmlGetAttr , 'id')
    group_name <- xmlSApply(Groups , xmlGetAttr , 'name')
    group_url <- xmlSApply(Groups , xmlGetAttr , 'href')
    org_unit_group <- data.frame(group_ID , group_name , group_url)
  }

  ##Extraction of org units datasets
  org_unit_dataset <- data.frame(dataset_ID = character() ,
                                 dataset_name = character() ,
                                 dataset_url = character())
  if (!is.null(root[['dataSets']])){
    Datasets <- root[['dataSets']]
    dataset_ID <- xmlSApply(Datasets , xmlGetAttr , 'id')
    dataset_name <- xmlSApply(Datasets , xmlGetAttr , 'name')
    dataset_url <- xmlSApply(Datasets , xmlGetAttr , 'href')
    org_unit_dataset <- data.frame(dataset_ID , dataset_name , dataset_url)
  }

  out <- list(org_unit_metadata , org_unit_group , org_unit_dataset)
  out
}

#'Extract the categories for data elements
#'
#' \code{extract_categories} extracts the list of categories that are used for different
#' data elements.
#'
#' @param categories_url The url of the categories page in the DHIS api. The function is
#' made to parse xml pages, so input url should be an xml adress or a generic web
#' adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each category as a line and for each
#' category, its unique ID, its name and its url.
extract_categories <- function(categories_url, userID, password){
  out <- data.frame(org_unit_ID = character() ,
                    org_unit_name = character()  ,
                    org_unit_url = character() )
  root <- parse_page(categories_url , userID , password)
  extract_info(categories_url , root , 'categoryOptionCombos' , out , userID , password)
}


#'Make relevant urls in DHIS web api
#'
#' \code{make_dhis_urls} takes the main adress of a DHIS implementation and returns
#' the relevant adresses in the web api that will be used for extracting data.
#'
#' @param base_url The url of the DHIS implementation
make_dhis_urls <- function(base_url){
  data_sets_url <- paste(base_url , '/api/dataSets' , sep = '')
  data_elements_url <- paste(base_url , '/api/dataElements' , sep = '')
  org_units_url <- paste(base_url , '/api/organisationUnits' , sep = '')
  data_elements_categories <- paste(base_url , '/api/categoryOptionCombos' , sep = '')
  data.frame(data_sets_url , data_elements_url , data_elements_categories , org_units_url)
}
