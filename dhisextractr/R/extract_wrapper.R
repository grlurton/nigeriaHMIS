#'Creating the data call adress
#'
#' \code{make_extract_call} creates a url used to call some data
#'
#' @param base_url The base url of the DHIS2 setting
#' @param data_sets A table of data sets, as extracted by \link{extract_dhis_datasets}
#' @param org_unit A table of organization units, as extracted by \link{extract_org_unit}
#' @param period_start Date of the beginning of the period from which to extract data
#' @param period_end Date of the end of the period from which to extract data
#' @return Returns an url that calls on the data to be extracted based on inputted
#' parameters
make_extract_call <- function(base_url , data_sets , org_unit , period_start , period_end){
  data_set_url <- paste('dataSet=' , data_sets$datasets_ID , '&' , collapse = '' , sep = '')
  org_unit_url <- paste('orgUnit=' , org_unit$org_unit_ID , '&' , collapse = '' , sep = '')
  url_call <- paste(base_url , '/api/dataValueSets.xml?' , data_set_url ,
                    org_unit_url ,
                    'startDate=' , period_start , '&endDate=' , period_end, sep = '')
  url_call
}

#'Extracting a data
#'
#' \code{extract_data} extracts data based on a url call
#'
#' @param url_call A data calling url as made by \link{make_extract_call}
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a dataframe with one data value by line, and columns data_element_ID ,
#' period , org_unit_ID , value and category.
extract_data <- function(url_call , userID , password){
  pass <- paste(userID , password , sep = ':')
  response<-getURL(url_call , userpwd=pass , httpauth = 1L ,
                   header=FALSE , ssl.verifypeer = FALSE)

  if(substr(response , 1 , 5) == "<?xml"){
    ParsedPage <- xmlParse(response)
    root <- xmlRoot(ParsedPage)

    data_element_ID <- unlist(as.character(xmlSApply(root, xmlGetAttr, "dataElement")))
    period <- unlist(as.character(xmlSApply(root, xmlGetAttr, "period")))
    org_unit_ID <- unlist(as.character(xmlSApply(root , xmlGetAttr , "orgUnit")))
    value <- unlist(as.character(xmlSApply(root , xmlGetAttr , "value")))
    category <- unlist(as.character(xmlSApply(root , xmlGetAttr , "categoryOptionCombo")))
    last_update <-unlist(as.character(xmlSApply(root , xmlGetAttr , "lastUpdated")))

    out <- data.frame(data_element_ID , period , org_unit_ID , value , category ,
                      last_update)

    out
  }
}





#'Extracting multiple sets of data value
#'
#' \code{extract_all_data} Extracts a data based on list of data sets, organisation units, #' and a period.Can be used to make complete extraction.
#'
#' @param base_url The base url of the DHIS2 setting
#' @param data_sets A table of data sets, as extracted by \link{extract_dhis_datasets}
#' @param org_unit A table of organization units, as extracted by \link{extract_org_unit}
#' @param period_start Date of the beginning of the period from which to extract data
#' @param period_end Date of the end of the period from which to extract data
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns an url that calls on the data to be extracted based on inputted
#' parameters
extract_all_data <- function(base_url , data_sets , org_units , deb_period , end_period ,
                             userID , password){
  extract_data <- ddply(org_units , .(org_unit_ID) ,
                        function(org_units){
                          print(as.character(org_units$org_unit_ID))
                          url_call <- make_extract_call(base_url ,
                                                        data_sets , org_units ,
                                                        deb_period , end_period)
                          out <- data.frame(data_element_ID = org_units$org_unit_ID,
                                            period = '' ,
                                            org_unit_ID = '',
                                            value = '' ,
                                            category = '' ,
                                            last_update = '')
                          try({out <- extract_data(url_call , userID , password)})

                          out
                        } ,
                        .progress = 'win'
  )
  extract_data
}
