make_extract_call <- function(base_url , data_sets , org_unit , period_start , period_end){
  data_set_url <- paste('dataSet=' , data_sets$datasets_ID , '&' , collapse = '' , sep = '')
  org_unit_url <- paste('orgUnit=' , org_unit$org_unit_ID , '&' , collapse = '' , sep = '')
  url_call <- paste(base_url , '/api/dataValueSets.xml?' , data_set_url ,
                    org_unit_url ,
                    'startDate=' , period_start , '&endDate=' , period_end, sep = '')
  url_call
}

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

extract_all_data <- function(base_url , data_sets , org_units , deb_period , end_period ,
                             userID , password){
  extract_data <- ddply(org_units , .(org_unit_ID) ,
                        function(org_units){
                          print(as.character(org_units$org_unit_ID))
                          url_call <- make_extract_call(base_url ,
                                                        data_sets , org_units ,
                                                        deb_period , end_period)
                          extract_data(url_call , userID , password)
                        } ,
                        .progress = 'win'
  )
  extract_data
}




