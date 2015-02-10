#'Extract content information from DHIS
#'
#' \code{extract_dhis_content} extracts content information from DHIS
#'
#' @param base_url The base url of the DHIS2 setting
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a list of seven tables :
#'
#' \strong{data_sets} The list of data sets as extracted by
#' \link{extract_dhis_datasets}.
#'
#' \strong{data_elements} The list of data elements as extracted by
#' \link{extract_data_elements}.
#'
#' \strong{data_elements_categories} The list of categories as extracted by
#' \link{extract_categories}.
#'
#' \strong{org_units_list} The list of organization units as extracted by
#' \link{extract_orgunits_list}.
#'
#' \strong{org_units_description} The description of organization units as extracted by
#' \link{extract_org_unit}.
#'
#' \strong{org_units_group} The list of the groups of organization units as extracted by
#' \link{extract_org_unit}.
#'
#' \strong{org_units_report} The list of reports for each organization unit as extracted by
#' \link{extract_org_unit}.
extract_dhis_content <- function(base_url , userID, password){
  print('Making DHIS urls')
  urls <- make_dhis_urls(base_url)

  print('Extracting Data Sets')
  data_sets <- extract_dhis_datasets(as.character(urls$data_sets_url) ,
                                     userID ,
                                     password)

  print('Extracting Data Elements')
  data_elements <- ddply(data_sets , .(datasets_ID , datasets_name) ,
                         function(data_sets){
                           extract_data_elements(as.character(data_sets$datasets_url) ,
                                                 userID , password)
                           },
                         .progress = 'text')

  print('Extracting Categories')
  data_elements_categories <- extract_categories(as.character(urls$data_elements_categories) ,
                                                 userID ,
                                                 password)


  print('Extracting Organisation Units List')
  org_units_list <- extract_orgunits_list(as.character(urls$org_units_url) ,
                                          userID , password)


  ## Taking out duplicated facilities
  n_units <- ddply(org_units_list  , .(org_unit_ID) , nrow)
  simple_units <- subset(n_units , V1 > 1)

  org_units_list <- subset(org_units_list , !(org_unit_ID %in% simple_units$org_unit_ID))

  print('Extracting units information')
  extracted_orgunits <- dlply(org_units_list , .(org_unit_ID) ,
                            function(org_units_list) {
                              extract_org_unit(as.character(org_units_list$org_unit_url) ,
                                               userID , password)
                              },
                              .progress = 'text'
                            )

  org_units_description <- ldply(extracted_orgunits , function(list) data.frame(list[[1]]))
  org_units_group <- ldply (extracted_orgunits, function(list) data.frame(list[[2]]))
  org_units_report <- ldply (extracted_orgunits, function(list) data.frame(list[[3]]))

  list(data_sets , data_elements , data_elements_categories , org_units_list ,
       org_units_description , org_units_group , org_units_report)
}
