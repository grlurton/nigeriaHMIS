extract_dhis_content <- function(url , userID, password , output = getwd() ){
  print('Making DHIS urls')
  urls <- make_dhis_urls(url)

  print('Extracting Data Sets')
  data_sets <- extract_dhis_datasets(as.character(urls$data_sets_url) ,
                                     userID ,
                                     password)

  print('Extracting Data Elements')
  data_elements <- ddply(data_sets , .(datasets_ID , datasets_name) ,
                         function(data_sets){
                           print(as.character(data_sets$datasets_ID))
                           extract_data_elements(as.character(data_sets$datasets_url) ,
                                                 userID , password)
                           },
                         .progress = 'text')

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

  list(data_sets , data_elements , org_units_list ,
       org_units_description , org_units_group , org_units_report)
}




