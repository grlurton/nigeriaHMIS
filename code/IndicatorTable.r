ClassifIndics <- read.csv('classifIndicators.csv' , stringsAsFactors = FALSE)
IndicReport <- read.csv('RepxDE.csv' , stringsAsFactors = FALSE)

tot <- merge(ClassifIndics , IndicReport , by.x = 'DEName' , by.y = 'dataElements')

tot$NID[tot$DataSetsName == 'PMTCT Monthly Summary Form'] <- 151860
tot$NID[tot$DataSetsName == 'NHMIS Monthly Summary (version 2013)'] <- 151858
tot$NID[tot$DataSetsName == 'Vaccine Management Monthly Summary'] <-151863
tot$NID[tot$DataSetsName == 'Archived'] <- 151854
tot$NID[tot$DataSetsName == 'NHMIS Quarterly Summary'] <- 151859
tot$NID[tot$DataSetsName == 'Integrated Disease Surveillance and Response(IDSR)'] <- NA
tot$NID[tot$DataSetsName == 'HCT Monthly Summary Form'] <- 151857
tot$NID[tot$DataSetsName == 'ART Monthly Summary Form'] <- 151855
tot$NID[tot$DataSetsName == 'Population Estimates (National Census)'] <- 151861

AlexIndics <- read.csv('J://Project/phc/nga/output/indicators_metadata.csv', stringsAsFactors = FALSE)

colnames(tot) <- c("indicator_name" , "indicator_ID" , "indicator_category" , 
                   "indicator_thematic_HIV" , "indicator_thematic_malaria" , 
                   "indicator_thematic_MNCH" , "indicator_thematic_immunization" , 
                   "indicator_unit" , "indicator_subcategory" , "X" , "DataSet" , "NID"
                   )

colnames(AlexIndics) <- c("indicator_ID" , "indicator_name" , "indicator_unit" ,
                          "indicator_category" , "indicator_subcategory" ,
                          "indicator_thematic_MNCH" ,
                          "indicator_thematic_HIV" , "indicator_thematic_malaria" , 
                          "indicator_thematic_immunization"
                          )

MyData <- subset(tot , select = c(indicator_name , indicator_ID , indicator_category , 
                                  indicator_thematic_HIV , indicator_thematic_malaria , 
                                  indicator_thematic_MNCH , indicator_thematic_immunization , 
                                  indicator_unit , indicator_subcategory))

Indicators <- rbind(AlexIndics , MyData)

table(Indicators$indicator_category)
Indicators$indicator_category[Indicators$indicator_category == 'Epi'] <- 'Epidemiology'
Indicators$indicator_category[Indicators$indicator_category == 'outcome'] <- 'Outcome'
Indicators$indicator_category[Indicators$indicator_category == 'coverage'] <- 'Coverage'
Indicators$indicator_category[Indicators$indicator_category == 'service'] <- 'Service'
Indicators$indicator_category[Indicators$indicator_category == 'SES'] <- 'Socio-economic Status'


replacey <- function(x){
  x[x == 'y'] <- 1
  x[x == ''] <- NA
  x
}

Indicators$indicator_thematic_MNCH <- replacey(Indicators$indicator_thematic_MNCH)
Indicators$indicator_thematic_HIV <- replacey(Indicators$indicator_thematic_HIV)
Indicators$indicator_thematic_malaria <- replacey(Indicators$indicator_thematic_malaria)
Indicators$indicator_thematic_immunization <- replacey(Indicators$indicator_thematic_immunization)


write.csv(Indicators , 'table_indicators.csv')
