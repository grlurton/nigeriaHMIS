ClassifIndics <- read.csv('classifIndicators.csv' , stringsAsFactors = FALSE)
IndicReport <- read.csv('RepxDE.csv' , stringsAsFactors = FALSE)

##Merging the two to know which indicator is in which report
tot <- merge(ClassifIndics , IndicReport , by.x = 'DEName' , by.y = 'dataElements')

##Recording NIDs for the different datasets
tot$NID[tot$DataSetsName == 'PMTCT Monthly Summary Form'] <- 151860
tot$NID[tot$DataSetsName == 'NHMIS Monthly Summary (version 2013)'] <- 151858
tot$NID[tot$DataSetsName == 'Vaccine Management Monthly Summary'] <-151863
tot$NID[tot$DataSetsName == 'Archived'] <- 151854
tot$NID[tot$DataSetsName == 'NHMIS Quarterly Summary'] <- 151859
tot$NID[tot$DataSetsName == 'Integrated Disease Surveillance and Response(IDSR)'] <- NA
tot$NID[tot$DataSetsName == 'HCT Monthly Summary Form'] <- 151857
tot$NID[tot$DataSetsName == 'ART Monthly Summary Form'] <- 151855
tot$NID[tot$DataSetsName == 'Population Estimates (National Census)'] <- 151861

##Loading Data given By Allie
AlexIndics <- read.csv('J://Project/phc/nga/output/indicators_metadata.csv', stringsAsFactors = FALSE)


##Standardizing variable names
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

##Subsetting DHIS Data to the data included in the IHME Database
MyData <- subset(tot , select = c(indicator_name , indicator_ID , indicator_category , 
                                  indicator_thematic_HIV , indicator_thematic_malaria , 
                                  indicator_thematic_MNCH , indicator_thematic_immunization , 
                                  indicator_unit , indicator_subcategory))

##Putting everything in one table
Indicators <- rbind(AlexIndics , MyData)

##Standardizing categories
Indicators$indicator_category[Indicators$indicator_category == 'Epi'] <- 'Epidemiology'
Indicators$indicator_category[Indicators$indicator_category == 'outcome'] <- 'Outcome'
Indicators$indicator_category[Indicators$indicator_category == 'coverage'] <- 'Coverage'
Indicators$indicator_category[Indicators$indicator_category == 'service'] <- 'Service'
Indicators$indicator_category[Indicators$indicator_category == 'SES'] <- 'Socio-economic Status'

##Function to change 'Y' into 1 (hum... gsub should make it simply)
replacey <- function(x){
  x[x == 'y'] <- 1
  x[x == ''] <- NA
  x
}

##Recode the thematics variables
Indicators$indicator_thematic_MNCH <- replacey(Indicators$indicator_thematic_MNCH)
Indicators$indicator_thematic_HIV <- replacey(Indicators$indicator_thematic_HIV)
Indicators$indicator_thematic_malaria <- replacey(Indicators$indicator_thematic_malaria)
Indicators$indicator_thematic_immunization <- replacey(Indicators$indicator_thematic_immunization)

##Write the resulting table
write.csv(Indicators , 'table_indicators.csv')

##Make matrix of indicators by thematics and category

library(plyr)

matrix <- ddply(Indicators , .(indicator_category) ,
                function(x){
                  nMNCH <- length(unique(x$indicator_name[x$indicator_thematic_MNCH == 1]))
                  MNCH <- data.frame(Thematics = rep('MNCH' , nMNCH) ,
                                     Indicators = unique(
                                       x$indicator_name[x$indicator_thematic_MNCH == 1])
                                     )
                  nHIV <- length(unique(x$indicator_name[x$indicator_thematic_HIV == 1]))
                  HIV <- data.frame(Thematics = rep('HIV' , nHIV) ,
                                    Indicators = unique(
                                      x$indicator_name[x$indicator_thematic_HIV == 1])
                                      )
                  nmalaria <- length(unique(x$indicator_name[x$indicator_thematic_malaria == 1]))
                  malaria <- data.frame(Thematics = rep('malaria' , nmalaria) ,
                                        Indicators = unique(
                                          x$indicator_name[x$indicator_thematic_malaria == 1])
                                          )                                        
                  nimmunization <- length(unique(x$indicator_name[x$indicator_thematic_immunization == 1]))
                  immunization <- data.frame(Thematics = rep('immunization' , nimmunization) ,
                                             Indicators = unique(
                                               x$indicator_name[x$indicator_thematic_immunization == 1])
                                               )
                  out <- rbind(MNCH , HIV , malaria , immunization)
                  out <- subset(out , !is.na(Indicators))
                }
                )


write.csv(matrix , 'indicators_matrix.csv' , row.names = FALSE)
