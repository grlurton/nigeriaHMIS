setwd('C://users/grlurton/Desktop/dhis')

library(plyr)

## Load and format data

data_fac <- read.csv('data_fac.csv')

data_fac$data_facElement <- as.character(data_fac$data_facElement)
data_fac$period <- as.Date(as.yearmon(data_fac$period, "%Y%m"))

qplot(data_fac = data_fac , x = period , binwidth = 30) +
  theme_bw()

data_fac <- subset(data_fac , orgUnit %in% OrgUnitsHierarchy$Level5ID)


## Function to take out elements that are duplicated (for security, no reason that this
## should happen)





indics <- read.csv('table_indicators.csv')
orgUnits <- read.csv('table_location.csv')
OrgUnitsHierarchy <- read.csv('Hierarchydata_fac.csv')


org_x_datasets <- read.csv('OrgUnitxDataSets.csv')
report_x_datasets <- read.csv('RepxDE.csv')


data_elements_x_orgunit <- merge(org_x_datasets , report_x_datasets ,
                                 by.x = 'DataSetsName' , by.y = 'DataSetsName')
colnames(data_elements_x_orgunit) <- c('data_sets' , 'x' , 'org_unit_ID' ,
                                       'y' , 'data_element')
data_elements_x_orgunit <- subset(data_elements_x_orgunit ,select = -c(x,y))


data_elements_x_orgunit <- merge(data_elements_x_orgunit , 
                                 OrgUnitsHierarchy_unique , 
                                 by.x = "org_unit_ID" ,
                                 by.y = "Level5ID" ,
                                 all.x = TRUE , all.y = FALSE)

data_elements_x_orgunit <- subset(data_elements_x_orgunit , 
                                  select = -c(data_sets , X , Level4 , 
                                              Level3 , Level1 , Level5 ,
                                              Level3ID , Level4ID , toMatch ,
                                              Level6ID))

#load('NigeriaWD_indicmap.Rdata')

malaria <- c("Children < 5 years with uncomplicated malaria - female","Individuals >= 5 years with uncomplicated malaria - female","Children < 5 years with severe malaria - female","Children < 5 years with uncomplicated malaria receiving ACT- male","Individuals >= 5 years with uncomplicated malaria - male","Individuals >= 5 years with uncomplicated malaria receiving ACT- male","Children < 5 years with severe malaria - male","Children < 5 years with uncomplicated malaria - male","Individuals >= 5 years with uncomplicated malaria receiving ACT- female","Children < 5 years with uncomplicated malaria receiving ACT- female","Malaria Death (IDSR)","Malaria  (IDSR)","Severe Malaria (IDSR)","Malaria(Pregnant women)(IDSR)","Malaria(Pregnant women) Death","Severe Malaria Death (IDSR)","Severe malaria","Malaria cases referred for further treatment","Clinical Malaria given ACT","Malaria cases referred for adverse drug reaction","Fever tested by Microscopy (for malaria parasites)","Confirmed uncomplicated malaria given other antimalaria","Confirmed uncomplicated malaria","Clinical Malaria","Pregnant women who received malaria IPT2","Pregnant women who received malaria IPT1","ADRs following antimalarials","Antimalaria Authentication Service","Malaria RDT tested positive","Malaria confirmed pregnant women","Pregnant women with clinical malaria","Malaria microscopy tested positive","Confirmed uncomplicated malaria given ACT")

data_fac <- subset(data_fac , )



malaCompl <- compleMonth(subset(data_fac , indicator_name %in% malaria))

#load('NigeriaWD_indicmap.Rdata')

data_fac_mal <- subset(data_fac , indicator_name %in% malaria)
rm(data_fac)
indics_mal <- subset(data_elements_x_orgunit , data_element %in% malaria)
rm(data_elements_x_orgunit)
save.image('NigeriaWD_malaria.Rdata')

#load('NigeriaWD_malaria.Rdata')
library(plyr)
should_report <- ddply(indics_mal , .(Level2ID , data_element) , nrow)
should_report <- subset(should_report , !is.na(Level2ID))
colnames(should_report) <- c("Level2ID","data_element","should_report")

reporting <- ddply(data_fac_mal , .(Level2ID , indicator_name , period) , 
                   function(data) length(unique(data$orgUnit)),
                   .progress = 'text')
colnames(reporting) <- c("Level2ID","indicator_name","period","Reporting")


coverage <- merge(should_report , reporting ,
                  by.x = c("Level2ID","data_element") ,
                  by.y = c("Level2ID","indicator_name") , 
                  all.x = TRUE , all.y = TRUE)

coverage$cov <- coverage$Reporting / coverage$should_report
coverage <- subset(coverage , !is.na(should_report))
state_dico <- subset(OrgUnitsHierarchy_unique , select = c(Level2 , Level2ID))
state_dico <- unique(state_dico)
coverage <- merge(coverage , state_dico , by = 'Level2ID' , all.x = TRUE , all.y = FALSE)

save.image('toplots.Rdata')

coverage_plot <- subset(coverage , cov <= 1 & should_report > 50)
ord_state <- subset(coverage_plot , select = c(Level2 , should_report))
ord_state <- unique(ord_state)
ord_state <- as.character(ord_state$Level2[order(ord_state$should_report)])

coverage_plot$Level2 <- factor(coverage_plot$Level2 ,  
                               levels = ord_state)

library(ggplot2)
ggplot(data = coverage_plot  , aes(x = period , y =cov) ) + 
  geom_rect(data = coverage_plot ,aes(fill = as.numeric(should_report)),
            xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,
            alpha = 0.3) +
  geom_line(aes(col = data_element)) +
  facet_wrap(~Level2) + theme_bw() + 
  scale_fill_continuous(low="white", high="black" , limits=c(0,5000) ,
                        guide = guide_legend(title = "Number of facilities in State"))
