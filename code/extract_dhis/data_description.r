setwd('C://users/grlurton/Desktop/dhis')

library(plyr)
library(ggplot2)
library(zoo)
library(scales)
## Load and format data

data_fac <- read.csv('data.csv')

## Function to take out elements that are duplicated (for security, no reason that this
## should happen)

get_unique <- function(data , identifier){
  data_tab <- ddply(data , as.quoted(identifier) , nrow)
  data_tab <- subset(data_tab , V1 < 2)
  print(nrow(data_tab))
  unique_data <- data[data[ , identifier] %in% data_tab[ ,identifier ],]
  print(nrow(unique_data))
  unique_data
}

indics <- read.csv('table_indicators.csv')
indics <- get_unique(indics , 'indicator_ID')

OrgUnitsHierarchy <- read.csv('HierarchyData.csv')
OrgUnitsHierarchy <- get_unique(OrgUnitsHierarchy , 'Level5ID')

data_fac <- subset(data_fac , orgUnit %in% OrgUnitsHierarchy$Level5ID)

data_fac$dataElement <- as.character(data_fac$dataElement)
data_fac$period <- as.Date(as.yearmon(data_fac$period, "%Y%m"))

data_fac <- merge(data_fac , indics , by.x = 'dataElement' , by.y = 'indicator_ID' ,
                      all.y = FALSE , all.x = FALSE)
data_fac <- merge(data_fac , OrgUnitsHierarchy , by.x = 'orgUnit' , by.y = 'Level5ID'  ,
                      all.x = FALSE , all.y = FALSE)

qplot(data = data_fac , x = period , binwidth = 30) +
  theme_bw()


#### START HERE###

org_x_datasets <- read.csv('OrgUnitxDataSets.csv')
report_x_datasets <- read.csv('RepxDE.csv')


data_elements_x_orgunit <- merge(org_x_datasets , report_x_datasets ,
                                 by.x = 'DataSetsName' , by.y = 'DataSetsName')
colnames(data_elements_x_orgunit) <- c('data_sets' , 'x' , 'org_unit_ID' ,
                                       'y' , 'data_element')
data_elements_x_orgunit <- subset(data_elements_x_orgunit ,select = -c(x,y))


data_elements_x_orgunit <- merge(data_elements_x_orgunit , 
                                 OrgUnitsHierarchy , 
                                 by.x = "org_unit_ID" ,
                                 by.y = "Level5ID" ,
                                 all.x = TRUE , all.y = FALSE)

data_elements_x_orgunit <- subset(data_elements_x_orgunit , 
                                  select = -c(data_sets , X , Level4 , 
                                              Level3 , Level1 , Level5 ,
                                              Level3ID , Level4ID , toMatch ,
                                              Level6ID))

#load('NigeriaWD_indicmap.Rdata')

data_class <- read.csv('table_indicators.csv') 

state_dico <- subset(OrgUnitsHierarchy , select = c(Level2 , Level2ID))
state_dico <- unique(state_dico)


make_coverage <- function(data, data_class, data_x_org, state_dico, theme,  
                              categories,OrgUnitsHierarchy){
  print('Getting Relevant data')
  theme_data <- data_class[data_class[theme] == 1 & 
                                  !is.na(data_class[theme]),]
  print(nrow(theme_data))
  theme_indicators <- theme_data$indicator_name
  data_fac <- subset(data , indicator_name %in% theme_indicators)
  print(nrow(data_fac))
  indicators <- subset(data_x_org , data_element %in% theme_indicators)  
  
  print('Computing availability parameters')
  should_report <- ddply(indicators , .(Level2ID , data_element) , nrow ,.progress = 'text')
  should_report <- subset(should_report , !is.na(Level2ID))
  colnames(should_report) <- c("Level2ID","data_element","should_report")
  
  reporting <- ddply(data_fac , .(Level2ID , indicator_name , period) , 
                     function(data) length(unique(data$orgUnit)),
                     .progress = 'text')
  colnames(reporting) <- c("Level2ID","indicator_name","period","Reporting")
  
  coverage <- merge(should_report , reporting ,
                    by.x = c("Level2ID","data_element") ,
                    by.y = c("Level2ID","indicator_name") , 
                    all.x = TRUE , all.y = TRUE)
  coverage$cov <- coverage$Reporting / coverage$should_report
  coverage <- subset(coverage , !is.na(should_report))
  
  #Coverage by state
  print('Merging availability with states names')
  coverage <- merge(coverage , state_dico , by = 'Level2ID' , all.x = TRUE , all.y = FALSE)
  coverage <- subset(coverage , cov <= 1 & should_report > 50)
  print('Ordering States')
  ord_state <- subset(coverage , select = c(Level2 , should_report))
  ord_state <- unique(ord_state)
  ord_state <- as.character(ord_state$Level2[order(ord_state$should_report)])
  coverage$Level2 <- factor(coverage$Level2 , levels = ord_state)
  coverage <- merge(coverage, data_class , by.x = "data_element" , by.y ="indicator_name" ,
                    all.x = TRUE , all.y=FALSE)
  coverage
}

coverage <- make_coverage(data = data_fac , data_class = data_class , 
                          data_x_org = data_elements_x_orgunit , 
                          state_dico = state_dico , 
                          theme = 'indicator_thematic_malaria' , 
                          categories = 'malaria_sub' ,
                          OrgUnitsHierarchy = OrgUnitsHierarchy)


#plotting availability

plot_availability <- function(coverage , categories){
  categories_list <- unique(coverage[ , categories])
  for (i in 1:length(categories_list)){
    cat <- as.character(categories_list[i])
    coverage_plot <- coverage[as.character(coverage[,categories]) == cat , ]
    print(cat)
    print(nrow(coverage_plot))
    a <- ggplot(data = coverage_plot  , aes(x = period , y =cov) ) + 
      geom_rect(data = coverage_plot ,aes(fill = as.numeric(should_report)),
                xmin = -Inf,xmax = Inf,
                ymin = -Inf,ymax = Inf,
                alpha = 0.3) +
      geom_line(aes(col = data_element)) + 
      scale_x_date(breaks = date_breaks("1 years"),
                   minor_breaks = "3 months" ,
                   labels = date_format("%y"))+
      facet_wrap(~Level2) + theme_bw() +
      scale_fill_continuous(low="white", high="black" , 
                            guide = guide_legend(title = 
                                                   "Number of facilities in State")) +
      ggtitle(cat)
    print(a)
  }
}

pdf('malaria_plots.pdf' , width = 14)
plot_availability(coverage = coverage , 
                  categories = 'malaria_sub')
dev.off()


