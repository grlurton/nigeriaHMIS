library(ggplot2)
library(zoo)

Data <- read.csv('Data.csv')

#Data$lastupdated <- as.Date(Data$lastupdated)
Data$dataElement <- as.character(Data$dataElement)

Data$period <- as.Date(as.yearmon(Data$period, "%Y%m"))

qplot(data = Data , x = period , binwidth = 30) +
  theme_bw()

sort(table(Data$DEName))


write.csv(table(Data$DEName) , 'indicators.csv')

attendance <- c("Facility Attendance","Antenatal total attendance","Inpatient Admissions","Antenatal 1st (booking) visit 20 weeks or later","Attendance 15 years and older - female","Antenatal 1st (booking) visit before 20 weeks","Attendance 15 years and older - male","Attendance 12 months - 59 months - female","Attendance 12 months - 59 months - male","Attendance 5 - 14years - female","Attendance 5 - 14 years - male","Attendance 29 days - 11 months - female","Attendance 29 days - 11 months - male","Attendance 0 - 28 days - male","Attendance 0 - 28 days - female","Outpatient Attendance","Inpatient days - Total","Inpatient discharges - total","Referral out")
supplies <- c("Stock out Essential Drugs for 7 consecutive days","Stock out of vaccine supplies for 7 consecutive days","Stock out of ACTs for 7 consecutive days in the last 1 month","Stock out of family planning commodities for 7 consecutive days","Stock out of HIV test kits for 7 consecutive days in the last 1 month","Stock out of anti-TB drugs for 7 consecutive days in the last 1 month","Functional beds","Stock out of ORS/Zinc","Stock out of Rapid Diagnostic Tests (RDTs) for 7 days consecutively","Stock out of SPs for 7 consecutive days","Stock out of LLINs for 7 consecutive days","Stock out of IFAs","Stock out of Female Condoms","Stock out of Injectable antibiotics","Stock out of amoxicillin DT","Stock out of Oxytocin","Stock out of Magnesium sulfate","Stock out of Resuscitation Equipment","Stock out of Emergency Contraception","Stock out of Misoprostol","Stock out of Antenatal Corticosteroid (ANCS)","Stock out of Chlorhexidine","Stock out of Antiretroviral Drugs for 7 consecutive days","Stock out of Implants","Oral pill cycle (packets)")
malaria <- c("Fever cases","Clinical Malaria","Confirmed uncomplicated malaria given ACT","Confirmed uncomplicated malaria","Individuals >= 5 years with uncomplicated malaria - female","Children < 5 years with uncomplicated malaria - female","Children < 5 years with uncomplicated malaria - male","Individuals >= 5 years with uncomplicated malaria - male","Fever tested by RDT","Malaria RDT tested positive","Clinical Malaria given ACT","Children < 5 years with uncomplicated malaria receiving ACT- male","Individuals >= 5 years with uncomplicated malaria receiving ACT- female","Children < 5 years with uncomplicated malaria receiving ACT- female","Individuals >= 5 years with uncomplicated malaria receiving ACT- male")
vaccine <- c("Oral Polio Vaccine 1 given","Oral Polio Vaccine 2 given","Oral Polio Vaccine 3 given","Oral Polio Vaccine 2 given","Oral Polio Vaccine 3 given","Measles Vaccine 1 given","Pentavalent Vaccine 1 given","Tetanus Toxoid Dose  1","Pentavalent Vaccine 3 given","Pentavalent Vaccine 2 given","Yellow Fever Vaccine given","OPV 1st dose under 1 year - female","DPT 1st dose under 1 year - female (2010)","DPT 1st dose under 1 year - male","Measles 1st dose under 1 year - female","Measles 1st dose under 1 year - male","DPT 3rd dose under 1 year - female","Oral Polio Vaccine 0 given","DPT 3rd dose under 1 year - male","Tetanus Toxoid Dose  2","HepB 1st dose under 1 year - female","BCG ","HepB 1st dose under 1 year - male","BCG 1st dose under 1 year - female (2010)","BCG 1st dose under 1 year - male (2010)","Hepatitis B Vaccine 0 birth","Immunised fully under 1 year - female","Immunised fully under 1 year - male","Fully Immunised Children under 1 year","HepB 3rd dose under 1 year - female","HepB 3rd dose under 1 year - male")
motherChild <- c("Deliveries - Normal","Children 0-59 months weighed","Live Birth","Diarrhoea  < 5 years - new case\t","Pregnant women who received malaria IPT2","Female 15-49 years using modern contraceptives","Children 0-6 months exclusivelly breastfed","Diarrhoea  < 5 years - new case given Oral Rehydration preparaions")
Divers <- c("Condoms distributed (sachet) - male","Individuals HIV counseled, tested and received results  ","Family Planning Injections","Prescriptions issued","Items dispensed","Family planning clients counselled")

compleMonth <- function(data){
  ddply(data , .(period , DEName) , function(x) {length(unique(as.character(x$unitName))) / 401} ,
        .progress = "text")
}

library(plyr)



attCompl <- compleMonth(subset(Data , DEName %in% attendance))
attCompl <- merge(attCompl , dsFull , by.x = 'DEName' , by.y = 'dataElements' , all.y = FALSE)

qplot(data = attCompl , x = period , y = V1 , geom = 'line' , col = DataSetsName) +
  theme_bw() + facet_wrap(~DEName)

supplCompl <- compleMonth(subset(Data , DEName %in% supplies))
qplot(data = supplCompl , x = period , y = V1 , geom = 'line') +
  theme_bw() + facet_wrap(~DEName)

malaCompl <- compleMonth(subset(Data , DEName %in% malaria))
qplot(data = malaCompl , x = period , y = V1 , geom = 'line') +
  theme_bw() + facet_wrap(~DEName)
  
vaccCompl <- compleMonth(subset(Data , DEName %in% vaccine))
qplot(data = vaccCompl , x = period , y = V1 , geom = 'line') +
  theme_bw() + facet_wrap(~DEName)

mothChCompl <- compleMonth(subset(Data , DEName %in% motherChild))
qplot(data = mothChCompl , x = period , y = V1 , geom = 'line') +
  theme_bw() + facet_wrap(~DEName)

DivCompl <- compleMonth(subset(Data , DEName %in% Divers))
qplot(data = DivCompl , x = period , y = V1 , geom = 'line') +
  theme_bw() + facet_wrap(~DEName)
