setwd('J://Project/phc/nga/dhis')


qplot(data = ReportsComplete[ReportsComplete$unitName == 'ab Agu Foundation Hosptial' &
                               ReportsComplete$DEName =='Facility Attendance', ] , 
      x = period , y = as.numeric(value) )

qplot(data = ReportsComplete[ReportsComplete$DEName =='Facility Attendance', ] , 
      x = period , y = as.numeric(value) ) +
  facet_wrap(~unitName)



qplot(data = ReportsComplete[ReportsComplete$unitName == 'ab Agu Foundation Hosptial', ] , 
      x = period , y = as.numeric(value) ) +
  facet_wrap(~DEName)



#GetReport(DfDataSets$DataSetsid[1] , DfOrgUnit$unitid[13] , 
#          '2008-01-01' , '2014-08-01')