# Question 4 -- Which industry do most graduates work in? Among different schools, which school contributes the most to each sector?(For the last 5 years)
# Read in data set
employment <- read_excel("Employment Situation of Full-time Graduates.xls", sheet="Customised Data Retrieval")
summary(employment)
# Undergraduate Graduate Employment
ug_employment <- filter(employment, `Level of study` == 'Undergraduate')

#Check no. of graduates and convert it into numeric
typeof(ug_employment$`No. of graduates`)
ug_employment$`No. of graduates` = as.numeric(ug_employment$`No. of graduates`)

# Find unique occupations 
occup_list <- unique(ug_employment$Occupation)
occup_list

# Simplify each university name
ug_employment$Institution[ug_employment$Institution == 'City University of Hong Kong'] = 'CityU'
ug_employment$Institution[ug_employment$Institution == 'Hong Kong Baptist University'] = 'BU'
ug_employment$Institution[ug_employment$Institution == 'Lingnan University'] = 'LU'
ug_employment$Institution[ug_employment$Institution == 'The Chinese University of Hong Kong'] = 'CUHK'
ug_employment$Institution[ug_employment$Institution == 'The Education University of Hong Kong'] = 'EduHK'
ug_employment$Institution[ug_employment$Institution == 'The Hong Kong Polytechnic University'] = 'PolyU'
ug_employment$Institution[ug_employment$Institution == 'The Hong Kong University of Science and Technology'] = 'HKUST'
ug_employment$Institution[ug_employment$Institution == 'The University of Hong Kong'] = 'HKU'

# Categorize occupation into different industries
for(i in 1:nrow(ug_employment)) {
  if(ug_employment[i,5] == "Accountants and Auditors" ){
    ug_employment[i,5] = 'Business'
  }
  else if(ug_employment[i,5] == "Agriculture and Fishery Workers" ){
    ug_employment[i,5] = 'Agriculture'
  }
  else if(ug_employment[i,5] == "Aircraft and Ship Officers" ){
    ug_employment[i,5] = 'Transport'
  }
  else if(ug_employment[i,5] == "Architects and Surveyors" ){
    ug_employment[i,5] = 'Construction'
  }
  
  else if(ug_employment[i,5] == "Artists" ){
    ug_employment[i,5] = 'Art'
  }
  
  else if(ug_employment[i,5] == "Authors, Journalists and Related Writers" ){
    ug_employment[i,5] = 'Media'
  }
  
  else if(ug_employment[i,5] == "Business Professionals" ){
    ug_employment[i,5] = 'Business'
  }
  
  else if(ug_employment[i,5] == "Clerical and Related Workers" ){
    ug_employment[i,5] = 'Clerical and Administrative'
  }
  
  else if(ug_employment[i,5] == "Economists, Statisticians and Mathematicians" ){
    ug_employment[i,5] = 'Statistics and Mathematics'
  }
  
  else if(ug_employment[i,5] == "Engineering Technicians" ){
    ug_employment[i,5] = 'Engineering'
  }
  
  else if(ug_employment[i,5] == "Engineers" ){
    ug_employment[i,5] = 'Engineering'
  }
  
  else if(ug_employment[i,5] == "Government Officials (General Grades)" ){
    ug_employment[i,5] = 'Government'
  }
  
  else if(ug_employment[i,5] == "Insurance, Real Estate, and Business Services" ){
    ug_employment[i,5] = 'Business'
  }
  
  else if(ug_employment[i,5] == "Legal Workers" ){
    ug_employment[i,5] = 'Law'
  }
  
  else if(ug_employment[i,5] == "Managers" ){
    ug_employment[i,5] = 'Business'
  }
  
  else if(ug_employment[i,5] == "Medical and Health Workers" ){
    ug_employment[i,5] = 'Medical and Health'
  }
  
  else if(ug_employment[i,5] == "Other Professionals and Technical Workers" ){
    ug_employment[i,5] = 'Engineering'
  }
  
  else if(ug_employment[i,5] == "Physical and Life Science Technicians" ){
    ug_employment[i,5] = 'Physics and Biology'
  }
  
  else if(ug_employment[i,5] == "Physical and Life Scientists" ){
    ug_employment[i,5] = 'Physics and Biology'
  }
  
  else if(ug_employment[i,5] == "Production and Related Workers" ){
    ug_employment[i,5] = 'Manufacturing'
  }
  
  else if(ug_employment[i,5] == "Protective Service Workers" ){
    ug_employment[i,5] = 'Service'
  }
  
  else if(ug_employment[i,5] == "Sales/Service Workers" ){
    ug_employment[i,5] = 'Service'
  }
  
  else if(ug_employment[i,5] == "System Analysts and Computer Programmers" ){
    ug_employment[i,5] = 'IT'
  }
  
  else if(ug_employment[i,5] == "Teaching Profession" ){
    ug_employment[i,5] = 'Education'
  }
  
  else if(ug_employment[i,5] == "Workers not reporting any occupation" ){
    ug_employment[i,5] = 'Missing Information/ Uncertain'
  }
  
  else if(ug_employment[i,5] == "Government Administrators and Diplomats" ){
    ug_employment[i,5] = 'Government'
  }
  
  else if(ug_employment[i,5] == "Occupations not elsewhere classified" ){
    ug_employment[i,5] = 'Missing Information/ Uncertain'
  }
  
  else if(ug_employment[i,5] == "Religious Workers" ){
    ug_employment[i,5] = 'Religion'
  }
  
  else if(ug_employment[i,5] == "Workers reporting occupations unidentifiable or inadequately described" ){
    ug_employment[i,5] = 'Missing Information/ Uncertain'
  
  }
  
  else if(ug_employment[i,5] == "Not in Full-time Employment" ){
    ug_employment[i,5] = 'Unemployed'
  }
  
  else if(ug_employment[i,5] == "Other Service Workers" ){
    ug_employment[i,5] = 'Service'
  }
  
} 

# Find updated unique occupations
occup_updated_list <- unique(ug_employment$Occupation)
occup_updated_list

## Visualize the trend for 10 years the Aggregate sum of each occupation for each school 
# Aggregate sum of each occupation throughout 10 years
agg_sum_occup <- aggregate(ug_employment$`No. of graduates`, by=list(ug_employment$Institution, ug_employment$Occupation), FUN=sum, na.rm=TRUE)

# Filter out missing or unemployed occupations
agg_sum_occup <- filter(agg_sum_occup, agg_sum_occup$Group.2 != 'Missing Information/ Uncertain' & agg_sum_occup$Group.2 != 'Unemployed')

# Plot bar chart for each profession at each school
ggplot(data=agg_sum_occup, aes(x=Group.2, y=x, fill=Group.1)) +
  coord_flip()+
  geom_bar(stat = "identity")
