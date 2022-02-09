# Author -- RUI JIANG 
# Project -- Exploratory Data Analysis on UGC-funded Schools in Hong Kong

# Get working directory
getwd()

# Prepare the package 
library(tidyverse)
library(readxl)

## Question 1 -- Which UGC-funded institution has the largest number of undergraduate students throughout 10 years ? 
# Read Student Enrollment by Institution Data set 
stud_enrol <- read_excel("Student Enrolment by Institution (hc).xls", sheet="Customised Data Retrieval")
summary(stud_enrol)

# Filter out students other than undergraduates
ug_stud <- filter(stud_enrol,`Level of study`== 'Undergraduate')
summary(ug_stud)
head(ug_stud)

# Rename column name
names(ug_stud)[7] <- 'Stud_no'
names(ug_stud)[1] <- 'Academic_year'
summary(ug_stud)

# Replace @ value in student_no column with 0
ug_stud$Stud_no[ug_stud$Stud_no == '@'] <- 0 

# Select Academic Year, Institution, and student_no only
ug_stud_copy <- ug_stud
ug_stud_copy <- select(ug_stud_copy, Academic_year, Institution, Stud_no)


# Aggregate Sum of student counts for each school at different year
# Check Stud_no type
typeof(ug_stud_copy$Stud_no)

# Change it into numeric
ug_stud_copy$Stud_no <- as.numeric(ug_stud_copy$Stud_no)

# Aggregate Sum
ug_stud_plot <- ug_stud_copy %>%
  group_by(Academic_year, `Institution`) %>%
  summarise(total = sum(Stud_no, na.rm = TRUE))
# Alternatively, use agg_sum
# agg_sum <- aggregate(ug_stud_copy$Stud_no, by=list(ug_stud_copy$Academic_year, ug_stud_copy$Institution), FUN=sum, na.rm=TRUE)
# head(agg_sum)

# Plot line chart to show the trend of student numbers at different schools
ggplot(data=ug_stud_plot) +
  geom_line(mapping=aes(x=Academic_year,y=total, group=Institution, color=Institution))+
  xlab("Academic Year") + ylab("Total Number of Undergraduate Students") +
  ggtitle("Total Number of Undergraduate Students for UGC-Funded University from 2009-2021")

## Question 2 -- What is the age distribution of first-year local JUPAS undergraduate student populations in each school?
# JUPAS
# Read in First-year Student Intakes (hc) data set
first_year_stud <- read_excel("Admission Qualification of First-year Ug Intakes by Age (hc).xls", sheet="Customised Data Retrieval" )
summary(first_year_stud)

# Rename column name
names(first_year_stud)[7] <- '1st_year_intake_no'
names(first_year_stud)[1] <- 'Academic_year'
names(first_year_stud)[2] <- 'Academic_struc'
names(first_year_stud)[5] <- 'Origin'

# Replace @ value in student_no column with 0
first_year_stud$`1st_year_intake_no`[first_year_stud$`1st_year_intake_no` == '@'] <- 0 
summary(first_year_stud)

# Filter out students other than locals as well as JUPAS
first_yr_jupas <- filter(first_year_stud, `Main Admission Qualification` == 'JUPAS' & Origin == 'Local student')

# 1. Holistic plotting -- understanding each age group (Below 17, 17-20, 21-24, 25-28, 29-)
first_yr_jupas_holistic <- first_yr_jupas
# Transform 'Below 17' and 'Above 30' into numerically characters
first_yr_jupas_holistic$Age[first_yr_jupas_holistic$Age == 'Below 17'] = '16'
first_yr_jupas_holistic$Age[first_yr_jupas_holistic$Age == 'Above 30'] = '31'

#Convert character values into numeric
as.numeric(first_yr_jupas_holistic$Age)

# Transform age into different age groups
for(i in 1:nrow(first_yr_jupas_holistic)) {
  if(first_yr_jupas_holistic[i,6] == 16){
    first_yr_jupas_holistic[i,6] = 'Below 17'
  }
  else if(first_yr_jupas_holistic[i,6] >= 17 & first_yr_jupas_holistic[i,6] <= 20 ){
    first_yr_jupas_holistic[i,6] = '17-20'
  }
  else if(first_yr_jupas_holistic[i,6] >= 21 & first_yr_jupas_holistic[i,6] <= 24 ){
    first_yr_jupas_holistic[i,6] = '21-24'
  }
  else if(first_yr_jupas_holistic[i,6] >= 25 & first_yr_jupas_holistic[i,6] <= 28 ){
    first_yr_jupas_holistic[i,6] = '25-28'
  }
  
  else if(first_yr_jupas_holistic[i,6] >= 29){
    first_yr_jupas_holistic[i,6] = '29 or Above'
  }
} 

# Aggregate sum of students for each age group within each academic year
first_yr_jupas_holistic$`1st_year_intake_no` <- as.numeric(first_yr_jupas_holistic$`1st_year_intake_no`)
first_yr_jupas_holistic <- first_yr_jupas_holistic %>%
  group_by(Academic_year, Age) %>%
  summarise(total = sum(`1st_year_intake_no`, na.rm = TRUE))

# Plot bar chart 
ggplot(data=first_yr_jupas_holistic, aes(x=Academic_year, y=total, fill=Age)) + 
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Academic Year") + ylab("Total Number of Undergraduate Students") +
  ggtitle("Total Number of Undergraduate Students from 2009-2021 by Ages")
  

# 2. Partial plotting -- understanding each age in the age group -- 17, 18, 19, 20 --
first_yr_jupas_partial <- first_yr_jupas
# Filter out observations with ages other than 17-20
first_yr_jupas_partial$Age <- as.numeric(first_yr_jupas_partial$Age)
first_yr_jupas_partial <- filter(first_yr_jupas_partial, Age >= 17 & Age <= 20)

# Aggregate sum
first_yr_jupas_partial$`1st_year_intake_no` <- as.numeric(first_yr_jupas_partial$`1st_year_intake_no`)
first_yr_jupas_partial <- first_yr_jupas_partial %>%
  group_by(Academic_year, Age) %>%
  summarise(total = sum(`1st_year_intake_no`, na.rm = TRUE))

# Plot partial bar chart
ggplot(data=first_yr_jupas_partial, aes(x=Academic_year, y=total, fill=as.factor(Age))) + 
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Academic Year") + ylab("Total Number of Undergraduate Students") +
  ggtitle("Total Number of Undergraduate Students from 2009-2021 in the Age Group (17-20)")

