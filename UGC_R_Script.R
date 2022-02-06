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
  geom_line(mapping=aes(x=Academic_year,y=total, group=Institution, color=Institution))

