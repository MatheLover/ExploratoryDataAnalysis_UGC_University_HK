## Question 3 -- Where do most first year undergraduates intakes come from? 
# Read in Data set
first_year_stud_q3 <- read_excel("Admission Qualification of First-year Ug Intakes by Age (hc).xls", sheet="Customised Data Retrieval" )

# Rename column name
names(first_year_stud_q3)[7] <- '1st_year_intake_no'
names(first_year_stud_q3)[1] <- 'Academic_year'
names(first_year_stud_q3)[2] <- 'Academic_struc'
names(first_year_stud_q3)[5] <- 'Origin'

# Select relevant variables
first_year_stud_q3 <- select(first_year_stud_q3, Academic_year, Origin, `1st_year_intake_no`)

# Replace @ value in student_no column with 0
first_year_stud_q3$`1st_year_intake_no`[first_year_stud_q3$`1st_year_intake_no` == '@'] <- 0 
summary(first_year_stud_q3)

# Aggregate sum of student numbers from each region
first_year_stud_q3$`1st_year_intake_no` <- as.numeric(first_year_stud_q3$`1st_year_intake_no`)
agg_sum <- aggregate(first_year_stud_q3$`1st_year_intake_no`, by=list(first_year_stud_q3$Academic_year, first_year_stud_q3$Origin), FUN=sum, na.rm=TRUE)

# Total number of student numbers from each region throughout 10 years
agg_sum_total <- aggregate(agg_sum$x, by=list(agg_sum$Group.2), FUN=sum, na.rm=TRUE)

# Filter out countries with 0 student enrolled in UGC schools
agg_sum_most <- filter(agg_sum_total, agg_sum_total$x != 0)

# Ascending order
agg_sum_most<- agg_sum_most[order(agg_sum_most$x),]

# Select countries with over 200 students
agg_sum_over_200 <- filter(agg_sum_most,agg_sum_most$x >= 200)

# Plot bar chart
ggplot(data=agg_sum_over_200, aes(x=Group.1, y=x, fill=Group.1)) +
  geom_bar(stat = "identity", position = "dodge")
