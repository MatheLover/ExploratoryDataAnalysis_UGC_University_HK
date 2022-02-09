# Question 5 : Is the number of students who discontinue their undergraduate studies increasing?
# Read in data set
withdraw <- read_excel("Students Who Discontinued Their Studies (excluding RPg) (hc).xls", sheet="Customised Data Retrieval")
summary(withdraw)

# Focus on ug
withdraw_ug <- filter(withdraw, `Level of study` == 'Undergraduate')

# Rename column name
names(withdraw_ug)[5] <- 'withdraw_no'

# Simplify University Name
withdraw_ug$Institution[withdraw_ug$Institution == 'City University of Hong Kong'] = 'CityU'
withdraw_ug$Institution[withdraw_ug$Institution == 'Hong Kong Baptist University'] = 'BU'
withdraw_ug$Institution[withdraw_ug$Institution == 'Lingnan University'] = 'LU'
withdraw_ug$Institution[withdraw_ug$Institution == 'The Chinese University of Hong Kong'] = 'CUHK'
withdraw_ug$Institution[withdraw_ug$Institution == 'The Education University of Hong Kong'] = 'EduHK'
withdraw_ug$Institution[withdraw_ug$Institution == 'The Hong Kong Polytechnic University'] = 'PolyU'
withdraw_ug$Institution[withdraw_ug$Institution == 'The Hong Kong University of Science and Technology'] = 'HKUST'
withdraw_ug$Institution[withdraw_ug$Institution == 'The University of Hong Kong'] = 'HKU'

# Aggregate sum of withdraw no. for each school within each year
withdraw_ug$withdraw_no = as.numeric(withdraw_ug$withdraw_no )
agg_sum_with <- aggregate(withdraw_ug$`withdraw_no`, by=list(withdraw_ug$Institution, withdraw_ug$`Academic Year`), FUN=sum, na.rm=TRUE)

# Plot bar chart for the number of withdrawing students at each school in each year
ggplot(data=agg_sum_with, mapping=aes(x=Group.2,y=x)) +
  geom_bar(stat="identity") +
  xlab("Academic Year") + ylab("Number of Undergraduate Students") +
  ggtitle("Number of Undergraduate Students Who Discontinue Their Studies from 2009-2021")
  
ggplot(data=agg_sum_with, mapping=aes(x=Group.1,y=x)) +
  geom_bar(stat="identity")+
  xlab("Institution") + ylab("Number of Undergraduate Students") +
  ggtitle("Number of Undergraduate Students Who Discontinue Their Studies by School from 2009-2021")
ggplot(data=agg_sum_with) +
  geom_line(mapping=aes(x=Group.2,y=x, group=Group.1, color=Group.1))+
  xlab("Institution") + ylab("Number of Undergraduate Students") +
  ggtitle("Number of Undergraduate Students Who Discontinue Their Studies by School from 2009-2021")

# Focus on observations from 2017/2018
agg_sum_with_17 <- filter(agg_sum_with, agg_sum_with$Group.2 == '2017/18' | agg_sum_with$Group.2 == '2018/19' | agg_sum_with$Group.2 == '2019/20' | agg_sum_with$Group.2 == '2020/21')
ggplot(data=agg_sum_with_17, mapping=aes(x=Group.2,y=x)) +
  geom_bar(stat="identity")+
  xlab("Academic Year") + ylab("Number of Undergraduate Students") +
  ggtitle("Number of Undergraduate Students Who Discontinue Their Studies from 2017-2021")
ggplot(data=agg_sum_with_17, mapping=aes(x=Group.1,y=x)) +
  geom_bar(stat="identity")+
  xlab("Institution") + ylab("Number of Undergraduate Students") +
  ggtitle("Number of Undergraduate Students Who Discontinue Their Studies by School from 2017-2021")
ggplot(data=agg_sum_with_17) +
  geom_line(mapping=aes(x=Group.2,y=x, group=Group.1, color=Group.1))+
  xlab("Academic Year") + ylab("Number of Undergraduate Students") +
  ggtitle("Number of Undergraduate Students Who Discontinue Their Studies by School from 2017-2021")
