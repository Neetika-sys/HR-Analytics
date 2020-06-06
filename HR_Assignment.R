#---------------------------------------------------LOADING DATSETS ------------------------------------------------------#

require(dplyr)
require(ggplot2)
employee_survey_data <- read.csv("employee_survey_data.csv" , stringsAsFactors = F)
general_data <- read.csv("general_data.csv" , stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv" , stringsAsFactors = F)
in_time <- read.csv("in_time.csv",stringsAsFactors = F)
out_time <- read.csv("out_time.csv",stringsAsFactors = F)

#---------------------------------------------------MERGING DATASETS-------------------------------------------------------#



# Checking if Rows are duplicated
length(unique(employee_survey_data$EmployeeID)) # 4410
length(unique(general_data$EmployeeID)) # 4410
length(unique(manager_survey_data$EmployeeID)) # 4410
length(unique(in_time$X)) # 4410
length(unique(out_time$X)) # 4410


# Checking if employee ID order is correct in all data frames so that they can be merged/cbind  
setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID)
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID)
setdiff(general_data$EmployeeID,in_time$X)
setdiff(general_data$EmployeeID,out_time$X)
# All have a diff of 0 which indicate them having same IDs



##In_time and Out_time##


# Coverting time to POSIXlt data for easy calculation of times  
in_time_1 <- data.frame(sapply(in_time[,-1], function(x){ as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S")} ))
out_time_1 <- data.frame(sapply(out_time[,-1], function(x){ as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S")} ))


##DERIVE VARIABLES.

# Derive variable : worked_hours
time_1 <- out_time_1 - in_time_1 # Subtracting (OutTime - Intime)
class(time_1$X2015.01.01)

time_df <- cbind(out_time$X,time_1) # Binding Employee ID and derived variable to time_df
colnames(time_df)[1]  <- "EmployeeID"

time_df[,-1] <- sapply(time_df[,-1], unclass)
time_df[,-1] <- sapply(time_df[,-1], as.numeric) # Converting to numeric

time_df$worked_hours_mean <- rowMeans(time_df[,-1],na.rm = T) # Finding mean of worked_hours_mean

time_df <- time_df[,c("EmployeeID","worked_hours_mean")]  

# Derive Variable : out_of_office

# calculating number of days on off/leave/out of office 
time_df$out_of_office <- rowSums(is.na(time_1))

# Derive Variable : overtime_count

# calculating number of days worked overtime
time_df$overtime_count <- rowSums(time_1 > 8,na.rm = T)

# Derive Variable : undertime_count

# calculating number of days worked undertime (less than 6 hours)
time_df$undertime_count <- rowSums(time_1 < 6,na.rm = T)

# Check order of Employee ID before merging  
setdiff(time_df$EmployeeID,general_data$EmployeeID)


combined_employee_details <- merge(employee_survey_data,general_data, by = 'EmployeeID')
combined_employee_details <- merge(combined_employee_details,manager_survey_data, by = 'EmployeeID')
combined_employee_details <- merge(combined_employee_details,time_df, by = 'EmployeeID')


#-------------------------------------------------------CHECKING STRUCTURE-----------------------------------------------------------------#

glimpse(combined_employee_details)


combined_employee_details<- mutate_if(combined_employee_details,is.character, toupper)

combined_employee_details$Attrition <- ifelse(combined_employee_details$Attrition == 'YES',1,0)



#--------------------------------------------- CHECKING MISSING VALUES------------------------------------------------------ #


colSums(is.na(combined_employee_details))

##### We have missing values in EnvironmentSatisfaction(25),JobSatisfaction(20) ,WorkLifeBalance(38),NumCompaniesWorked(19),TotalWorkingYears(9).

colSums(combined_employee_details==" "| combined_employee_details=="", na.rm = T)

##### No Blank values ######

#----------------------------------------------CHECKING AND REMOVING DUPLICATES---------------------------------------------#

combined_employee_details <- combined_employee_details[!duplicated(combined_employee_details),]


#----------------------------------------------UNIVARIATE AND BIVARIATE ANALYSIS--------------------------------------------#


# 1. EMPLOYEE ID

n_distinct(combined_employee_details$EmployeeID)
# we have distinct employee id's equal to the no of rows in the dataset, therefore it can be treated as our priamry key.


# 2. ENVIRONMENT SATISFACTION


table(combined_employee_details$EnvironmentSatisfaction)


ggplot(combined_employee_details, aes(x = EnvironmentSatisfaction, fill = as.factor(Attrition))) +
  geom_bar(color = 'black', position = 'fill') +
  coord_flip() +theme_bw() + labs(title = " Attrition based on EnvironmentSatisfaction")

group_by(combined_employee_details, EnvironmentSatisfaction) %>%
  summarise(prom_perc = sum(Attrition, na.rm = T)/n()*100) %>%
  arrange(prom_perc)

### We can observe that Attrition of employees Increases with the Decrease in Environment Satisfaction level.


# 3. JOB SATISFACTION

combined_employee_details %>%
  ggplot(aes(x = as.factor(JobSatisfaction))) +
  geom_bar(aes(y = (..count..), fill = JobSatisfaction)) +
  geom_text(aes(y = (..count..), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "Job Satisfaction Score", y = "Count", x = "Job Satisfaction Levels")+
  scale_fill_continuous(low = "darkred", high = "darkgreen")
### Though Job Satisfaction seems to be quite good , still there are about 40% employees with 2 or lower Job satisfaction rating


# 4. WORK LIFE BALANCE

ggplot(combined_employee_details,aes(combined_employee_details$MonthlyIncome,combined_employee_details$WorkLifeBalance, color=Attrition))+
  geom_point()

combined_employee_details %>%
  ggplot(aes(x = as.factor(WorkLifeBalance))) +
  geom_bar(aes(y = (..count..), fill = WorkLifeBalance)) +
  geom_text(aes(y = (..count..), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "Work-Life Balance Score", y = "Count", x = "Work-Life Balance Levels")+
  scale_fill_continuous("Work-Life Balance",low = "darkred", high = "darkgreen")
#### Work Life balance seems to be quite average and about 28.5% people are not so happy with the Work-Life balance


# 5. YEARS SINCE LAST PROMOTION

combined_employee_details %>%
  ggplot(aes(x = as.factor(YearsSinceLastPromotion))) +
  geom_bar(aes(y = (..count..), fill = YearsSinceLastPromotion)) +
  geom_text(aes(y = (..count..), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = 0.5, hjust=-0.1 ,angle = 90) +
  labs(title = "Years Since Last Promotion", y = "Count", x = "Years Since Last Promotion")+
  scale_fill_continuous("Years",low = "darkred", high = "darkgreen")

# Though there are very few people don't get promoted for a long time, about 25% employees don't get promotion for more than 3 years 



# 6. MARITAL STATUS - ATTRITION

combined_employee_details %>%
  ggplot(aes(x = MaritalStatus, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = as.factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes(y = (..prop..), label = scales::percent((..prop..)/sum(..prop..))), stat = "count", vjust = -0.5)+
  labs(title = "Marital-Status Vs Attrition", y = "Percentage Attrition", x = "Marital Status")+
  facet_grid(~Attrition)

# Clearly people who are single tend to leave the company more often, whereas married and divorced people tend to stay back longer



# 7.OVERTIME - ATTRITION

combined_employee_details[which(combined_employee_details$overtime_count > 1),] %>%
  ggplot(aes(x = overtime_count, group = Attrition)) + 
  geom_bar(aes(y = ..prop..)) + 
  scale_y_continuous(labels=scales::percent) +
  labs(title = "Overtiming Vs Attrition", y = "Percentage Attrition", x = "Overtime Frequency")+
  facet_grid(~Attrition)

# More people who overtime a lot throughout the year tend to leave the company


# 8. BUSINESS TRAVEL - ATTRITION

combined_employee_details %>%
  ggplot(aes(x = BusinessTravel, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = as.factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes(y = (..prop..), label = scales::percent((..prop..)/sum(..prop..))), stat = "count", vjust = -0.5)+
  labs(title = "Business Travel Vs Attrition", y = "Percentage Attrition", x = "Business Travel")+
  facet_grid(~Attrition)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))

# People travelling more tend to stay back with the company 


# 9. YEARS WITH CURRENT MANAGER - ATTRITION
combined_employee_details %>%
  ggplot(aes(x = YearsWithCurrManager, group = Attrition)) + 
  geom_bar(aes(y = ..prop..)) + 
  scale_y_continuous(labels=scales::percent) +
  labs(title = "Years With Current Manager Vs Attrition", y = "Percentage Attrition", x = "Years With Current Manager")+
  facet_grid(~Attrition)

# People under same manager for a longer time tend to stay back


# 10. ATTRITION 

table(combined_employee_details$Attrition) # 711/ 4410 Employees are Leaving.

combined_employee_details %>%
  ggplot(aes(x = Attrition)) +
  geom_bar(aes(y = (..count..), fill = Attrition)) +
  geom_text(aes(y = (..count..), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "Attrition Frequency", y = "Count", x = "Attrition")

# There is an attrition of 16.1%

# 11. AGE 

ggplot(combined_employee_details , aes( x = Age , fill = as.factor(Attrition))) + geom_bar() + 
  theme_bw() + labs(title = "Age effect on Attrition")

# Employees between 25 to 35 age  showing more attrition of employees


#. 12 DEPARTMENT

ggplot(combined_employee_details, aes(x = Department, fill = as.factor(Attrition))) +
  geom_bar(color = 'black', position = 'fill') +
  coord_flip() +
  ggtitle("Department Wise Attrition") +
  theme(plot.title = element_text(hjust = 0.5))

group_by(combined_employee_details, Department) %>%
  summarise(prom_perc = sum(Attrition, na.rm = T)/n()*100) %>%
  arrange(prom_perc)

## Human Resourse Dept shows more Attrition of Employees.


# 13. DISTANCE FROM HOME


ggplot(combined_employee_details , aes( x = DistanceFromHome , fill = as.factor(Attrition))) + geom_bar() + 
  theme_bw() + labs(title = "DISTANCE FROM HOME on Attrition")

# Employees Staying Nearby shows higher Attrition. 


#14. EDUCATION 

ggplot(general_data , aes( x = Education , y= EducationField, fill = as.factor(Attrition))) + geom_col()+
theme_bw() + labs(title = " education wise Attrition")

group_by(combined_employee_details, Education) %>%
  summarise(prom_perc = sum(Attrition, na.rm = T)/n()*100) %>%
  arrange(prom_perc)

# LifeSciences and Medical Department Employees Are Majority in number.

# 15. JOB LEVEL 

ggplot(combined_employee_details, aes(x = JobLevel, fill = as.factor(Attrition))) +
  geom_bar(color = 'black', position = "fill")

#Employees with job level 4 & 5 show least attrition.


# 16. JOB ROLE 

ggplot(combined_employee_details, aes(x = JobRole, fill = as.factor(Attrition))) +
  geom_bar()

group_by(combined_employee_details, JobRole) %>%
  summarise(prom_perc = sum(Attrition, na.rm = T)/n()*100) %>%
  arrange(prom_perc)

## Employees working as laboratory technician, Research Director, Research Scientist and Sales Executive have higher rate of attrition.



# 17. scatter plot between monthly income, work life balance and attrition

ggplot(combined_employee_details,aes(combined_employee_details$MonthlyIncome,combined_employee_details$WorkLifeBalance, color=as.factor(Attrition)))+geom_point()


# 18. scatter plot between monthly income, JobLevel and attrition

ggplot(combined_employee_details,aes(combined_employee_details$MonthlyIncome,combined_employee_details$JobLevel, color=as.factor(Attrition)))+geom_point()


# 19. boxplot between monthly income and attrition

ggplot(combined_employee_details,aes(Attrition,MonthlyIncome,fill=as.factor(Attrition)))+geom_boxplot()


# 20.boxplot between monthly income and attrition

ggplot(combined_employee_details,aes(Attrition,YearsSinceLastPromotion,fill=as.factor(Attrition)))+geom_violin()



# .21 GENDER

table(combined_employee_details$Gender)
# Lesser no of female employees

ggplot(combined_employee_details, aes(x = Gender, fill = as.factor(Attrition))) +
  geom_bar(color = 'black', position = 'fill') +
  coord_flip() +
  ggtitle("Gender Wise Attrition") +
  theme(plot.title = element_text(hjust = 0.5))

group_by(combined_employee_details, Gender) %>%
  summarise(prom_perc_gen = sum(Attrition, na.rm = T)/n()*100) %>%
  arrange(prom_perc_gen)
# Not much difference in the attrition of males and females.



#. 22 NUMBER COMPANIES WORKED

ggplot(combined_employee_details, aes(x = NumCompaniesWorked , fill = as.factor(Attrition))) + geom_bar()

##It seems that Employees whose XZY is Second Organisation are looking for Switch for more Opportunities.



# 23. PERCENT SALARY HIKE

ggplot(combined_employee_details , aes( x = PercentSalaryHike , fill = as.factor(Attrition))) + geom_bar()+
  theme_bw() + labs(title = "Percent Salary Hike on Attrition")

group_by(combined_employee_details,PercentSalaryHike) %>%
  summarise(prom_perc_gen = sum(Attrition, na.rm = T)/n()*100) %>%
  arrange(prom_perc_gen)

## #The ratio of non-leaving employees to leaving employees is higher in the salary hike range of 11% - 14% , 
##this ratio is almost similar & lower than the previous for the range of 15% - 22%.
##Employees with salary hike of 25 % show least tendency of leaving the company.



# 24. STOCK OPTION LEVEL

table(combined_employee_details$StockOptionLevel)

combined_employee_details %>%
  ggplot(aes(x = StockOptionLevel, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = as.factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes(y = (..prop..), label = scales::percent((..prop..)/sum(..prop..))), stat = "count", vjust = -0.5)+
  labs(title = "StockOptionLevel Vs Attrition", y = "Percentage Attrition", x = "StockOptionLevel")+
  facet_grid(~Attrition)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))

## Same pattern is Observed among Yes or No Attrition of employees With respect to Stock Option Level.



# 25. TOTAL WORKING YEARS

ggplot(combined_employee_details , aes( x = TotalWorkingYears , fill = as.factor(Attrition))) + geom_bar()+
  theme_bw() + labs(title = "Total Working Years on Attrition")

### Employees Working having more than 10 years of experience are least likely to leave the company.



# 26. TRAINING TIMES LAST YEAR

ggplot(combined_employee_details , aes( x = TrainingTimesLastYear , fill = as.factor(Attrition))) + geom_bar()+
  theme_bw() + labs(title = " no of Trainings last year on Attrition")


group_by(combined_employee_details,TrainingTimesLastYear) %>%
  summarise(prom_perc_gen = sum(Attrition, na.rm = T)/n()*100) %>%
  arrange(prom_perc_gen)

#### Employees who didnt get training session at all are more likely to leave the company.



# 27. YEARS AT COMPANY 


combined_employee_details %>%
  ggplot(aes(x = as.factor(YearsAtCompany))) +
  geom_bar(aes(y = (..count..), fill = YearsAtCompany)) +
  geom_text(aes(y = (..count..), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = 0.5, hjust=-0.1 ,angle = 90) +
  labs(title = "Years At Company", y = "Count", x = "YearsAtCompany")+
  scale_fill_continuous("Years",low = "darkred", high = "darkgreen")

## Employees working in the XYZ organisation mostly stay from 0 to 10 years.
  
  
  
  
  # 28. JOB INVOLVEMENT

table(combined_employee_details$JobInvolvement)

combined_employee_details %>%
  ggplot(aes(x = JobInvolvement, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = as.factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes(y = (..prop..), label = scales::percent((..prop..)/sum(..prop..))), stat = "count", vjust = -0.5)+
  labs(title = "Job Involvement Vs Attrition", y = "Percentage Attrition", x = "Job Involvement")+
  facet_grid(~Attrition)

#The majority of the employees show a high job involvement.


# 29. PERFORMANCE RATING

table(combined_employee_details$PerformanceRating)

ggplot(combined_employee_details, aes(x = PerformanceRating, fill = as.factor(Attrition))) +
  geom_bar()

group_by(combined_employee_details, PerformanceRating) %>%
  summarise(prom_perc = sum(Attrition, na.rm = T)/n()*100) %>%
  arrange(prom_perc)

## Performance rating 3 and 4 have almost same percent of Attrition.



# 30. WORKED HOURS MEAN

ggplot(combined_employee_details, aes(x = as.integer(worked_hours_mean), fill = as.factor(Attrition))) +
  geom_bar()

group_by(combined_employee_details, round(worked_hours_mean,0)) %>%
  summarise(prom_perc = sum(Attrition, na.rm = T)/n()*100) %>%
  arrange(prom_perc)

## Employees working more than 8 hours throughout are more likely to leave the Company.


# 31.OUT OF OFFICE
combined_employee_details[which(combined_employee_details$undertime_count > 1),] %>%
  ggplot(aes(x = out_of_office, group = Attrition)) + 
  geom_bar(aes(y = ..prop..)) + 
  scale_y_continuous(labels=scales::percent) +
  labs(title = "out_of_office Vs Attrition", y = "Percentage Attrition", x = "out of office Frequency")+
  facet_grid(~Attrition)
# Irregular Employees are more likely to leave the company.


# 32.UNDER TIME
combined_employee_details[which(combined_employee_details$undertime_count > 1),] %>%
  ggplot(aes(x = undertime_count, group = Attrition)) + 
  geom_bar(aes(y = ..prop..)) + 
  scale_y_continuous(labels=scales::percent) +
  labs(title = "Undertiming Vs Attrition", y = "Percentage Attrition", x = "Undertime Frequency")+
  facet_grid(~Attrition)
# people who undertime on continuous pace alot tend to leave the company.





#-------------------------------------------TREATING MISSING VALUES---------------------------------------------------------#



# 1. Environment Satisfaction


combined_employee_details[is.na(combined_employee_details$EnvironmentSatisfaction),]

## We Observe that the attrition of missing values of environment Satisfaction is mostly 0 ie. They are less likely to leave.
## We also observed in Data Visualization that as the level of environment Satisfaction increases attrition percentage decrease.

summary(combined_employee_details$EnvironmentSatisfaction)
table(combined_employee_details$EnvironmentSatisfaction)

## So replacing missing values by mode 3.

combined_employee_details$EnvironmentSatisfaction[is.na(combined_employee_details$EnvironmentSatisfaction)] <- 3



# 2. Job Satisfaction

combined_employee_details[is.na(combined_employee_details$JobSatisfaction),]

summary(combined_employee_details$JobSatisfaction)

table(combined_employee_details$JobSatisfaction)

## We Observe that the attrition of missing values of Job Satisfaction is mostly 0 ie. They are less likely to leave.

## So replacing missing values by mode 3.

combined_employee_details$JobSatisfaction[is.na(combined_employee_details$JobSatisfaction)] <-3


# 3. WORK LIFE BALANCE


combined_employee_details[is.na(combined_employee_details$WorkLifeBalance),]

summary(combined_employee_details$WorkLifeBalance)

table(combined_employee_details$WorkLifeBalance)

## We Observe that the attrition of missing values of Work Life Balance is mostly 0 ie. They are less likely to leave.

## So replacing missing values by mode 3.

combined_employee_details$WorkLifeBalance  [is.na(combined_employee_details$WorkLifeBalance )] <-3


# 4.Number Companies Worked


combined_employee_details[is.na(combined_employee_details$NumCompaniesWorked),]

summary(combined_employee_details$NumCompaniesWorked)

#### MICE IMPUTATION #####

require(mice)
mice_df <- combined_employee_details[ ,c(2, 3, 4, 7, 15,18,20, 23, 24,25, 26, 27, 28)]

set.seed(11)
mice_imp <- mice(mice_df)
df_1 <- complete(mice_imp)

par(mfrow = c(1,2))
hist(combined_employee_details$NumCompaniesWorked, col = 'skyblue')
hist(df_1$NumCompaniesWorked, col = 'lightgreen')

combined_employee_details$NumCompaniesWorked <- df_1$NumCompaniesWorked

summary(combined_employee_details$NumCompaniesWorked)



# 4. TOTAL WORKING YEARS

## Since only 9 values are missing.
##To achieve this we take the value of NumCompaniesWorked for the corresponding row of NA TotalWorkingYears 
##and compute the median of TotalWorkingYears with all rows having same NumCompaniesWorked

summary(combined_employee_details$TotalWorkingYears)

combined_employee_details[which(is.na(combined_employee_details$TotalWorkingYears)),"TotalWorkingYears"] <- sapply(combined_employee_details[which(is.na(combined_employee_details$TotalWorkingYears)),"NumCompaniesWorked"], function(x){
  round(median(combined_employee_details$TotalWorkingYears[which(combined_employee_details$NumCompaniesWorked == x)],na.rm = T))
})

summary(combined_employee_details$TotalWorkingYears)

colSums(is.na(combined_employee_details))


#--------------------------------------------------MISSING VALUES TREATMENT OVER--------------------------------------------#




#-------------------------------------------------REMOVING SPARSE COLUMNS--------------------------------------------------#


# REMOVING COLUMNS WHICH HAVE SAME VALUES FOR ALL.

# checking columns having same values in all.
vapply(combined_employee_details, function(x) length(unique(x)) > 1, logical(1L))

# removing columns having same value in all.
combined_employee_details <-combined_employee_details[colSums(combined_employee_details[1,][col(combined_employee_details)]!=combined_employee_details)!=0]



#----------------------------------------------------OUTLIERS TREATMENT ---------------------------------------------------#




# Checking for Outliers

sapply(combined_employee_details, function(x){if(is.numeric(x)){boxplot.stats(x)$out}})

# GOT VARIABLES FOR OUTLIER TREATMENT :
#  TOTAL WORKING YEARS
#  YEARS AT COMPANY
#  YEARS WITH CURRENT MANAGER
#  UNDERTIME COUNT



# Checking and removing outliers .

outlier <- function(vect){
  Q <- as.numeric(quantile(vect))
  min <- Q[1]
  Q1 <- Q[2]
  Q2 <- Q[3]
  Q3 <- Q[4]
  max <-Q[5]
  IQR <- Q3-Q1
  LO <- Q1-1.5*IQR
  UO <- Q3+1.5*IQR
  for(i in 1:length(vect)){
    if (vect[i]>UO) {
      vect[i]=UO
    }else{
      if(vect[i] < LO){
        vect[i]=LO
      }else{
        vect[i]
      }
    }
    
  }
  return(vect)
}



# Treating outliers.


combined_employee_details$TotalWorkingYears <- outlier(combined_employee_details$TotalWorkingYears)
combined_employee_details$YearsAtCompany <- outlier(combined_employee_details$YearsAtCompany)
combined_employee_details$YearsWithCurrManager <- outlier(combined_employee_details$YearsWithCurrManager)
combined_employee_details$undertime_count <- outlier(combined_employee_details$YearsWithCurrManager)


# Checking Plots.


# 1. TOTAL WORKING YEARS

plot(quantile(combined_employee_details$TotalWorkingYears, seq(0, 1, 0.01), na.rm = T))


# 2. YEARS AT COMPANY

plot(quantile(combined_employee_details$YearsAtCompany, seq(0, 1, 0.01), na.rm = T))


# 3. YEARS WITH CURRENT MANAGER

plot(quantile(combined_employee_details$YearsWithCurrManager, seq(0, 1, 0.01), na.rm = T))



# 4. UNDERTIME COUNT

plot(quantile(combined_employee_details$undertime_count, seq(0, 1, 0.01), na.rm = T))





#-------------------------------------------------------OUTLIERS TREATMENT OVER--------------------------------------------#




#------------------------------------------------------SCALING AND FACTORING-----------------------------------------------#


glimpse(combined_employee_details)
combined_employee_details$EnvironmentSatisfaction <- as.factor(combined_employee_details$EnvironmentSatisfaction)
combined_employee_details$JobSatisfaction <- as.factor(combined_employee_details$JobSatisfaction)
combined_employee_details$WorkLifeBalance <- as.factor(combined_employee_details$WorkLifeBalance)
combined_employee_details$JobLevel <- as.factor(combined_employee_details$JobLevel)
combined_employee_details$StockOptionLevel <- as.factor(combined_employee_details$StockOptionLevel)
combined_employee_details$JobInvolvement <- as.factor(combined_employee_details$JobInvolvement)
combined_employee_details$PerformanceRating <- as.factor(combined_employee_details$PerformanceRating)
combined_employee_details$Education <- as.factor(combined_employee_details$Education)
combined_employee_details$Attrition <- as.factor(combined_employee_details$Attrition)
combined_employee_details$EmployeeID <- as.factor(combined_employee_details$EmployeeID)
combined_employee_details <- mutate_if(combined_employee_details , is.character, as.factor)



combined_employee_details <- mutate_if(combined_employee_details , is.numeric, scale)

## FOR DUMMIES ##
combined_employee_details$EmployeeID <-as.numeric(combined_employee_details$EmployeeID)
combined_employee_details$Attrition <- as.numeric(combined_employee_details$Attrition)


#--------------------------------------------------------MODEL BUILDING----------------------------------------------------#


require(dummies)

master_HR_dummy <- dummy.data.frame(combined_employee_details)


# Splitting the master HR dataset into Train and Test datasets

master_HR_dummy$Attrition <- as.factor(master_HR_dummy$Attrition)

require(caTools)

set.seed(37)
index <- sample.split(master_HR_dummy$Attrition , SplitRatio = 0.75)
train_HR_df <- master_HR_dummy[index , ]  
test_HR_df <- master_HR_dummy[!index , ]  

# Checking if the ratios are maintained

prop.table(table(master_HR_dummy$Attrition)) * 100
prop.table(table(train_HR_df$Attrition)) * 100
prop.table(table(test_HR_df$Attrition)) * 100



#---------------------------------------------------------LOGISTIC REGRESSION----------------------------------------------#


log_model_1 <- glm(Attrition ~ . , data = train_HR_df[ , -1], family = "binomial")
summary(log_model_1) # AIC 

log_model_2 <- step( log_model_1, direction = "both")

require(car)
sort(vif(log_model_2))
summary(log_model_2)



# Removing EducationField Life Sciences due to high variance and low significance  
 log_model_3 <- glm(formula = Attrition ~ EnvironmentSatisfaction1 + EnvironmentSatisfaction2 + 
      EnvironmentSatisfaction3 + JobSatisfaction1 + JobSatisfaction2 + 
      JobSatisfaction3 + WorkLifeBalance1 + WorkLifeBalance3 + 
      Age + `BusinessTravelNON-TRAVEL` + BusinessTravelTRAVEL_FREQUENTLY + 
      `DepartmentHUMAN RESOURCES` + Education2 + `EducationFieldHUMAN RESOURCES` + 
       EducationFieldMEDICAL + JobLevel2 + 
      `JobRoleMANUFACTURING DIRECTOR` + `JobRoleRESEARCH DIRECTOR` + 
      `JobRoleRESEARCH SCIENTIST` + `JobRoleSALES EXECUTIVE` + 
      MaritalStatusDIVORCED + MaritalStatusMARRIED + NumCompaniesWorked + 
      StockOptionLevel0 + StockOptionLevel2 + TotalWorkingYears + 
      TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
      JobInvolvement3 + out_of_office + overtime_count + undertime_count + 
      Education5, family = "binomial", data = train_HR_df[, -1])



sort(vif(log_model_3))
summary(log_model_3)


# Removing Environment Satisfaction 3 due to high variance and low significance  

log_model_4 <- glm(formula = Attrition ~ EnvironmentSatisfaction1 + EnvironmentSatisfaction2 + 
                      JobSatisfaction1 + JobSatisfaction2 + 
                     JobSatisfaction3 + WorkLifeBalance1 + WorkLifeBalance3 + 
                     Age + `BusinessTravelNON-TRAVEL` + BusinessTravelTRAVEL_FREQUENTLY + 
                     `DepartmentHUMAN RESOURCES` + Education2 + `EducationFieldHUMAN RESOURCES` + 
                     EducationFieldMEDICAL + JobLevel2 + `JobRoleMANUFACTURING DIRECTOR` + 
                     `JobRoleRESEARCH DIRECTOR` + `JobRoleRESEARCH SCIENTIST` + 
                     `JobRoleSALES EXECUTIVE` + MaritalStatusDIVORCED + MaritalStatusMARRIED + 
                     NumCompaniesWorked + StockOptionLevel0 + StockOptionLevel2 + 
                     TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + JobInvolvement3 + out_of_office + 
                     overtime_count + undertime_count + Education5, family = "binomial", 
                   data = train_HR_df[, -1])


 sort(vif(log_model_4))
 summary(log_model_4)

 
 # Removing Job Satisfaction 2 due to no significance.
 
 log_Model_5 <- glm(formula = Attrition ~ EnvironmentSatisfaction1 + EnvironmentSatisfaction2 + 
       JobSatisfaction1 + JobSatisfaction3 + 
       WorkLifeBalance1 + WorkLifeBalance3 + Age + `BusinessTravelNON-TRAVEL` + 
       BusinessTravelTRAVEL_FREQUENTLY + `DepartmentHUMAN RESOURCES` + 
       Education2 + `EducationFieldHUMAN RESOURCES` + EducationFieldMEDICAL + 
       JobLevel2 + `JobRoleMANUFACTURING DIRECTOR` + `JobRoleRESEARCH DIRECTOR` + 
       `JobRoleRESEARCH SCIENTIST` + `JobRoleSALES EXECUTIVE` + 
       MaritalStatusDIVORCED + MaritalStatusMARRIED + NumCompaniesWorked + 
       StockOptionLevel0 + StockOptionLevel2 + TotalWorkingYears + 
       TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
       JobInvolvement3 + out_of_office + overtime_count + undertime_count + 
       Education5, family = "binomial", data = train_HR_df[, -1])
 
sort(vif(log_Model_5)) 
summary(log_Model_5)


# Removing Job Satisfaction 3 due to no significance.


log_model_6 <- glm(formula = Attrition ~ EnvironmentSatisfaction1 + EnvironmentSatisfaction2 + 
                     JobSatisfaction1 +Age + `BusinessTravelNON-TRAVEL` + BusinessTravelTRAVEL_FREQUENTLY + 
                     `DepartmentHUMAN RESOURCES` + Education2 + `EducationFieldHUMAN RESOURCES` + 
                     EducationFieldMEDICAL + JobLevel2 + `JobRoleMANUFACTURING DIRECTOR` + 
                     `JobRoleRESEARCH DIRECTOR` + `JobRoleRESEARCH SCIENTIST` + 
                     `JobRoleSALES EXECUTIVE` + MaritalStatusDIVORCED + MaritalStatusMARRIED + 
                     NumCompaniesWorked + StockOptionLevel0 + StockOptionLevel2 + 
                     TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + JobInvolvement3 + out_of_office + 
                     overtime_count + undertime_count + Education5, family = "binomial", 
                   data = train_HR_df[, -1])


sort(vif(log_model_6)) 
summary(log_model_6)


# Removing Environment Satisfaction2 due to no significance.


log_model_7 <- glm(formula = Attrition ~ EnvironmentSatisfaction1 +
                     JobSatisfaction1 + Age + `BusinessTravelNON-TRAVEL` + BusinessTravelTRAVEL_FREQUENTLY + 
                     `DepartmentHUMAN RESOURCES` + Education2 + `EducationFieldHUMAN RESOURCES` + 
                     EducationFieldMEDICAL + JobLevel2 + `JobRoleMANUFACTURING DIRECTOR` + 
                     `JobRoleRESEARCH DIRECTOR` + `JobRoleRESEARCH SCIENTIST` + 
                     `JobRoleSALES EXECUTIVE` + MaritalStatusDIVORCED + MaritalStatusMARRIED + 
                     NumCompaniesWorked + StockOptionLevel0 + StockOptionLevel2 + 
                     TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + JobInvolvement3 + out_of_office + 
                     overtime_count + undertime_count + Education5, family = "binomial", 
                   data = train_HR_df[, -1])

sort(vif(log_model_7)) 
summary(log_model_7)


# Removing EducationField Medical due to no significance.


log_model_8 <- glm(formula = Attrition ~ EnvironmentSatisfaction1 + JobSatisfaction1 + 
                     Age + `BusinessTravelNON-TRAVEL` + BusinessTravelTRAVEL_FREQUENTLY + 
                     `DepartmentHUMAN RESOURCES` + Education2 + `EducationFieldHUMAN RESOURCES` + 
                      JobLevel2 + `JobRoleMANUFACTURING DIRECTOR` + 
                     `JobRoleRESEARCH DIRECTOR` + `JobRoleRESEARCH SCIENTIST` + 
                     `JobRoleSALES EXECUTIVE` + MaritalStatusDIVORCED + MaritalStatusMARRIED + 
                     NumCompaniesWorked + StockOptionLevel0 + StockOptionLevel2 + 
                     TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + JobInvolvement3 + out_of_office + 
                     overtime_count + undertime_count + Education5, family = "binomial", 
                   data = train_HR_df[, -1])


sort(vif(log_model_8)) 
summary(log_model_8)


# Removing Department Human Resources due to high variance and low significance.


log_model_9 <- glm(formula = Attrition ~ EnvironmentSatisfaction1 + JobSatisfaction1 + 
      Age + `BusinessTravelNON-TRAVEL` + BusinessTravelTRAVEL_FREQUENTLY + 
       Education2 + `EducationFieldHUMAN RESOURCES` + 
      JobLevel2 + `JobRoleMANUFACTURING DIRECTOR` + `JobRoleRESEARCH DIRECTOR` + 
      `JobRoleRESEARCH SCIENTIST` + `JobRoleSALES EXECUTIVE` + 
      MaritalStatusDIVORCED + MaritalStatusMARRIED + NumCompaniesWorked + 
      StockOptionLevel0 + StockOptionLevel2 + TotalWorkingYears + 
      TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
      JobInvolvement3 + out_of_office + overtime_count + undertime_count + 
      Education5, family = "binomial", data = train_HR_df[, -1])

sort(vif(log_model_9)) 
summary(log_model_9)


# Removing Education 5 due to no significance.

  log_model_10 <- glm(formula = Attrition ~ EnvironmentSatisfaction1 + JobSatisfaction1 + 
                        Age + `BusinessTravelNON-TRAVEL` + BusinessTravelTRAVEL_FREQUENTLY + 
                        Education2 + `EducationFieldHUMAN RESOURCES` + JobLevel2 + 
                        `JobRoleMANUFACTURING DIRECTOR` + `JobRoleRESEARCH DIRECTOR` + 
                        `JobRoleRESEARCH SCIENTIST` + `JobRoleSALES EXECUTIVE` + 
                        MaritalStatusDIVORCED + MaritalStatusMARRIED + NumCompaniesWorked + 
                        StockOptionLevel0 + StockOptionLevel2 + TotalWorkingYears + 
                        TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                        JobInvolvement3 + out_of_office + overtime_count + undertime_count,
                          family = "binomial", data = train_HR_df[, -1])


  sort(vif(log_model_10)) 
  summary(log_model_10)
  
  
  
  log_model_11 <- glm(formula = Attrition ~ EnvironmentSatisfaction1 + JobSatisfaction1 + 
                        Age + `BusinessTravelNON-TRAVEL` + BusinessTravelTRAVEL_FREQUENTLY + 
                        Education2 + `EducationFieldHUMAN RESOURCES` + JobLevel2 + 
                        `JobRoleMANUFACTURING DIRECTOR` + `JobRoleRESEARCH DIRECTOR` + 
                        `JobRoleRESEARCH SCIENTIST` + `JobRoleSALES EXECUTIVE` + 
                        MaritalStatusDIVORCED + MaritalStatusMARRIED + NumCompaniesWorked + 
                        StockOptionLevel0 + StockOptionLevel2 + TotalWorkingYears + 
                        TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                        JobInvolvement3 + overtime_count + undertime_count, 
                      family = "binomial", data = train_HR_df[, -1])
  
  
  sort(vif(log_model_11)) 
  summary(log_model_11)
  
  ### Now all variables are significant.
  
  
#---------------------------------------------------PREDICTION ON TEST DATA-------------------------------------------------#
  
  
  # NOW WE START PREDICTION USING OUR REGRESSION MODEL.
  
  
  
  pred_attrition_1 <- predict(log_model_11 , newdata = test_HR_df[,-1] , type = 'response')
  
  summary(pred_attrition_1)
  
  
  # SETTING THRESHOLD AS MEAN.
  
  
  pred_attrition <- as.factor(ifelse(pred_attrition_1 > 0.16,1,0))
  
  
  
  # CHECKING ACCURACY USING CONFUSION MATRIX 
  
  
  require(caret)
  
  test_HR_df$Attrition <- as.factor(test_HR_df$Attrition)
  confusionMatrix( pred_attrition,test_HR_df$Attrition, positive = '1')
  
  
  # WE GOT BALANCED SPECIFICITY AND SENSITIVITY BUT LESS ACCURACY.
  
  ##   Accuracy : 0.7316  
  ##   Sensitivity : 0.7360          
  ##   Specificity : 0.7308 
  
  
  cutoff_for_balance(pred_attrition_1,test_HR_df$Attrition)
  
  
  # CHECKING ACCURACY WITH ROC CURVE .
  
  # ROC CURVE .
  
  pred_attrition_1 <- as.data.frame(pred_attrition_1)
  
  Attrition <- as.data.frame(test_HR_df$Attrition)
  
  df_roc <- bind_cols(pred_attrition_1,Attrition)
  
  
  df_roc <- arrange(df_roc, desc(pred_attrition_1))
  
  colnames(df_roc)[2] <- "Attrition"
  
  
  
  
  require(ROCR)
  

  Pred_roc <- prediction(predictions = df_roc$pred_attrition_1, labels = df_roc$Attrition)
  
  perf <- performance(Pred_roc ,"tpr", "fpr")
  
  
  plot(perf, colorsize = TRUE)
  

  
  
  
  
#-------------------------------------------REGRESSION MODEL OVER----------------------------------------------------------#  
  
  
  
  

  
#----------------------------------------------------RANDOM FOREST---------------------------------------------------------#
  
  
 
  
  
  
  # CORRECTING STRUCTURE.
  
  glimpse(combined_employee_details)
  
  combined_employee_details$Attrition <- as.factor(combined_employee_details$Attrition)
  
  
  
  
  # Now all variables are in correct structure.
  
  
  # SPLITTING DATASET.
  
  require(caTools)
  set.seed(9921)
  index_r <- sample.split(combined_employee_details$Attrition , SplitRatio = 0.90)
  
  train_RF <- combined_employee_details[index_r, ]
  test_RF <- combined_employee_details[!index_r, ]
  
  
  # CHECKING PROPORTION .
  
  
  prop.table(table(combined_employee_details$Attrition))*100
  prop.table(table(train_RF$Attrition))*100
  prop.table(table(test_RF$Attrition))*100
  table(combined_employee_details$Attrition)
  
  
  # BUILDING MODEL.
  
  require(randomForest)
  
  RF_model <- randomForest(Attrition~., data = train_RF[,-1], ntree = 200,
                           do.trace = T)
  
  
  pred_RF <- predict(RF_model, newdata = test_RF[,-1], type = "prob")
  pred_RF<- as.data.frame(pred_RF)
  summary(pred_RF)
  
  pred_RF_final <- as.factor(ifelse(pred_RF[,2] > 0.28,1,0))
  
  summary(pred_RF_final)
  
  
  require(e1071)
  
  confusionMatrix(pred_RF_final, test_RF$Attrition, positive = '1')
  
 
  #Accuracy : 1  
  #Sensitivity : 1.000      
  #Specificity : 1.000  
  
  #Accuracy and Balance Achieved.
  
  
  
  
#-----------------------------------------------------------SVM----------------------------------------------------------------#
  

  
  master_HR_dummy$Attrition <- as.factor(ifelse(master_HR_dummy$Attrition == 1,'YES','NO'))
  
  # SPLITTING DATASET.
  
  require(caTools)
  set.seed(993)
  index_svm <- sample.split( master_HR_dummy$Attrition , SplitRatio = 0.70)
  
  train_svm <- master_HR_dummy[index_svm, ]
  test_svm <- master_HR_dummy[!index_svm, ]
  
  
  # CHECKING PROPORTION .
  
  prop.table(table(master_HR_dummy$Attrition))*100
  prop.table(table(train_svm$Attrition))*100
  prop.table(table(test_svm$Attrition))*100


  
  # TUNNING HYPER PARAMETERS.
  
  x = train_svm[,-c(1,15)]
  
  y = train_svm[,15]
  
  
  require(e1071)
  
  svm_tune <- tune(svm,train.x=train_svm[,-c(1,15)], train.y= train_svm[,15],
                   ranges=list(cost=10^(-2:2),gamma=2^(-2:2)))
  
  tuned_params <- summary(svm_tune)
  
  tuned_params$best.parameters #cost = 1, gamma 0.25
 
  
  # LINEAR CLASSIFIER .
      
  
  svm_model_linear <- svm(Attrition~ ., data = train_svm[,-1], kernel = "linear", scale = F, probability = TRUE,
                          cost = 1, gamma = 0.25)
  
  
  pred_linear <- predict(svm_model_linear, newdata = test_svm[,-1], probability = TRUE)
  
  
  summary(pred_linear)
  
  prob_linear <- attr(pred_linear,"probabilities")
  
  View(prob_linear)
  summary(prob_linear)
  
  linear_set <- as.factor(ifelse(prob_linear[,1] > 0.15, "YES", "NO"))
  
  require(caret)
  
  confusionMatrix(linear_set,test_svm$Attrition, positive = "YES")
  
  
  ##   Accuracy    : 0.7205
  ##   Sensitivity : 0.7277          
  ##   Specificity : 0.7108 
  
  
  
  # RADIAL KERNEL.
  
  svm_model_radial <- svm(Attrition ~ .,data = train_svm[,-1], kernel = "radial", scale = FALSE,
                       probability = TRUE,cost = 1, gamma = 0.25)
  
  pred_radial <- predict(svm_model_radial, newdata = test_svm[,-1], probability = TRUE)
  
  summary(pred_radial)
  
  prob_radial <- attr(pred_radial,"probabilities")
  
  summary(prob_radial)
  
  View(prob_radial)
  
  radial_set <- as.factor(ifelse(prob_radial[,1] > 0.163,'YES','NO'))
  
  
  confusionMatrix(radial_set, test_svm$Attrition, positive = "YES") 
  
  
  
  ##   Accuracy : 0.9554 
  ## Sensitivity : 0.9408          
  ## Specificity : 0.9640 
  
  
  # POLYNOMIAL KERNEL.
  
  
  svm_model_polynomial <- svm(Attrition ~ .,data = train_svm[,-1], kernel = "polynomial", scale = F,
                       probability = T, cost = 1, gamma = 0.25)
  
  pred_polynomial <- predict(svm_model_polynomial, newdata = test_svm[,-1], probability = TRUE)
  
  prob_polynomial <- attr(pred_polynomial,"probabilities")
  
  summary(prob_polynomial)
  
  polynomial_set <- as.factor(ifelse(prob_polynomial[,1] > 0.165, "YES", "NO"))
  
  confusionMatrix(polynomial_set, test_svm$Attrition, positive = "YES") 
  
  
  
  ##   Accuracy : 0.9705 
  ##   Sensitivity : 0.9549         
  ##   Specificity : 0.9793 
  
#-------------------------------------------------------SVM ANALYSIS OVER------------------------------------------------------#
  
  
  # LINEAR REGRESSION MODEL ACCURACY  :   #Accuracy : 0.7316  
 
  
  # RANDOM FOREST MODEL ACCURACY      :   #Accuracy : 1.00
  
  
  # SUPPORT VECTOR MODEL ACCURACY     :   #Accuracy  : 0.7105(LINEAR KERNEL) 
                                          #Accuracy : 0.9554(RADIAL KERNEL)   
                                          #Accuracy : 0.9705 (POLYNOMIAL KERNAL)
  
  
  # HENCE CONSIDERING RANDOM FOREST PREDICTION FOR THE RESULTS.
  

#------------------------------------------------------------------------------------------------------------------------------#    
  
  
