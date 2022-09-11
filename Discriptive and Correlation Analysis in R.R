
# This project was completed in one of my public health classes. The data set was provided by the instructor and was cleaned in Excel.
# The data set contains information on the health and demographic information of 502 participants.


# Importing working data into R
library(readxl)
Prostate <- read_excel("D:/Analytics/Data/R Projects/WEEK 4/Prostate.xlsx")
View(Prostate)

# Examining the structure and dimension of data
str(Prostate)
dim(Prostate)

# Creating factor variables rx_f and status_f from the character variables treatment (rx) and status
Prostate$rx_f     <-  factor(Prostate$rx)
Prostate$status_f <- factor(Prostate$status)


# Summarizing the categorical variables rx_f and status_f (i.e., Obtaining frequency tables)
table(Prostate$rx_f)
table(Prostate$status_f)


# Obtaining relative frequency tables (proportions or %s) of rx_f and status_f
prop.table(table(Prostate$rx_f))
prop.table(table(Prostate$status_f))


# Creating a new dichotomous variable, died, from the variable status using for loop
for (i in (1:502))
{
  if (Prostate$status[i] == "alive") 
  {
    Prostate$died[i] = "No"
  }
  else 
  {
    Prostate$died[i] = "Yes"
  }
}


# Converting the new character variable, dead, to a factor
Prostate$died_f <-  factor(Prostate$died)


# Obtaining a cross-tab (with counts) of rx_f and died_f
table(Prostate$rx_f, Prostate$died_f)


# Obtaining a cross-tab (with cell %s) of rx_f and died_f
(prop.table(table(Prostate$rx_f, Prostate$died_f)))*100


# Obtaining relative frequency tables (with row %s) of rx_f and died_f 
prop.table(table(Prostate$rx_f, Prostate$died_f), 1)


# Obtain relative frequency tables (with column %s) of rx_f and died_f
prop.table(table(Prostate$rx_f, Prostate$died_f), 2)
  

# Summarizing the continuous variables age, weight(wt), systolic blood pressure (sbp), diastolic blood pressure (dbp), hg, sz and sg.  
# Descriptive statistics
summary(Prostate$age)
summary(Prostate$wt)
summary(Prostate$sbp)
summary(Prostate$dbp)
summary(Prostate$hg)
summary(Prostate$sz)
summary(Prostate$sg)


# Using Hmisc packgage to obtain additonal discriptive statistics (i.e., percentiles) that were not availabe in the previous packgage used 
library(Hmisc)

# Obtain summary descriptive statistics on age, wt, sbp, dbp, hg, sz and sg using the describe() function.  
describe(Prostate$age)
describe(Prostate$wt)
describe(Prostate$sbp)
describe(Prostate$dbp)
describe(Prostate$hg)
describe(Prostate$sz)
describe(Prostate$sg)

bystats(Prostate$age, 0)
bystats(Prostate$wt, 0)
bystats(Prostate$sbp, 0)
bystats(Prostate$dbp, 0)
bystats(Prostate$hg, 0)
bystats(Prostate$sz, 0)
bystats(Prostate$sg, 0)

bystats(Prostate$age, 0, fun=function(x) c(Mean=mean(x), Median=median(x), Mode=mode(x), SD=sd(x), quantile(x)))
bystats(Prostate$wt, 0, fun=function(x) c(Mean=mean(x), Median=median(x), Mode=mode(x), SD=sd(x), quantile(x)))
bystats(Prostate$sbp, 0, fun=function(x) c(Mean=mean(x), Median=median(x), Mode=mode(x), SD=sd(x), quantile(x)))
bystats(Prostate$dbp, 0, fun=function(x) c(Mean=mean(x), Median=median(x), Mode=mode(x), SD=sd(x), quantile(x)))
bystats(Prostate$hg, 0, fun=function(x) c(Mean=mean(x), Median=median(x), Mode=mode(x), SD=sd(x), quantile(x)))
bystats(Prostate$sz, 0, fun=function(x) c(Mean=mean(x), Median=median(x), Mode=mode(x), SD=sd(x), quantile(x)))
bystats(Prostate$sg, 0, fun=function(x) c(Mean=mean(x), Median=median(x), Mode=mode(x), SD=sd(x), quantile(x)))


# Examining correlataions- Using the subset function to create a dataframe with only the continuous variables of interest from the Prostrate dataset
Prostate_ContinuousVars <-  subset(Prostate, select = c(age, wt, sbp, dbp, hg, sz, sg))
View(Prostate_ContinuousVars)

# Obtaining the Pearson and Spearman correlation matrices using the cor() function with complete.obs option to remove rows 
# with missing data for any of the continuous variables selected
cor_pearson <- cor(Prostate_ContinuousVars, method = c("pearson"), use="complete.obs")
cor_spearman <- cor(Prostate_ContinuousVars, method = c("spearman"), use="complete.obs")

# Using the round() function on the output of the cor() function to round the results to 2 decimal places.
round(cor_pearson, 2)
round(cor_spearman, 2)

# Plotting the correlation outputs (correlograms)
library(corrplot)
corrplot(corr=cor_pearson, type="upper", method="ellipse", tl.col = "black")
corrplot(corr=cor_spearman, type="lower", method="ellipse", tl.col = "black")

# Round the correlation matrices to 1 decimal place for easy interpretation  
round(cor_pearson, 1)
round(cor_spearman, 1)


# Ouputs were exported to a pdf document using R markdown and have been attached to this repository. 



