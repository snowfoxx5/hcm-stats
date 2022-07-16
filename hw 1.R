##### Intro to Statistics
### Instructor: Kent Oliver Bhupathi, MEcon
# Pre-class: R & RStudio Basics
#Student name: Magdalene Chai

################################################################################
##### Clearing the R environment and console

### Clean the environment
rm(list=ls())

### Clear the console
cat("\014")

################################################################################
##### Reading-in Data/Datasets

### Example #2: CSV files
# Defining the file-path:

data_folderpath <- "/Users/magc/Desktop/homework/HCM stats/Homework 1/dataset/"

datadoc_1 <- paste0(data_folderpath,
                    "hw_dataset.csv")
dataset <- data.frame(read.csv(datadoc_1))
dim(dataset) # 1999   12

################################################################################
##### Cleaning

# Remove all but a single R object
rm(list=setdiff(ls(), c("dataset")))
##Removing any rows with NAs (i.e. missing values)
dataset[complete.cases(dataset),]

### Subsetting:
View(dataset)
names(dataset)
#subsetting row of bonus_potential = 1 with all columns as bonusset
bonusset <- dataset[which(dataset$bonus_potential == 1),] 
View(bonusset) # check if data is 
#subsetting row of bonus_potential = 0 with all columns as nobonusset
nobonusset <- dataset[which(dataset$bonus_potential == 0),]
View(nobonusset) # check if data is correct

# install.packages("pastecs")
pastecs_stats_1 <- data.frame(
  round(
    pastecs::stat.desc(bonusset),2
  )
) 
View(pastecs_stats_1)
pastecs_stats <- data.frame(
  round(
    pastecs::stat.desc(nobonusset),2
  )
)
View(pastecs_stats)

# 3 variables within bonusset ("rate_hrly", "avg_wkly_hrs", "age")
library("dplyr")
names(dataset)

three_variables <- dataset |>
  group_by(bonus_potential) |>
  summarize ((standd_rate = sd(rate_hrly)),(standd_avg = sd(avg_wkly_hrs)), (standd_age = sd(age)))
View(three_variables)

round(sd(dataset$rate_hrly),2) #68.09 
round(sd(bonusset$rate_hrly),2) #62.16
round(sd(nobonusset$rate_hrly),2) #56.68

round(sd(dataset$avg_wkly_hrs),2) #6.55
round(sd(bonusset$avg_wkly_hrs),2) #6.98
round(sd(nobonusset$avg_wkly_hrs),2) #6.32

round(sd(dataset$age),2) #16.2
round(sd(nobonusset$age),2) #16.17
round(sd(bonusset$age),2) #15.85

### Skewness in age for both subsets
#install.packages("moments")
library(moments)
skewness(bonusset$age) # 0.2776852
skewness(nobonusset$age) #0.04614685

#nobonusset has a more normal distribution for age
###############
### Kurtosis in age for both subsets
library(moments)
kurtosis(bonusset$age) #2.048936
kurtosis(nobonusset$age) #1.926019
#nobonusset has a more normal distribution for age 


######################4############################
#histogram
hist(dataset$age) # histogram for dataset with age variable
hist(bonusset$age) # histogram for bonusset with age variable
hist(nobonusset$age) # histogram for nobonusset with age variable

# Yes, the histogram from bonusset differs from the nobunusset
# The historgram for nobunusset with the age variable has a much more similar pattern as compared to the original dataset's histogram
# this could be because the nobonusset data has a considered more "normal" distribution with less skewness and kurtosis.
#IT could mean that the employees in nobonusset represent the majority of the employees while the employees represented in bonusset have larger data in the younger ages

rm(list=setdiff(ls(), c("dataset"))) # remove all but original data set

##########################5############################
### Subsetting:
View(dataset)
names(dataset)
#subsetting row of wfh = 1 with all columns as wfhset
wfhset <- dataset[which(dataset$wfh == 1),] 
View(wfhset) # check if data is 
#subsetting row of wfh = 0 with all columns as nowfhset
nowfhset <- dataset[which(dataset$wfh == 0),]
View(nowfhset) # check if data is correct
###### mean of absent_days_yr within wfh and nowfh
library("dplyr")
mean_absentdays <- dataset |>
  group_by(wfh) |>
  summarize ((mean_absent = mean(absent_days_yr)))
View(mean_absentdays)

absent_mean_wfh <- round(mean(wfhset$absent_days_yr),2) #7.33
absent_mean_nowfh <- round(mean(nowfhset$absent_days_yr),2) #6.73
difference_mean <- absent_mean_wfh - absent_mean_nowfh #0.6
## the mean of "absent_days_yr" with the wfh set is 0.6 greater than nowfh set

###### mean of avg_wkly_hr within wfh and nowfh
library("dplyr")
mean_wklyhrs <- dataset |>
  group_by(wfh) |>
  summarize ((mean_wkly = mean(avg_wkly_hrs)))
View(mean_wklyhrs)

avg_mean_wfh <- round(mean(wfhset$avg_wkly_hrs),2) #61.56
avg_mean_nowfh <- round(mean(nowfhset$avg_wkly_hrs),2) #58.51
difference_mean_avg <- avg_mean_wfh - avg_mean_nowfh #3.05
#### the avg mean of avg_wkly_hrs for wfhset is 3.05 hours a week more than the nowfhset
### the difference in part C offsets the differences in part B
### based on the differences, it seems to suggests that the employees in wfh who work on an ave 3.05 hours more weekly tend to take 0.6 days more absent days in a year.

#cleaning
rm(list=setdiff(ls(), c("dataset"))) # remove all but original data set

#####################6################
#### Pearson and Spearman correlation
#install.packages("ggpubr")
library("ggpubr")
cor.test(dataset$absent_days_yr, dataset$avg_wkly_hrs, method = c("pearson"))
## Pearson's product-moment correlation

#data:  dataset$absent_days_yr and dataset$avg_wkly_hrs
#t = -25.852, df = 1997, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.5328932 -0.4671645
#sample estimates: cor 
#-0.5007504

cor.test(dataset$absent_days_yr, dataset$avg_wkly_hrs, method = c("spearman"), exact=FALSE)
#Spearman's rank correlation rho

#data:  dataset$absent_days_yr and dataset$avg_wkly_hrs
#S = 1956548393, p-value < 2.2e-16
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
# rho 
#-0.469615

cor.test(dataset$absent_days_yr, dataset$avg_wkly_hrs, method = c("kendall"))
##
#Kendall's rank correlation tau

#data:  dataset$absent_days_yr and dataset$avg_wkly_hrs
#z = -21.635, p-value < 2.2e-16
#alternative hypothesis: true tau is not equal to 0
#sample estimates:
#      tau 
#-0.384046

install.packages("generalCorr")
library("generalCorr")
corr_matrix <- as.matrix(
  cbind(dataset$absent_days_yr, dataset$avg_wkly_hrs)
)
vinod_1 <- generalCorr::gmcmtx0(corr_matrix)

#there is a correlation between both with a p values of <2.2e-16, however, it is not as imagined as it is a negative cor that suggests otherwise if the avg weekly hours increase the absent days is to decrease.
## there is a weak negative linear relationship 