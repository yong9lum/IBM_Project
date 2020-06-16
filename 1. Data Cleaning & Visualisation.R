library(ggplot2)
library(RColorBrewer)

current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

#======================================================================================================================
# Initial Data Cleaning
#======================================================================================================================

# Load data into R & check the data
ibm <- fread("IBM HR Data.csv")
summary(ibm)
# Some numerical anomalies in EnvironmentalSatisfaction, JobInvolvement, NumCompaniesWorked, PerformanceRating, StandardHours, StockOptionLevel, TrainingTimesLastYear

# To find out which row contains the error
which(ibm$EnvironmentSatisfaction == 129588) # Row 6147
ibm[EnvironmentSatisfaction == 129588] # EmployeeNumber is empty, all rows from column 10 have been shifted 1 column to the right

# To fix the problem, shift row 6147 by 1 column to the left & leave the last column 'Employee Source' as NA
shiftleft <- c(6147)
ibm[shiftleft, 10:36] <- ibm[shiftleft, 11:37]
ibm[6147, `Employee Source` := NA]

# Check data again
summary(ibm)
# We still realised some numerical anomalies in EnvironmentalSatisfaction, JobInvolvement, NumCompaniesWorked, PerformanceRating, StandardHours, StockOptionLevel, TrainingTimesLastYear

which(ibm$EnvironmentSatisfaction == 127249) # Row 3808
ibm[EnvironmentSatisfaction == 127249] # DailyRate is NA, all rows from column 4 have been shifted 1 column to the right

# Shift row 3808 by 1 column to the left & leave the last column 'Employee Source' as NA
shiftleft2 <- c(3808)
ibm[shiftleft2, 4:36] <- ibm[shiftleft2, 5:37]
ibm[3808, `Employee Source` := NA]

# Check data again
summary(ibm) # No more visible anomalies identified

# Drop duplicate rows which will be identified by having identical values under "Application ID"
ibm <- ibm[!duplicated(ibm$`Application ID`)]

# Change data types for numerical columns from 'character' into 'integer'
ibm$DistanceFromHome <- as.integer(ibm$DistanceFromHome)
ibm$HourlyRate <- as.integer(ibm$HourlyRate)
ibm$MonthlyIncome <- as.integer(ibm$MonthlyIncome)
ibm$PercentSalaryHike <- as.integer(ibm$PercentSalaryHike)

# Factor & re-order all the numbered Categorical variables
ibm$Education <- factor(ibm$Education, ordered = TRUE, levels = c(1,2,3,4,5))
ibm$EnvironmentSatisfaction <- factor(ibm$EnvironmentSatisfaction, ordered = TRUE, levels = c(1,2,3,4))
ibm$JobInvolvement <- factor(ibm$JobInvolvement, ordered = TRUE, levels = c(1,2,3,4))
ibm$JobSatisfaction <- factor(ibm$JobSatisfaction, ordered = TRUE, levels = c(1,2,3,4))
ibm$JobLevel <- factor(ibm$JobLevel, ordered = TRUE, levels = c(1,2,3,4,5))
ibm$WorkLifeBalance <- factor(ibm$WorkLifeBalance, ordered = TRUE, levels = c(1,2,3,4))
ibm$RelationshipSatisfaction <- factor(ibm$RelationshipSatisfaction, ordered = TRUE, levels = c(1,2,3,4))
ibm$PerformanceRating <- factor(ibm$PerformanceRating, ordered = TRUE, levels = c(1,2,3,4))

# Factor all the Categorical Variables
ibm$Attrition <- factor(ibm$Attrition)
ibm$BusinessTravel <- factor(ibm$BusinessTravel, ordered = TRUE, levels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"))
ibm$Department <- factor(ibm$Department)
ibm$EducationField <- factor(ibm$EducationField)
ibm$Gender <- factor(ibm$Gender)
ibm$JobRole <- factor(ibm$JobRole)
ibm$MaritalStatus <- factor(ibm$MaritalStatus)
ibm$OverTime <- factor(ibm$OverTime)
ibm$`Employee Source` <- factor(ibm$`Employee Source`)
ibm$Education <- factor(ibm$Education, ordered = TRUE, levels = c(1,2,3,4,5))
ibm$EnvironmentSatisfaction <- factor(ibm$EnvironmentSatisfaction, ordered = TRUE, levels = c(1,2,3,4))
ibm$JobInvolvement <- factor(ibm$JobInvolvement, ordered = TRUE, levels = c(1,2,3,4))
ibm$JobSatisfaction <- factor(ibm$JobSatisfaction, ordered = TRUE, levels = c(1,2,3,4))
ibm$JobLevel <- factor(ibm$JobLevel, ordered = TRUE, levels = c(1,2,3,4,5))
ibm$WorkLifeBalance <- factor(ibm$WorkLifeBalance, ordered = TRUE, levels = c(1,2,3,4))
ibm$RelationshipSatisfaction <- factor(ibm$RelationshipSatisfaction, ordered = TRUE, levels = c(1,2,3,4))
ibm$PerformanceRating <- factor(ibm$PerformanceRating, ordered = TRUE, levels = c(1,2,3,4))
ibm$JobSatisfaction <- factor(ibm$JobSatisfaction, ordered = TRUE, levels = c(1,2,3,4))

#======================================================================================================================
# Data Visualisation
#======================================================================================================================

# 1. Exploring Performance Rating & Job Satisfaction
# EmployeeSource against PerformanceRating
count1 <- table(ibm$PerformanceRating, ibm$`Employee Source`)
barplot(count1, col = brewer.pal(n = 4, name = "Blues"), cex.names = 0.7, xlab = "Employee Source")
legend("topright", title = "Performance Rating", inset = c(0, 0), fill = brewer.pal(n = 4, name = "Blues"), legend = rownames(count1), border = "grey", cex = 0.8)
prop1 <- prop.table(count1, margin = 2)
barplot(prop1, col = brewer.pal(n = 4, name = "Blues"), cex.names = 0.7, xlab = "Employee Source", ylab = "Proportion of Performance Rating")
legend("bottomright", title = "Performance Rating", inset=c(0,0), fill = brewer.pal(n = 4, name = "Blues"), legend = rownames(prop1), border = "grey", cex = 0.7)
# Not much insight from this visualisation

# PerformanceRating against TotalWorkingYears
ggplot(data = ibm) + aes(x = TotalWorkingYears, y = as.numeric(PerformanceRating)) + geom_jitter() + xlab("Total Working Years") + ylab("Performance Rating")
# Most of the data's Performance Rating score is 3, the graph shows that having more experience (in working years) does not necessarily lead to a higher performance rating

# Job Satisfaction against TotalWorkingYEars
ggplot(data = ibm) + aes(x = TotalWorkingYears, y = JobSatisfaction) + geom_jitter()
# Job satisfaction levels not affected by total working years, employees are generally more satisfied

# 2. Exploring Attrition
# Attrition against YearsAtCompany
getOption("scipen")
options(scipen = 100000000)
summary(ibm$YearsAtCompany)
percentile <- ecdf(ibm$YearsAtCompany)
percentile(20) #0.9552
YearsAtCompany.cat <- cut(ibm$YearsAtCompany, breaks = c(0, 5, 10, 20, 40), include.lowest = T, dig.lab = 50)
count4 <- table(ibm$Attrition, YearsAtCompany.cat)
prop4 <- prop.table(count4, margin = 2)
par(las = 0)
barplot(prop4, col = brewer.pal(n = 4, name = "Blues"), xlab = "Years At Company")
legend("center", title = "Attrition",inset = c(0, 0), fill = brewer.pal(n = 4, name = "Blues"), legend = rownames(count4), border = "grey", cex = 0.7)
# Attrition rate in IBM is more significant among the newer employees

# Attrition against Age
getOption("scipen")
options(scipen = 100000000)
summary(ibm$Age)
Age.cat <- cut(ibm$Age, breaks = c(18, 25, 35, 45, 55, 60), include.lowest = T, dig.lab = 50)
count5 <- table(ibm$Attrition, Age.cat)
prop5 <- prop.table(count5, margin = 2)
par(las = 0)
barplot(prop5, col = brewer.pal(n = 4, name = "Blues"), xlab = "Age")
legend("center", title = "Attrition", inset = c(0, 0), fill = brewer.pal(n = 4, name = "Blues"), legend = rownames(count4), border = "grey", cex = 0.7)
# Almost 40% of the younger employees resigned voluntarily

# Attrition against MonthlyRate
getOption("scipen")
options(scipen = 100000000)
summary(ibm$MonthlyRate)
MonthlyRate.cat <- cut(ibm$MonthlyRate, breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000), include.lowest = T, dig.lab = 50)
count6 <- table(ibm$Attrition, MonthlyRate.cat)
barplot(count6, col = brewer.pal(n = 4, name = "Blues"), xlab = "Monthly Rate")
legend("center", title = "Attrition", inset = c(0, 0), fill = brewer.pal(n = 4, name = "Blues"), legend = rownames(count6), border = "grey", cex = 0.7)
# The fewest voluntary resignations come from those with the highest monthly rate bracket

# Attrition against DistanceFromHome
summary(ibm$DistanceFromHome)
DistanceFromHome.cat <- cut(ibm$DistanceFromHome, breaks = c(0, 10, 20, 30), include.lowest = T, dig.lab = 50)
count7 <- table(ibm$Attrition, DistanceFromHome.cat)
prop7 <- prop.table(count7, margin = 2)
barplot(prop7, col=brewer.pal(n = 4, name = "Blues"), xlab = "Distance From Home")
legend("center", title = "Attrition", inset = c(0, 0), fill = brewer.pal(n = 4, name = "Blues"), legend = rownames(count7),border = "grey", cex = 0.7)
# The proportion of employees who resigned voluntarily increases as their distance from home increases

# Attrition against BusinessTravel
count8 <- table(ibm$Attrition, ibm$BusinessTravel)
prop8 <- prop.table(count8, margin = 2)
barplot(prop8, col = brewer.pal(n = 4, name = "Blues"), xlab = "Business Travel", cex.names = 0.8)
legend("bottomleft", title = "Attrition",inset = c(0.05, 0), fill = brewer.pal(n = 4, name = "Blues"), legend = rownames(count6), border = "grey", cex = 0.7)
# The more an employee travels for business, the more likely they are to voluntarily resign

# Attrition against JobLevel
count9 <- table(ibm$Attrition, ibm$JobLevel)
barplot(count9, col = brewer.pal(n = 4, name="Blues"), xlab = "Job Level")
legend("topright", title = "Attrition", inset = c(0, 0), fill = brewer.pal(n = 4, name = "Blues"), legend = rownames(count9), border = "grey", cex = 0.7)
# The majority of the employees in IBM are in the lower tier of job level, which is where attrition is more widely present

#======================================================================================================================
# Final Data Cleaning
#======================================================================================================================

# Drop redundant columns
ibm[, c("DailyRate", "Department", "EmployeeCount", "EmployeeNumber", "EnvironmentSatisfaction", "HourlyRate", "JobInvolvement", "JobSatisfaction", "MonthlyIncome", "Over18", "PercentSalaryHike", "PerformanceRating", "RelationshipSatisfaction", "Application ID", "StandardHours", "StockOptionLevel", "TrainingTimesLastYear", "WorkLifeBalance", "YearsInCurrentRole", "YearsSinceLastPromotion", "YearsWithCurrManager", "Employee Source")] <- NULL

# We next check the columns where the values are "character" to sieve out any string anomalies
table(ibm$Attrition) # No errors
table(ibm$BusinessTravel) # No errors
table(ibm$DistanceFromHome) # Numbers are string rather than integers
table(ibm$EducationField) # 1 count of 'Test' needs to be removed
table(ibm$Gender) # No errors
table(ibm$JobRole) # No errors
table(ibm$MaritalStatus) # No errors
table(ibm$OverTime) # No errors

# Change 'character' into 'integer'
ibm$DistanceFromHome <- as.integer(ibm$DistanceFromHome)

# Remove 'Test' under Education Field & Employee Source columns
ibm[EducationField == "Test", EducationField := NA]

# Age & Total Working Years discrepancies
which(ibm$Age - ibm$TotalWorkingYears < 14) # 1700 entries where employee has been working even before U.S. legal working age of 14 years
ibm <- ibm[!(ibm$Age - ibm$TotalWorkingYears < 14)] # Removing the 1700 entries

# Creating new column for YearsAtCompany prior to IBM
ibm[, PriorExperience := TotalWorkingYears - YearsAtCompany]
ibm[PriorExperience < 0, PriorExperience := NA] # 6 entries with years at company > total working years, replace with NA

# Factor & re-order all the numbered Categorical variables
ibm$Education <- factor(ibm$Education, ordered = TRUE, levels = c(1,2,3,4,5))
ibm$JobLevel <- factor(ibm$JobLevel, ordered = TRUE, levels = c(1,2,3,4,5))

# Check data type of all variables
str(ibm) # All correct

# Export cleaned dataset into a new CSV file
fwrite(ibm, "Clean_IBM.csv")

#======================================================== END =========================================================