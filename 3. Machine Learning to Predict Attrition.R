library(data.table)
library(caTools)
library(car)
library(rpart)
library(rpart.plot)
set.seed(2004)

current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

#======================================================================================================================
# Logistic Regression for Attrition
#======================================================================================================================

# Load new cleaned CSV file
ibm <- fread("Clean_IBM.csv")

# Drop all the rows where Attrition is blank
ibm <- ibm[!(ibm$Attrition == "")]

# Factor all the Categorical Variables
ibm$Attrition <- factor(ibm$Attrition)
ibm$BusinessTravel <- factor(ibm$BusinessTravel)
ibm$EducationField <- factor(ibm$EducationField)
ibm$Gender <- factor(ibm$Gender)
ibm$JobRole <- factor(ibm$JobRole)
ibm$MaritalStatus <- factor(ibm$MaritalStatus)
ibm$OverTime <- factor(ibm$OverTime)
ibm$Education <- factor(ibm$Education, ordered = TRUE, levels = c(1,2,3,4,5))
ibm$JobLevel <- factor(ibm$JobLevel, ordered = TRUE, levels = c(1,2,3,4,5))

#-------------------------------------
# Cleaning NA/empty values in cells
#-------------------------------------

# Checking the data table
summary(ibm)

# Replacing NA values in numeric columns with column median
ibm$Age[is.na(ibm$Age)] <- round(median(ibm$Age, na.rm = TRUE))
ibm$DistanceFromHome[is.na(ibm$DistanceFromHome)] <- round(median(ibm$DistanceFromHome, na.rm = TRUE))
ibm$MonthlyRate[is.na(ibm$MonthlyRate)] <- round(median(ibm$MonthlyRate, na.rm = TRUE))
ibm$NumCompaniesWorked[is.na(ibm$NumCompaniesWorked)] <- round(median(ibm$NumCompaniesWorked, na.rm = TRUE))

# Replacing NA values in factor columns with column mode
ibm[is.na(Education), Education := ibm[, .N, Education][order(-N)][1, Education]]
ibm[is.na(JobLevel), JobLevel := ibm[, .N, JobLevel][order(-N)][1, JobLevel]]

# Replacing empty values in factor columns with column mode
ibm[BusinessTravel == "", BusinessTravel := ibm[, .N, BusinessTravel][order(-N)][1, BusinessTravel]]
ibm[EducationField == "", EducationField := ibm[, .N, EducationField][order(-N)][1, EducationField]]
ibm[Gender == "", Gender := ibm[, .N, Gender][order(-N)][1, Gender]]
ibm[JobRole == "", JobRole := ibm[, .N, JobRole][order(-N)][1, JobRole]]
ibm[MaritalStatus == "", MaritalStatus := ibm[, .N, MaritalStatus][order(-N)][1, MaritalStatus]]
ibm[OverTime == "", OverTime := ibm[, .N, OverTime][order(-N)][1, OverTime]]

#-------------------------------------
# Model: Logistic Regression
#-------------------------------------

# Remove Current Employees who have stayed for less than 4 years because we are uncertain if they will leave before 4 years is up
ibm <- ibm[!(ibm$Attrition == "Current employee" & ibm$YearsAtCompany < 4)]

# Categorise Attrition into factor variables of 0 & 1
ibm[Attrition != "", LogReg := 1] # Set all cells as 1 by default
ibm[Attrition == "Voluntary Resignation" & YearsAtCompany < 4, LogReg := 0] # Set undesirable employees as 0
ibm[Attrition == "Termination" & YearsAtCompany < 4, LogReg := 0] # Set undesirable employees as 0
ibm$LogReg <- factor(ibm$LogReg)

# Drop unnecessary columns
ibm[, c("Attrition", "TotalWorkingYears", "YearsAtCompany")] <- NULL

# Train-Test Split (70/30)
train <- sample.split(Y = ibm$LogReg, SplitRatio = 0.7)
trainset <- subset(ibm, train == T)
testset <- subset(ibm, train == F)

# Modelling
m1 <- glm(LogReg ~ . , family = binomial, data = trainset)
summary(m1) # All variables are significant

OR <- exp(coef(m1))
OR.CI <- exp(confint(m1))
OR.CI # All variables are significant as they don't contain 1

# Checking for multicollinearity
vif(m1) # JobLevel & JobRole have multicollinearity problems, remove JobRole that has a higher GVIF
m1 <- glm(LogReg ~ . -JobRole , family = binomial, data = trainset)
vif(m1) # PriorExperience still has multicollinearity problem, remove PriorExperience

m2 <- glm(LogReg ~ . -JobRole  -PriorExperience, family = binomial, data = trainset)
summary(m2)
vif(m2) # No more multicollinearity problems

# Setting the threshold for predicting Y = 1 based on probability
threshold <- sum(ibm$LogReg == 1) / length(ibm$LogReg)
threshold # 0.9097096

# Predicting trainset data
prob.train <- predict(m2, newdata = trainset, type = 'response')
predict.logreg.train <- ifelse(prob.train > threshold, 1, 0)
logreg_confusion_matrix_train <- table(trainset$LogReg, predict.logreg.train)
prop.table(logreg_confusion_matrix_train) # 75.09% accuracy

# Predicting testset data
prob.test <- predict(m2, newdata = testset, type = 'response')
predict.logreg.test <- ifelse(prob.test > threshold, 1, 0)
logreg_confusion_matrix <- table(testset$LogReg, predict.logreg.test)
prop.table(logreg_confusion_matrix) # 76.09% accuracy

#======================================================================================================================
# CART
#======================================================================================================================

# Reload new cleaned CSV file
ibm <- fread("Clean_IBM.csv")

# Drop all the rows where Attrition is blank
ibm <- ibm[!(ibm$Attrition == "")]

# Factor all the Categorical Variables
ibm$Attrition <- factor(ibm$Attrition)
ibm$BusinessTravel <- factor(ibm$BusinessTravel)
ibm$EducationField <- factor(ibm$EducationField)
ibm$Gender <- factor(ibm$Gender)
ibm$JobRole <- factor(ibm$JobRole)
ibm$MaritalStatus <- factor(ibm$MaritalStatus)
ibm$OverTime <- factor(ibm$OverTime)
ibm$Education <- factor(ibm$Education, ordered = TRUE, levels = c(1,2,3,4,5))
ibm$JobLevel <- factor(ibm$JobLevel, ordered = TRUE, levels = c(1,2,3,4,5))

# Remove Current Employees who have stayed for less than 4 years because we are uncertain if they will leave before 4 years is up
ibm <- ibm[!(ibm$Attrition == "Current employee" & ibm$YearsAtCompany < 4)]

# Categorise Attrition into factor variables of 0 & 1
ibm[Attrition != "", decision := 1] # Set all cells as 1 by default
ibm[Attrition == "Voluntary Resignation" & YearsAtCompany < 4, decision := 0] # Set undesirable employees as 0
ibm[Attrition == "Termination" & YearsAtCompany < 4, decision := 0] # Set undesirable employees as 0
ibm$decision <- factor(ibm$decision)

# Drop unnecessary columns
ibm[, c("Attrition", "TotalWorkingYears", "YearsAtCompany")] <- NULL

#-------------------------------------
# CART 1: For Machine Use
#-------------------------------------

# Creating the Maximum Tree
cart.max <- rpart(decision ~ . , data = ibm, method = "class", control = rpart.control(minsplit = 2, cp = 0))
rpart.plot(cart.max, nn = T, main = "Maximal Tree in cart.max") # Visualising the Maximum Tree
print(cart.max)
printcp(cart.max, digits = 3)
plotcp(cart.max) # Visualising the Optimal CP

# Finding the Optimal CP for Pruning
CVerror.cap <- cart.max$cptable[which.min(cart.max$cptable[, "xerror"]), "xerror"] + cart.max$cptable[which.min(cart.max$cptable[, "xstd"]), "xstd"]
i <- 1; j <- 4
while (cart.max$cptable[i,j] > CVerror.cap)
{i <- i + 1}
cp.opt <- ifelse(i > 1, sqrt(cart.max$cptable[i, 1] * cart.max$cptable[i-1, 1]), 1)
cp.opt # 0.0004890088

# Prune the Maximum Tree using the Optimal CP
cart.opt <- prune(cart.max, cp = cp.opt)

# Plotting the Optimal Tree
rpart.plot(cart.opt, nn = T, main = "Optimal Tree in cart.opt")

# Checking the importance of each variable in the Optimal Tree
cart.opt$variable.importance

# Predicting Decision outcome
predicted <- predict(cart.opt, newdata = ibm, type = "class")
table <- table(ibm$decision, predicted)
prop.table(table)
mean(ibm$decision == predicted) # 99.94% accuracy

#-------------------------------------
# CART 2: For Human Understanding
#-------------------------------------

# Creating the Maximum Tree
cart.max <- rpart(decision ~ . , data = ibm, method = "class", control = rpart.control(minsplit = 150, cp = 0))
rpart.plot(cart.max, nn = T, main = "Maximal Tree in cart.max") # Visualising the Maximum Tree
printcp(cart.max, digits = 3)
plotcp(cart.max) # Visualising the Optimal CP

# Finding the Optimal CP for Pruning
CVerror.cap <- cart.max$cptable[which.min(cart.max$cptable[, "xerror"]), "xerror"] + cart.max$cptable[which.min(cart.max$cptable[, "xstd"]), "xstd"]
i <- 1; j <- 4
while (cart.max$cptable[i,j] > CVerror.cap)
{i <- i + 1}
cp.opt <- ifelse(i > 1, sqrt(cart.max$cptable[i, 1] * cart.max$cptable[i-1, 1]), 1)
cp.opt # 0.00466997

# Prune the Maximum Tree using the Optimal CP
cart.opt <- prune(cart.max, cp = cp.opt)

# Plotting the Optimal Tree
rpart.plot(cart.opt, nn = T, main = "Optimal Tree in cart.opt")

# Checking the importance of each variable in the Optimal Tree
cart.opt$variable.importance

# Predicting Decision outcome
predicted <- predict(cart.opt, newdata = ibm, type = "class")
table <- table(ibm$decision, predicted)
prop.table(table)
mean(ibm$decision == predicted) # 93.24% accuracy

#======================================================== END =========================================================