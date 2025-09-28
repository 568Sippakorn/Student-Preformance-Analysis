# import Data set
library(readxl)
StudentCleaned <- 
read_excel("C:/Users/Windows 10/OneDrive/Desktop/Coding/Python/Regression Project/StudentCleaned.xlsx")
View(StudentCleaned)

# Setting Variables
Y = StudentCleaned$'Performance Index'
X1 = StudentCleaned$'Hours Studied'
X2 = StudentCleaned$'Previous Scores'
X3 = StudentCleaned$'Sample Question Papers Practiced'
X4 = StudentCleaned$'Extracurricular Activities_Encoded'

# Correration test
cor.test(X1,Y)
cor.test(X2,Y)
cor.test(X3,Y)
cor.test(X4,Y)

# Make Model
model = lm(Y~X1+X2+X3+X4)
summary(model)

# Assumption Check
nortest::ad.test(Y)
lmtest::dwtest(model)
lmtest::bgtest(model)

#Fix Independent
hist(Y); qqnorm(Y); qqline(Y)
library(MASS); bc = boxcox(lm(Y ~ 1))
lambda = bc$x[which.max(bc$y)]
Y_bc = if(lambda==0) log(Y) else (Y^lambda-1)/lambda

# Independent Check
model = lm(Y_bc ~ X1 + X2 + X3 + X4)
summary(model)
qqnorm(residuals(model)); qqline(residuals(model))
nortest::ad.test(residuals(model))

#Multiple Assumptions
library(Car)
vif(model)

#Powerful Value
influence.measures(model)

# Appropriate model
library(readxl)
library(leaps)
dat <- read_excel("StudentCleaned.xlsx")
dat <- read_excel("C:/Users/Windows 10/OneDrive/Desktop/Coding/Python/Regression Project/StudentCleaned.xlsx")
fit <- regsubsets('Performance Index' ~ 'Hours Studied' + 'Previous Scores' + 'Sample Question Papers Practiced' + 'Extracurricular Activities_Encoded', data = dat, nvmax = 4)
cbind(sum.all$rss, sum.all$cp, sum.all$outmat)
model.both <- step(model,direction = "both")
summary(model.both)