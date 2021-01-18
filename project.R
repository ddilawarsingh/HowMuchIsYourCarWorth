#Packages to be loaded
library(MASS)
library(leaps)

#Import data from csv file
Cars_Data <- read.csv(file='Project_data_Cars.csv')


#Activity 1
#--1
plot(Cars_Data$Mileage,Cars_Data$Price, xlab = "Mileage", ylab = "Price", main="Scatter Plot")

#--2
Corr_Mileage_Price = cor(Cars_Data$Mileage,Cars_Data$Price)
linear_model_Mileage <- lm(Price ~ Mileage, data = Cars_Data)
summary(linear_model_Mileage)

#--3
linear_model_Mileage$residuals[1]

#Activity 2
#--5
#----a
linear_model_Cyl <- lm(Price ~ Cyl, data = Cars_Data)
linear_model_Liter <- lm(Price ~ Liter, data = Cars_Data)
linear_model_Doors <- lm(Price ~ Doors, data = Cars_Data)
linear_model_Cruise <- lm(Price ~ Cruise, data = Cars_Data)
linear_model_Sound <- lm(Price ~ Sound, data = Cars_Data)
linear_model_Leather <- lm(Price ~ Leather, data = Cars_Data)

R_Squares_X1 <- list()
R_Squares_X1["Mileage"] <- summary(linear_model_Mileage)$r.squared
R_Squares_X1["Cyl"] <- summary(linear_model_Cyl)$r.squared
R_Squares_X1["Liter"] <- summary(linear_model_Liter)$r.squared
R_Squares_X1["Doors"] <- summary(linear_model_Doors)$r.squared
R_Squares_X1["Cruise"] <- summary(linear_model_Cruise)$r.squared
R_Squares_X1["Sound"] <- summary(linear_model_Sound)$r.squared
R_Squares_X1["Leather"] <- summary(linear_model_Leather)$r.squared

which.max(R_Squares_X1)

#----b
linear_model_Cyl_Mileage <- lm(Price ~ Cyl + Mileage, data = Cars_Data)
linear_model_Cyl_Liter <- lm(Price ~ Cyl + Liter, data = Cars_Data)
linear_model_Cyl_Doors <- lm(Price ~ Cyl + Doors, data = Cars_Data)
linear_model_Cyl_Cruise <- lm(Price ~ Cyl + Cruise, data = Cars_Data)
linear_model_Cyl_Sound <- lm(Price ~ Cyl + Sound, data = Cars_Data)
linear_model_Cyl_Leather <- lm(Price ~ Cyl + Leather, data = Cars_Data)

R_Squares_X1_X2 <- list()
R_Squares_X1_X2["Mileage"] <- summary(linear_model_Cyl_Mileage)$r.squared
R_Squares_X1_X2["Liter"] <- summary(linear_model_Cyl_Liter)$r.squared
R_Squares_X1_X2["Doors"] <- summary(linear_model_Cyl_Doors)$r.squared
R_Squares_X1_X2["Cruise"] <- summary(linear_model_Cyl_Cruise)$r.squared
R_Squares_X1_X2["Sound"] <- summary(linear_model_Cyl_Sound)$r.squared
R_Squares_X1_X2["Leather"] <- summary(linear_model_Cyl_Leather)$r.squared

which.max(R_Squares_X1_X2)

#----c
linear_model_StepWise_intercept = lm(Price~1, data=Cars_Data)
linear_model_StepWise_total = lm(Price~Cyl+Mileage+Liter+Doors+Cruise+Sound+Leather,data=Cars_Data)
linear_model_StepWise_final = stepAIC(linear_model_StepWise_intercept, direction = "forward", scope=formula(linear_model_StepWise_total))
linear_model_StepWise_final$anova



#--6

#Modified Car data so that it can be used by regsubsets

Mod_Cars_Data = Cars_Data
Mod_Cars_Data$Make = factor(Mod_Cars_Data$Make, labels=1:6)
Mod_Cars_Data$Make = as.numeric(Mod_Cars_Data$Make)
Mod_Cars_Data$Model = factor(Mod_Cars_Data$Model, labels=1:32)
Mod_Cars_Data$Model = as.numeric(Mod_Cars_Data$Model)
Mod_Cars_Data$Trim = factor(Mod_Cars_Data$Trim, labels=1:47)
Mod_Cars_Data$Trim = as.numeric(Mod_Cars_Data$Trim)
Mod_Cars_Data$Type = factor(Mod_Cars_Data$Type, labels=1:5)
Mod_Cars_Data$Type = as.numeric(Mod_Cars_Data$Type)

linear_model_BestSub = regsubsets(Price~., data = Mod_Cars_Data, nvmax = 11)
linear_model_BestSub_summ = summary(linear_model_BestSub)
which.max(linear_model_BestSub_summ$adjr2)
which.min(linear_model_BestSub_summ$cp)
par(mfrow=c(1,2))
plot(linear_model_BestSub, scale = "adjr2", main="Adjusted R-Squared")
plot(linear_model_BestSub, scale = "Cp", main="Mallow Cp")


#Activity 3
#--8
par(mfrow=c(1,1))
plot(Cars_Data$Cyl,linear_model_StepWise_final$residuals, xlab = "Cyl", ylab="Residuals", main="Residuals Vs Cyl")
plot(Cars_Data$Cruise,linear_model_StepWise_final$residuals, xlab = "Cruise", ylab="Residuals", main="Residuals Vs Cruise")
plot(Cars_Data$Leather,linear_model_StepWise_final$residuals, xlab = "Leather", ylab="Residuals", main="Residuals Vs Leather")
plot(Cars_Data$Mileage,linear_model_StepWise_final$residuals, xlab = "Mileage", ylab="Residuals", main="Residuals Vs Mileage")
plot(Cars_Data$Doors,linear_model_StepWise_final$residuals, xlab = "Doors", ylab="Residuals", main="Residuals Vs Doors")
plot(Cars_Data$Sound,linear_model_StepWise_final$residuals, xlab = "Sound", ylab="Residuals", main="Residuals Vs Sound")
plot(linear_model_StepWise_final$fitted.values, linear_model_StepWise_final$residuals, xlab = "Predicted Price", ylab="Residuals", main="Residuals Vs Predicted Price")
plot(linear_model_StepWise_final,1)

#----c
plot(density(linear_model_StepWise_final$residuals), main="Density Plot: Residuals", xlab="Residuals", ylab="Frequency")
polygon(density(linear_model_StepWise_final$residuals), col="blue")
abline(v = 0, lty = 2)

plot(Cars_Data$Mileage,linear_model_StepWise_final$residuals, xlab = "Mileage", ylab="Residuals", main="Residuals Vs Mileage")
abline(a = 0, b=0)

#----d
plot(c(1:804),linear_model_StepWise_final$residuals, xlab="observation order", ylab="Residuals", main="Residual Ordered Plot")
abline(a=0,b=0)

#--9
par(mfrow=c(1,2))

linear_Model_Log = lm(log(Price) ~ Cyl + Cruise + Leather + Mileage + Doors + Sound, data=Cars_Data)
summary(linear_Model_Log)
plot(linear_Model_Log,1, main = "Residuals vs Fitted(Log(Price))")

plot(density(linear_Model_Log$residuals), main="Density Plot Log: Residuals", xlab="Residuals", ylab="Frequency")
polygon(density(linear_Model_Log$residuals), col="blue")
abline(v = 0, lty = 2)

linear_Model_Sqrt = lm(sqrt(Price) ~ Cyl + Cruise + Leather + Mileage + Doors + Sound, data=Cars_Data)
summary(linear_Model_Sqrt)
plot(linear_Model_Sqrt,1, main = "Residuals vs Fitted(Sqrt(Price))")

plot(density(linear_Model_Sqrt$residuals), main="Density Plot Sqrt: Residuals", xlab="Residuals", ylab="Frequency")
polygon(density(linear_Model_Sqrt$residuals), col="blue")
abline(v = 0, lty = 2)


#Activity 4
#--10
par(mfrow=c(1,1))
plot(linear_model_StepWise_final,2)

#--11

cooksd = cooks.distance(linear_model_StepWise_final)
plot(linear_model_StepWise_final, which = 4)
Mod_Cars_Data2 = Cars_Data[-c(151:160),]

lm_without_outlier = lm(Price~Cyl + Cruise + Leather + Mileage + Doors + Sound, data=Mod_Cars_Data2)
summary(lm_without_outlier)

#FINAL MODEL

fm_intercept = lm(log(Price)~1, data=Cars_Data)
fm_total = lm(log(Price)~., data=Cars_Data)
fm_final = stepAIC(fm_intercept, direction = "forward", scope = formula(fm_total))
summary(fm_final)

