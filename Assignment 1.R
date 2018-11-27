# Assignment 1 #

# Set up #

setwd("C:/Users/Rob/Documents/Work/Deakin/Deakin - T2/Predictive Analytics/Assignment 1/R")

library("psych", lib.loc="~/R/win-library/3.4")
library("car", lib.loc="~/R/win-library/3.4")

csv <- read.csv("american_time_use_sample.csv")

# Extract some variables #

weeklyearnings <- csv$Weekly.Earnings
weeklyhrsworked <- csv$Weekly.Hours.Worked
age <- csv$Age
sleeping <- csv$Sleeping
socialising <-csv$Socializing...Relaxing
foodprep <- csv$Food...Drink.Prep
volunteering <- csv$Volunteering


# Xtra Variables #

employ <- csv$Employment.Status
maleyes <- as.numeric(csv$Gender == "Male")

education <- ifelse(csv$Education.Level == '9th Grade',1,
                    ifelse(csv$Education.Level == '10th Grade',2,
                           ifelse(csv$Education.Level == '11th Grade',3,
                                  ifelse(csv$Education.Level == '12th Grade',4,
                                         ifelse(csv$Education.Level == 'High School',5,
                                                ifelse(csv$Education.Level == 'Some College',6,
                                                       ifelse(csv$Education.Level == 'Associate Degree',7,
                                                              ifelse(csv$Education.Level == 'Bachelor',8,
                                                                     ifelse(csv$Education.Level == 'Master',9,
                                                                            ifelse(csv$Education.Level == 'Doctoral Degree',10,
                                                                                   ifelse(csv$Education.Level == 'Doctoral Degree',11,0)))))))))))


# -------------------------------------------------- Form Data Frame ------------------------------ #

data <- data.frame(weeklyearnings,
                   weeklyhrsworked,
                   employ,
                   education,
                   age,
                   sleeping,
                   socialising,
                   foodprep,
                   volunteering,
                   maleyes)

# Subset of Weekly Hrs > 0 #

nozero <- subset(data,data[,2] >0)

# Subset of Weekly Earnings > 0 #

nozero <- subset(nozero,nozero[,1] >0)

# Subset of Employed #

nozero <- subset(nozero,nozero[,3] == 'Employed')

# Subset of Education ?

# Create Payrate column #
nozero$payrate <- with(nozero, weeklyearnings / weeklyhrsworked)

# ------------------------------------------------------- Density plots --------------------------------------- #

par(mfrow=c(3,3))
plot(density(nozero$weeklyearnings), main="Weekly Earnings Density Plot")
plot(density(nozero$weeklyhrsworked), main="Weekly Hrs Worked Density Plot")
plot(density(nozero$payrate), main="Payrate Density Plot")
plot(density(nozero$age), main="Age Density Plot")
plot(density(nozero$education), main="Education Density Plot")
plot(density(nozero$sleeping), main="Sleeping Density Plot")
plot(density(nozero$foodprep), main="Food Prep Density Plot")
plot(density(nozero$socialising), main="Socialising Density Plot")
plot(density(nozero$volunteering), main="Volunteering Density Plot")

# ---------------------------------------------------------- Transformations ----------------------------------#

sqrtweeklyearnings <-sqrt(nozero$weeklyearnings)
sqrtsocial <- sqrt(nozero$socialising)
sqrtfoodprep <-sqrt(nozero$foodprep)
sqrtvolunteering <- sqrt(nozero$volunteering)

# Re-Define Trans Data-set #

weeklyearnings2 <- sqrtweeklyearnings
weeklyhrsworked2 <- nozero$weeklyhrsworked
age2 <- nozero$age
education2 <- nozero$education
sleeping2 <- nozero$sleeping
foodprep2 <- sqrtfoodprep
socialising2 <- sqrtsocial
volunteering2 <- sqrtvolunteering
maleyes2 <- nozero$maleyes
  
#  -------------------------------------------------- New Transformed Data Frame  ----------------------------- #

trans <- data.frame(weeklyearnings2,
                    weeklyhrsworked2,
                    age2,
                    education2,
                    sleeping2,
                    socialising2,
                    foodprep2,
                    volunteering2,
                    maleyes2)

# --------------------------------------------------- New Density Plots --------------------------------------- #

par(mfrow=c(3,3))
plot(density(trans$weeklyearnings2), main="Weekly Earnings Density Plot 2")
plot(density(trans$weeklyhrsworked2), main="Weekly Hrs Worked Density Plot 2")
plot(density(trans$age2), main="Age Density Plot 2")
plot(density(trans$education2), main="Education Density Plot 2")
plot(density(trans$sleeping2), main="Sleeping Density Plot 2")
plot(density(trans$foodprep2), main="Food Prep Density Plot 2")
plot(density(trans$socialising2), main="Socialising Density Plot 2")
plot(density(trans$volunteering2), main="Volunteering Density Plot 2")

par(mfrow=c(2,4))
plot(density(trans$weeklyearnings2), main="Weekly Earnings Density Plot 2")
plot(density(trans$weeklyhrsworked2), main="Weekly Hrs Worked Density Plot 2")
plot(density(trans$age2), main="Age Density Plot 2")
plot(density(trans$education2), main="Education Density Plot 2")
plot(density(trans$sleeping2), main="Sleeping Density Plot 2")
plot(density(trans$foodprep2), main="Food Prep Density Plot 2")
plot(density(trans$socialising2), main="Socialising Density Plot 2")



# Scatter Plots
par(mfrow=c(2,4))
plot((trans$weeklyearnings2), main="Weekly Earnings Scatter Plot 2")
plot((trans$weeklyhrsworked2), main="Weekly Hrs Worked Scatter Plot 2")
plot((trans$age2), main="Age Scatter Plot 2")
plot((trans$education2), main="Education Scatter Plot 2")
plot((trans$sleeping2), main="Sleeping Scatter Plot 2")
plot((trans$foodprep2), main="Food Prep Scatter Plot 2")
plot((trans$socialising2), main="Socialising Scatter Plot 2")
plot((trans$volunteering2), main="Volunteering Scatter Plot 2")


# -------------------------------------------------- Correlation ---------------------------------------------- #

cor.plot(trans,numbers = TRUE)

# New select table --+-- Drop Volunteering + Socialising + Food Prep #

select <- trans
select$volunteering2 <- NULL
select$foodprep2 <- NULL
select$socialising2 <- NULL

# ------------------------------------------------- Train Sample + Fit -------------------------------------- #

indextrain <- sample.int(nrow(trans),round(nrow(trans) * 0.8))

trainingsample <- trans[indextrain,]
validationsample <- trans[-indextrain,]

fit1 <- lm(weeklyearnings2 ~., data = trainingsample) # All # F Score = 305.3
summary(fit1)

fit2 <- lm(weeklyearnings2 ~ weeklyhrsworked2+age2+education2+maleyes2, data = trainingsample) # Select Variables # F Score = 643
summary(fit2)

fit3 <- lm(weeklyearnings2 ~ weeklyhrsworked2+education2+maleyes2, data = trainingsample) # Dropped Age # F Score = 804.5
summary(fit3)

fit4 <- lm(weeklyearnings2 ~ weeklyhrsworked2+education2, data = trainingsample) # Dropped Male # F Score = 1094
summary(fit4)

plot(fit4)

# ---------- Predicting --------- #
trainingsample$pred.weekearnings <- predict(fit4, newdata = subset(trainingsample, select = c(weeklyhrsworked2,education2)))

train.corr <- round(cor(trainingsample$pred.weekearnings, trainingsample$weeklyearnings2), 2) # 0.57 #
train.RMSE <- round(sqrt(mean((trainingsample$pred.weekearnings^2 - trainingsample$weeklyearnings2^2)^2))) # 543 #
train.MAE <- round(mean(abs(trainingsample$pred.weekearnings^2 - trainingsample$weeklyearnings2^2))) # 368 #
c(train.corr^2, train.RMSE, train.MAE)

# Validation #
validationsample$pred.weekearnings <- predict(fit4, newdata = subset(validationsample, select = c(weeklyhrsworked2,education2)))

valid.corr <- round(cor(validationsample$pred.weekearnings, validationsample$weeklyearnings2), 2) # 0.57 #
valid.RMSE <- round(sqrt(mean((validationsample$pred.weekearnings^2 - validationsample$weeklyearnings2^2)^2))) # 543 #
valid.MAE <- round(mean(abs(validationsample$pred.weekearnings^2 - validationsample$weeklyearnings2^2))) # 368 #
c(valid.corr^2, valid.RMSE, valid.MAE)

# VIF #
vif(fit4)



