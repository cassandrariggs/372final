library(readr)
library(tidyverse)
library(dplyr)
healthdata <- read_csv("~/Desktop/372final/healthdata.csv")
View(healthdata)

#I am running a linear regression for each of the variables I am testing.
# First, I am plotting the original graph. 
plot(healthdata$ppprop, healthdata$hiv, 
     main="Linear Regression of the Planned Parenthood 
  Presence and HIV Rates",
     xlab = "Proportion of Planned Parenthoods per 1 Million Residents",
     ylab = "Rate of HIV per 100,000 Residents")

#Now, I am creating a linear regression model to determine how the presence of 
#the prop. of planned parenthood is related to HIV rates. 
modelhiv <- lm(hiv ~ ppprop, data = healthdata)

#I am pulling up the summary for the model
summary(modelhiv)

#Lastly, I am adding a line of best fit to my orginal graph
abline(lm(hiv ~ ppprop, data = healthdata))

#I did the same exact thing 6 morre times for 6 different variables. 
plot(healthdata$ppprop, healthdata$std,
    main="Linear Regression of the Planned Parenthood
  Presence and STD Rates",
     xlab = "Proportion of Planned Parenthoods per 1 Million Residents",
     ylab = "Rate of STDs per 100,000 Residents")
modelstd <- lm(ppprop ~ std, data = healthdata)
summary(modelstd)
abline(lm(std ~ ppprop, data = healthdata))

plot(healthdata$ppprop, healthdata$sexed,
     main="Linear Regression of the Planned Parenthood Presence and 
     Whether the State Requires Sexual Education in Schools",
     xlab = "Proportion of Planned Parenthoods per 1 Million Residents",
     ylab = "Does the State Require Sexual Education in Schools")
modelsexed <- lm(ppprop ~ sexed, data = healthdata)
summary(modelsexed)
abline(lm(sexed ~ ppprop, data = healthdata))

plot(healthdata$ppprop, healthdata$hived,
     main="Linear Regression of the Planned Parenthood Presence and 
     Whether the State Requires HIV Education in Schools",
     xlab = "Proportion of Planned Parenthoods per 1 Million Residents",
     ylab = "Does the State Require HIV Education in Schools")
modelhived <- lm(ppprop ~ hived, data = healthdata)
summary(modelhived)
abline(lm(hived ~ ppprop, data = healthdata))

plot(healthdata$ppprop, healthdata$teenbirth,
     main="Linear Regression of the Planned Parenthood
    Presence and Teen Birth Rates",
     xlab = "Proportion of Planned Parenthoods per 1 Million Residents",
     ylab = "Rate of Teen Births per 100,000 Residents")
modeltb <- lm(ppprop ~ teenbirth, data = healthdata)
summary(modeltb)
abline(lm(teenbirth ~ ppprop, data = healthdata))

plot(healthdata$ppprop, healthdata$matmort,
     main="Linear Regression of the Planned Parenthood
  Presence and Maternal Mortality Rates",
     xlab = "Proportion of Planned Parenthoods per 1 Million Residents",
     ylab = "Rate of Maternal Mortality per 100,000 Residents")
modelmm <- lm(ppprop ~ matmort, data = healthdata)
summary(modelmm)
abline(lm(matmort ~ ppprop, data = healthdata))

plot(healthdata$ppprop, healthdata$cervcanc,
     main="Linear Regression of the Planned Parenthood Presence 
     and Death by Cervical Cancer Rates",
     xlab = "Proportion of Planned Parenthoods per 1 Million Residents",
     ylab = "Rate of Deaths by Cervical Cancer per 100,000 Residents")
modelcc <- lm(ppprop ~ cervcanc, data = healthdata)
summary(modelcc)
abline(lm(cervcanc ~ ppprop, data = healthdata))