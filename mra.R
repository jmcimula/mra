#Load data
mra <- read.csv("data.csv")

#The data contain seven variables (X1, X2, X3, X4, X5) grouped by city and there are 53 observations. Those variables are well explained below:

#X1 = death rate per 1000 residents
#X2 = doctor availability per 100,000 residents
#X3 = hospital availability per 100,000 residents
#X4 = annual per capita income in thousands of dollars
#X5 = population density people per square mile

fit <- lm (X5 ~ X1 + X2 + X3 + X4, data = mra)

#Summary
summary(fit)

#Plot
plot(fit)

#Multicollinearity
vif(fit)