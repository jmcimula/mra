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

#Correlation
library(corrplot)
corrplot(cor(mra), method="circle", is.corr=TRUE)

#Try to user Spark with R

library(sparklyr)
library(dplyr)

sc <- spark_connect("local", version = "1.6.1")
mra_tbl <- copy_to(sc, mra, "mra", overwrite = TRUE)

# transform our data set, and then partition into 'training', 'test'
partitions <- mra_tbl %>%
  filter(X5 >= 20) %>%
  sdf_partition(training = 0.8, test = 0.2, seed = 1099)

# fit a linear mdoel to the training dataset
fit <- partitions$training %>%
       ml_linear_regression(response = "X5", features = c("X1", "X2", "X3", "X4"))
       print(fit)


library(ggplot2)

# compute predicted values on our test dataset
predicted <- predict(fit, newdata = partitions$test)

# extract the true 'X5' values from our test dataset
actual <- partitions$test %>%
  select(X5) %>%
  collect() %>%
  `[[`("X5")

# produce a data.frame housing our predicted + actual 'X5' values
data <- data.frame(
  predicted = predicted,
  actual    = actual
)

# plot predicted vs. actual values
ggplot(data, aes(x = actual, y = predicted)) +
  geom_abline(lty = "dashed", col = "red") +
  geom_point() +   theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1) +
  labs(
    x = "Actual Fuel Consumption",
    y = "Predicted Fuel Consumption",
    title = "Predicted vs. Actual Fuel Consumption"
  )