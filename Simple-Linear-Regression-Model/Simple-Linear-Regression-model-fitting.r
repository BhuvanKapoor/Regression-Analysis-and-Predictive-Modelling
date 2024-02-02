# Simple linear regression model fitting, estimation of parameters, computing R**2 and adjusted R**2 and model interpretation

cars
summary(cars)

speed <- cars$speed
speed_bar <- mean(speed)

distance <- cars$dist
distance_bar <- mean(distance)

# b1 <- cov(speed,distance)/var(speed)
b1 <- sum((speed-speed_bar)*(distance-distance_bar))/sum((speed-speed_bar)**2)
b1

# b0 <- y_bar - b1*x_bar
b0 <- distance_bar - b1*speed_bar
b0

# Scatter Plot, estimation of regression coeff using inbuild function & adding a line of best fit
plot(speed~distance, xlab = "Speed (in mph)", ylab = "Car Stopping Distance (in ft)", main = "Car Speed vs Stopping Distance")

SLR <- lm(distance~speed)
SLR
abline(SLR, col="red")

# Error analysis
# We assume the errors are normally distributed and are independent with constant variance
residual <- SLR$residuals
hist(residual)
# the above histogram is bell-shaped and positively skewed 

# Homoscedasticity Assumption - variance is const
plot(residual~speed)
abline(0,0)   # for slower speed, there is a little variability, while for higher speed there is large variability

# R-sq and Goodness of Fit
summary(SLR)  # high r-sq value represents smaller difference between observed and fitted values

# Hypothesis Testing
# Ho: B1 = 0
# H1: B1 != 0

anova(SLR)

# Using Table Values
Ft <- qf(0.95,1,48)
Ft    
# Ft < F_calculated(using anova), therefore reject Ho

# P-value
pv <- 1 - pf(89.567,1,48) 
pv  # our p-value < alpha (i.e. 0.05), we reject Ho

# Confidence Interval
confint(SLR,level = 0.95)

# Prediction and Confidence intervals for stopping distance when the speed is 15mph
new_data <- data.frame(speed = 15)
new_data
conf <- predict(SLR, new_data, interval = "confidence")
conf
pred <- predict(SLR, new_data, interval = "predict")
pred



#--------------------------------#
#           Exercise             #
#--------------------------------#
# Import the data set "pressure" that represents vapor pressure of mercury as a function of temperature in to R and perform the following.

pressure
temp <- pressure$temperature
pres <- pressure$pressure

# i) Obtain the summary statistics for the imported data set
summary(pressure)

# ii) Estimate the regression coefficients by OLS method.
temp_bar <- mean(temp)
temp_bar
pres_bar <- mean(pres)
pres_bar

b1 <- sum((temp-temp_bar)*(pres-pres_bar))/sum((temp-temp_bar)**2)
b1

# b0 <- y_bar - b1*x_bar
b0 <- pres_bar - b1*temp_bar
b0

# iii) Obtain the scatterplot and estimate the Regression Coefficients using inbuilt function.
plot(pres~temp, xlab = "Temperature", ylab = "Pressure", main = "Pressure vs Temperature")
SLR <- lm(pres~temp)
SLR
abline(SLR, col="red")

# modification to the model
pressure_data_new <- pressure
pressure_data_new$temp2 <- pressure_data_new$temperature**2
pressure_data_new$temp3 <- pressure_data_new$temperature**3
pressure_data_new$temp4 <- pressure_data_new$temperature**4
poly_reg <- lm(pressure~., data = pressure_data_new)

# Visualize
library(ggplot2)
ggplot() + geom_point(aes(pressure_data_new$temperature,pressure_data_new$pressure),col="red") + geom_line(aes(pressure_data_new$temperature,predict(poly_reg, newdata = pressure_data_new)),col="blue") + ggtitle("Polynomial Regression Model") + xlab("Temperature") + ylab("Pressure")

summary(poly_reg)

# iv) Perform test for significance of slope parameter by ANOVA
anova(SLR)

# v) Obtain confidence interval and prediction intervals when the temperature is 250.
new_data <- data.frame(temp = 250)
new_data
conf <- predict(SLR, new_data, interval = "confidence")
conf
pred <- predict(SLR, new_data, interval = "predict")
pred
