install.packages("forecast")

library(forecast)

rm(list = ls())
setwd("C:/Users/beakl/IE 4031")
par(pty = "s")

data <- read.csv("ProjectData.csv")
head(data)

#year is dt_year
#ghg is greenhousegas_emissione_mtco2equivalent

data3 <- aggregate(greenhousegas_emissione_mtco2equivalent ~ dt_year, data = data, mean)
print(data3)

attach(data3)

new_column_names <- c("Year","GHGEmission")
names(data3) <- new_column_names

plot(data3$Year, data3$GHGEmission, 
     main = "AVG GHG vs Year", 
     xlab = "Year", 
     ylab = "GHG",
     pch = 16,  # Use a solid point as the marker
     col = "blue"  # Use blue color for the points
)

# Plot with Title and Y-axis Label
plot.ts(data3$GHGEmission, pch = 20, type = "o", lty = 5, main = "GHG Emissions Over Time", ylab = "GHG Emissions")
abline(mean(data3$GHGEmission), 0, lwd = 2, col = "orange")
t = 1:length(data3$GHGEmission)

model1 = lm(data3$GHGEmission~1)
summary(model1)
anova(model1)

  # LR model: y = beta0
model2 = lm(data3$GHGEmission ~ t)
abline(model2, col = "red", lwd = 2)
summary(model2)

# For the square term t^2, you need to put it inside I() so that lm() can recognize it as a square term.
model3 = lm(data3$GHGEmission ~ t + I(t^2))
summary(model3)
y = predict(model3, newdata = data.frame(t = 1:18))
lines(x = 1:18, y, lwd = 2, col = "green")


anova(model3)
anova(model2)
summary(model3)
summary(model2)

predict(model2,newdata = data.frame(t=c(19,20)))

library(tseries)
adf.test(data3$GHGEmission)

plot.ts(diff(data3$GHGEmission, differences = 2))
adf.test(diff(data3$GHGEmission, differences = 2))

plot.ts(diff(data3$GHGEmission, differences = 2), pch = 20, type = "o", lty = 5, main = "GHG Emissions Over Time", ylab = "GHG Emissions")
abline(mean(diff(data3$GHGEmission, differences = 2)), 0, lwd = 2, col = "orange")
t = 1:length(diff(data3$GHGEmission, differences = 2))
model1 = lm(diff(data3$GHGEmission, differences = 2)~1)
summary(model1)
anova(model1)
model2 = lm(diff(data3$GHGEmission, differences = 2) ~ t)
abline(model2, col = "red", lwd = 2)
summary(model2)

#DETECTING AUTOCORRELATION

#Durbin-Watson test 
install.packages("lmtest")
library(lmtest)

# dwtest by defualut does the Positive Autocorrelatin Test
dwtest(model2) 

dwtest(model2, alternative = "less")
dwtest(model2, alternative = "two.sided")

# Autocorrelation
#acf(model2) 
#pacf(model2)

#Plot Residuals
residuals <- residuals(model2)
plot(residuals)

#Normality of Residuals
hist(residuals)
qqnorm(residuals); qqline(residuals)

#NOT WORKING
residuals <- residuals(model2)
outliers <- tsoutliers(residuals)
print(outliers)

#OUTLIERS
# Residual analysis for model2
residuals_model2 <- residuals(model2)
plot(model2, which = 1)  # Residuals vs Fitted
plot(model2, which = 3)  # Cook's distance

# Identify potential outliers in model2
outliers_model2 <- which(abs(residuals_model2) > 2 * sd(residuals_model2))


#Normality (residuals) plot
qqnorm(model2$res)
qqline(model2$res)

#Histogram (residuals)
hist(model2$res)
#Residuals vs fitted plot
plot(model2$fit,model2$res)

#Normality test
#To see the mean of residuals
mean(model2$res)
#Shapiro-Wilk normality test
shapiro.test(model2$res)
#Anderson-Darling normality test
ad.test(model2$res)

residuals_model2 <- residuals(model2)

# Install and load the nortest package
#install.packages("nortest")
#library(nortest)

# Anderson-Darling test for normality
#ad_test_result <- ad.test(residuals_model2)
#print(ad_test_result)



#EXPONENTIAL SMOOTHING
half = 9
meanhalf = mean(data3$GHGEmission[1:9]) #MAY HAVE TO FIX THIS

#h is Number of periods for forecasting.
#intial is the method used for selecting intial state values.
es_model = ses(data3$GHGEmission, h=6, initial="optimal")
summary(es_model[["model"]])


#Provide the optimal value of alpha and the initial value l0
es_model[["model"]]$par

autoplot(es_model, ylab="GHGEmission") + 
  autolayer(fitted(es_model), series="Fitted")

predict(es_model)
forecast(es_model)


#EXP SMOOTHING 2nd Diff
half = 9
meanhalf = mean(data3$GHGEmission[1:9]) #MAY HAVE TO FIX THIS

#h is Number of periods for forecasting.
#intial is the method used for selecting intial state values.
es_model = ses(diff(data3$GHGEmission, differences = 2), h=6, initial="optimal")
summary(es_model[["model"]])


#Provide the optimal value of alpha and the initial value l0
es_model[["model"]]$par

autoplot(es_model, ylab="GHGEmission") + 
  autolayer(fitted(es_model), series="Fitted")

predict(es_model)
forecast(es_model)





# Assuming data3$GHGEmission is your time series
your_time_series <- data3$GHGEmission

# Generate forecasts
forecast_values <- forecast(es_model)

# Actual values
actual_values <- your_time_series

# Assuming 'holt_model2' is the result of the Holt's Exponential Smoothing model
# Assuming 'data3' is your dataset with the observed GHGEmission values

# Extracting the fitted values and the original data for the observed period
fitted_values <- fitted(es_model)
observed_values <- data3$GHGEmission

# Calculating the absolute percentage error for each observation
absolute_percentage_error <- abs((observed_values - fitted_values) / observed_values) * 100

# Calculating the mean absolute percentage error (MAPE)
mape <- mean(absolute_percentage_error, na.rm = TRUE)

# Displaying the MAPE
print(paste("MAPE:", mape))


# Assuming 'holt_model2' is the result of the Holt's Exponential Smoothing model
# Assuming 'data3' is your dataset with the observed GHGEmission values

# Extracting the fitted values and the original data for the observed period
fitted_values <- fitted(es_model)
observed_values <- data3$GHGEmission

# Calculating the absolute deviation for each observation
absolute_deviation <- abs(observed_values - fitted_values)

# Calculating the mean absolute deviation (MAD)
mad <- mean(absolute_deviation, na.rm = TRUE)

# Displaying the MAD
print(paste("MAD:", mad))


residuals <- residuals(es_model)
tsoutliers(residuals)

Box.test(residuals, lag = 20, type = "Ljung-Box")

#HOLT MODEL
plot.ts(data3$GHGEmission)
holt_model = holt(data3$GHGEmission, h=6)
summary(holt_model[["model"]])

#Provide the optimal values of alpha, beta(gamma) 
# and the initial values, l0, b0
holt_model[["model"]]$par

autoplot(holt_model, ylab="GHGEmission") + 
  autolayer(fitted(holt_model), series="Fitted")



predict(holt_model)

residualsHolt <- residuals(holt_model)

# Plotting residuals
plot(residualsHolt, main = "Residuals Plot")

plot.ts(data3$GHGEmission)
holt_model2 = holt(data3$GHGEmission)
summary(holt_model2[["model"]])

#Provide the optimal values of alpha, beta(gamma) 
# and the initial values, l0, b0
holt_model2[["model"]]$par

autoplot(holt_model2, ylab="GHGEmission") + 
  autolayer(fitted(holt_model), series="Fitted")



predict(holt_model)

residualsHolt <- residuals(holt_model)

# Assuming 'holt_model2' is the result of the Holt's Exponential Smoothing model
# Assuming 'data3' is your dataset with the observed GHGEmission values

# Extracting the fitted values and the original data for the observed period
fitted_values <- fitted(holt_model2)
observed_values <- data3$GHGEmission

# Calculating the absolute percentage error for each observation
absolute_percentage_error <- abs((observed_values - fitted_values) / observed_values) * 100

# Calculating the mean absolute percentage error (MAPE)
mape <- mean(absolute_percentage_error, na.rm = TRUE)

# Displaying the MAPE
print(paste("MAPE:", mape))


# Assuming 'holt_model2' is the result of the Holt's Exponential Smoothing model
# Assuming 'data3' is your dataset with the observed GHGEmission values

# Extracting the fitted values and the original data for the observed period
fitted_values <- fitted(holt_model2)
observed_values <- data3$GHGEmission

# Calculating the absolute deviation for each observation
absolute_deviation <- abs(observed_values - fitted_values)

# Calculating the mean absolute deviation (MAD)
mad <- mean(absolute_deviation, na.rm = TRUE)

# Displaying the MAD
print(paste("MAD:", mad))

