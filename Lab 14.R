install.packages('gridExtra')
library(ggplot2)
library(gridExtra)

# Explore the dataset 
head(WASDE)
str(WASDE)

# Corn Price Plot
  g_price <- ggplot(data = WASDE, aes(x = year, y = corn_price)) +
  geom_line(color = "darkgreen") +
  ggtitle("Corn Prices Over Time (1973-2019)") +
  labs(y = "Corn Price ($)", x = "Year")
  
# Corn Demand Plot
  g_demand <- ggplot(data = WASDE, aes(x = year, y = total_use)) +
    geom_line(color = "blue") +
    ggtitle("Corn Demand Over Time (1973-2019)") +
    labs(y = "Use (Mil bu.)", x = "Year")
  
# Corn Supply Plot
  g_supply <- ggplot(data = WASDE, aes(x = year, y = total_supply)) +
    geom_line(color = "orange") +
    ggtitle("Corn Supply Over Time (1973-2019)") +
    labs(y = "Supply (Mil bu.)", x = "Year")
  
  grid.arrange(g_price, g_demand, g_supply, nrow = 3)

  
WASDE$SUR <- WASDE$end_stocks / WASDE$total_use  

ggplot(data = WASDE, aes(x = SUR, y = corn_price)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", color = "blue") +
  ggtitle("Corn Prices vs. Stock-to-Use Ratio (1973-2019)") +
  labs(x = "Stock-to-Use Ratio (SUR)", y = "Corn Price ($)")  

# Estimate the linear regression model
reg1 <- lm(corn_price ~ SUR, data = WASDE)

# View summary of regression results
summary(reg1)

# Calculate averages
mean_sur <- mean(WASDE$SUR, na.rm = TRUE)
mean_price <- mean(WASDE$corn_price, na.rm = TRUE)

# Get the beta coefficient from the model
beta_hat <- coef(reg1)["SUR"]

# Calculate elasticity
elasticity <- beta_hat * (mean_sur / mean_price)
elasticity  

# Summary statistics of residuals
summary(resid(reg1))

# Histogram of residuals
hist(resid(reg1), 
     main = "Histogram of Linear Regression Errors",
     xlab = "Linear Model Residuals",
     col = "lightblue", border = "white")  

# Residuals vs SUR plot
ggplot(data = WASDE, aes(x = SUR, y = resid(reg1))) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Residuals vs. Stock-to-Use Ratio") +
  labs(x = "Stock-to-Use Ratio (SUR)", y = "Residuals")  

# Nicely formatted regression summary
install.packages('gtsummary')
library(gtsummary)
tbl_regression(reg1, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))

# Create the inverse of stock-to-use ratio
WASDE$SUR_Inv <- 1 / WASDE$SUR

# Estimate the inverse SUR regression model
reg2 <- lm(corn_price ~ SUR_Inv, data = WASDE)

# View summary of regression
summary(reg2)

# Generate formatted regression table
tbl_regression(reg2, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))

# Summary statistics of residuals
summary(resid(reg2))

# Histogram of residuals
hist(resid(reg2),
     main = "Histogram of Non-linear Regression Errors",
     xlab = "Non-linear Model Residuals",
     col = "lightgreen", border = "white")

# Residuals vs SUR scatterplot
ggplot(data = WASDE, aes(x = SUR, y = resid(reg2))) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Non-linear Regression Errors vs. Stock-to-Use Ratio") +
  labs(x = "Stock-to-Use Ratio", y = "Residuals")

# Create a categorical period label and numeric dummy variable for 2006–2019
WASDE$period <- ifelse(WASDE$year >= 2006, "2006-2019", "1973-2005")
WASDE$P2006 <- as.numeric(WASDE$year >= 2006)

# Scatterplot with linear trends by period
ggplot(data = WASDE, aes(x = SUR, y = corn_price, color = period)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE) +
  ggtitle("Corn Prices vs. Stock-to-Use Ratio (1973–2019)") +
  labs(x = "Stock-to-Use Ratio", y = "Corn Price ($)") +
  theme_minimal()

# Run regression with interaction term
reg3 <- lm(corn_price ~ SUR + P2006 + SUR:P2006, data = WASDE)

# View regression summary
summary(reg3)

# Collect the residuals from the last regression, create a time series of the errors with a one-year lag of the error, then regress the error terms on the lagged error terms
error <- ts(resid(reg3), start=1973, end=2019, frequency=1)   # the ts() function tells R to set the errors as a time-series 
lag_error <- lag(error, -1)                                   # the lag() function creates a one-period lag of the error term
error <- cbind(error, lag_error)                              # cbind() binds the specified vectors together as columns to create a new data frame

reg4 <- lm(error ~ lag_error, data=error)

summary(reg4)
