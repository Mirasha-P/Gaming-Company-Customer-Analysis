# Import libraries
library(tidyverse)

library(skimr)

library(DataExplorer)
 
library(moments)

library(plotly)

library(caret)

library(broom)
library(GGally)

# Import the dataset
reviews <- read.csv("turtle_reviews.csv",header = T)

#View the dataframe
head(reviews)
as_tibble(reviews)

# View descriptive statistics
summary(reviews)
skim(reviews)
create_report(reviews)

# Check Nulls
sum(is.na(reviews))

# Check duplicates
sum(duplicated(reviews))

# Remove language and platform
reviews_df = subset(reviews, select = -c(language,platform))
head(reviews_df)

#Rename columns
names(reviews_df)
reviews_df <- reviews_df %>%
  rename(Salary =remuneration..k..,
         Spending = spending_score..1.100.)

# View the dataframe
head(reviews_df)
as.tibble(reviews_df)

############################################################################
# Explore Categorical Variables
table(reviews$gender)
table(reviews$education)
table(reviews$product)


#Gender (Female customers are high)
barplot(table(reviews_df$gender),
        main = "Customers by Gender",
        col = '#66c2a5',
        ylab = "count",)

#Education (Majority Grads)
barplot(table(reviews_df$education),
        main = "Customers by Education",
        col = '#66c2a5',
        ylab = "count",)

# Categorical Variables relationship exploration
## Education by gender
ggplot(reviews_df,
       mapping = aes(x=education,fill = gender))+
  geom_bar()
#Graduate customers are high, among that group females are hgh.
###############################################################
#Quantative Variables
##Age (Majority age 35-40)
ggplot(reviews_df,
       mapping = aes(x=age))+
  geom_histogram(bins = 10,fill = '#66c2a5')+
  scale_x_continuous(breaks = seq(0,70,10))+
  theme_classic()+
  labs(title = "Age Distribution",
       x = "Age",
       y = "Count")

## Salary (Most reviews are from mid income earners (40-50), is between 110-120 outliers?)
ggplot(reviews_df,
       mapping = aes(x=Salary))+
  geom_histogram(bins = 15,fill = '#66c2a5',color = 'white')+
  scale_x_continuous(breaks = seq(0,120,10))+
  theme_classic()+
  labs(title = "Salary Distribution",
       x = "Salary",
       y = "Count")
#is between 110-120 outliers?
boxplot(reviews_df$Salary)

table(reviews_df$Spending)

## Spending (40-50 highest category)
ggplot(reviews_df,
       mapping = aes(x=Spending))+
  geom_histogram(bins = 10,fill = '#66c2a5')+
  scale_x_continuous(breaks = seq(0,110,10))+
  theme_classic()

## Loyalty points (high freq points from 1200-1800) 
ggplot(reviews_df,
       mapping = aes(x=loyalty_points))+
  geom_histogram(bins = 10)+
  scale_x_continuous(breaks = seq(0,7000,1000))+
  scale_y_continuous(breaks = seq(0,600,100))



## Gender by Age
 ggplot(reviews_df,
       mapping = aes(x=age, y=gender))+
  geom_boxplot()

## Spending vs Loyalty (Spending behavior correlates positively with loyalty point accumulation.)
Spending_loyalty <- ggplot(reviews_df,
       aes(x=Spending, y=loyalty_points))+
  geom_point(color = '#66c2a5')+
  geom_smooth(method = "lm",se=F)+
  labs(title = "Spending and Loyalty Points",
       x = "Spending Score",
       y = "Loyalty Points")+
  theme_classic()+
  scale_x_continuous(breaks = seq(0,100,20))+
  scale_y_continuous(breaks = seq(0,7000,1000))

#Interactive plots
Spending_loyalty <- ggplotly(Spending_loyalty)
Spending_loyalty

#Loyalty by Salary
ggplot(reviews_df,
       mapping = aes(x=Salary, y=loyalty_points))+
  geom_point(color = '#66c2a5')+
  geom_smooth(method = "lm",se=F)+
  labs(title = "Loyalty Points by Remuneration",
       x = "Salary",
       y = "Loyalty Points")+
  theme_classic()

## Salary vs Spending
ggplot(reviews_df,
       mapping = aes(x=Salary, y=Spending))+
  geom_point(color = '#66c2a5')+
  geom_smooth(se=F)+
  labs(title = "Spending Score by Remuneration",
       x = "Salary",
       y = "Spending Score")+
  theme_classic()

# Loyalty points by gender
ggplot(reviews_df,
       mapping = aes(x=gender, y=loyalty_points))+
  geom_violin(fill = '#66c2a5',lwd=0)+
  geom_boxplot(fill = '#006400',
               width = 0.25,
               outlier.color = '#006400',
               outlier.size = 1,
               outlier.shape = 'square',
               notch = T)+
  labs(title = "Loyalty Points by Gender",
       x = "Gender",
       y = "Loyalty Points")+
  theme_classic() ### Remove loyalty points above 6000


#Loyalty by Age
ggplot(reviews_df,
       mapping = aes(y=loyalty_points, x=age))+
  geom_point(color = '#66c2a5')+
  geom_smooth(method = "lm",se=F)+
  labs(title = "Loyalty Points by Age",
       x = "Age",
       y = "Loyalty Points")+
  theme_classic()+
  scale_x_continuous(breaks = seq(0,80,10))+
  scale_y_continuous(breaks = seq(0,7000,1000))

#Spending by Age
ggplot(reviews_df,
       mapping = aes(y=Spending, x=age))+
  geom_point(color = '#66c2a5')+
  geom_smooth(method = "lm",se=F)+
  labs(title = "Spending by Age",
       x = "Age",
       y = "Spending Score")+
  theme_classic()+
  scale_x_continuous(breaks = seq(0,80,10))


#Loyalty by Education
ggplot(reviews_df,
       mapping = aes(y=loyalty_points, x=education))+
  geom_boxplot(fill = '#66c2a5')+
  geom_smooth(method = "lm",se=F)+
  labs(title = "Education and Loyalty Points",
       x = "Education",
       y = "Loyalty Points")+
  theme_classic()+
  scale_y_continuous(breaks = seq(0,7000,500))

ggplot(reviews_df,
       mapping = aes(y=loyalty_points, x=education))+
  geom_bar(stat = "summary", fun = "median", fill = '#66c2a5')+
  labs(title = "Loyalty Points by Education Level ",
       x = "Education",
       y = "Loyalty Points")+
  theme_classic()

#Spending by Education Level
ggplot(reviews_df,
       mapping = aes(y=Spending, x=education))+
  geom_bar(stat = "summary", fun = "median", fill = '#66c2a5')+
  labs(title = "Spending by Education Level ",
       x = "Education",
       y = "Loyalty Points")+
  theme_classic()

# Descriptive statistics for loyalty points
summary(reviews_df$loyalty_points)

# Determine the variance.
var(reviews_df$loyalty_points)

# Return the standard deviation.
sd(reviews_df$loyalty_points)

###############################################################################

#Distribution of the data.

# Specify boxplot function.
boxplot(reviews_df$loyalty_points)

# Specify histogram function.
hist(reviews_df$loyalty_points)

###############################################################################

#Determine normality of data.

# Specify qqnorm function (draw a qqplot).
qqnorm(reviews_df$loyalty_points)

# Specify qqline function.
qqline(reviews_df$loyalty_points) 
## The Q-Q plot shows that the data points deviate from the straight line, 
##indicating that the loyalty points data does not follow a normal distribution.


## Shapiro-Wilk test:
# Specify shapiro.test function (Shapiro-Wilk test).
shapiro.test(reviews_df$loyalty_points)
#The Shapiro-Wilk test indicates that the loyalty points data is not normally distributed (p-value < 0.05)

## Skewness and Kurtosis
# Specify the skewness and kurtosis functions.
skewness(reviews_df$loyalty_points)
kurtosis(reviews_df$loyalty_points) #heacy tailed

#Evaluate the Goodness of Fit and Interpret the Model Summary Statistics
# Fit the multiple linear regression model
model <- lm(loyalty_points ~ age + Salary + Spending, data = reviews_df)

# Summarize the model
summary(model)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)

# Scatter plot matrix
ggpairs(reviews_df, columns = c("loyalty_points", "age", "Salary", "Spending"))

# Evaluate the goodness of fit
# Extract residuals and fitted values
residuals <- model$residuals
fitted_values <- model$fitted.values

# Plot residuals vs fitted values
ggplot(data = reviews_df, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")

# Prediction using the model
##Scenario 1: A 20-year-old customer with a salary of 20,000 and a spending score of 20.
##Scenario 2: A 45-year-old customer with a salary of 50,000 and a spending score of 60
##Scenario 3: A 60-year-old customer with a salary of 80,000 and a spending score of 90.

# Creating a sample scenario for prediction
new_data <- data.frame(age = c(20, 45,60), Salary = c(20,50,80), Spending = c(20, 60, 90))
predicted_loyalty_points <- predict(model, newdata = new_data)
predicted_loyalty_points

# Visualize the model's predictions
# Adding predictions to the original dataset
reviews_df <- reviews_df %>%
  mutate(predicted_loyalty_points = predict(model, newdata = reviews_df))

# Scatter plot of actual vs predicted loyalty points
ggplot(data = reviews_df, aes(x = loyalty_points, y = predicted_loyalty_points)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Loyalty Points", x = "Actual Loyalty Points", y = "Predicted Loyalty Points")

# Calculate evaluation metrics manually
mae <- mean(abs(reviews_df$loyalty_points - reviews_df$predicted_loyalty_points))
mse <- mean((reviews_df$loyalty_points - reviews_df$predicted_loyalty_points)^2)
rmse <- sqrt(mse)
r2 <- summary(model)$r.squared

# Print the evaluation metrics
cat("MAE: ", mae, "\n")
cat("MSE: ", mse, "\n")
cat("RMSE: ", rmse, "\n")
cat("R-squared: ", r2, "\n")
