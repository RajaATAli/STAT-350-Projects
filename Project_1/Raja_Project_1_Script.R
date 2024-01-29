# Task 1

# A) Setting Current Working Directory
setwd("~/Desktop/STAT-350/Project_1")

# B) Reading in the txt file
MHW <- read.table('MHW.txt', header = TRUE, sep = ',')

# C) Printing the top 6 lines of the data set
head(MHW)

# D) Printing the first 10 rows of this data frame
MHW[1:10,]

# E) Attach the data frame to be your working data set
attach(MHW)

# Task 2 - Summarizing the MHW Data

# A) Obtaining the five number summary of the data (including individual columns)
print("This is the five number summary of the whole data set")
summary(MHW)

print("This is the five number summary of the grain column")
summary(MHW$grain)

print("This is the five number summary of the straw column")
summary(MHW$straw)


# B) Obtaining various statistics for grain and straw

# Grain Yield Statistics
print("Statistics for Grain Yield:")
print(paste("Minimum:", min(grain)))
print(paste("Maximum:", max(grain)))
print(paste("Mean:", mean(grain)))
print(paste("Median:", median(grain)))
print(paste("Variance:", var(grain)))
print(paste("Standard Deviation:", sd(grain)))
print("Quantiles:")
print(quantile(grain))
print(paste("Interquartile Range:", IQR(grain)))

# Straw Yield Statistics
print("Statistics for Straw Yield:")
print(paste("Minimum:", min(straw)))
print(paste("Maximum:", max(straw)))
print(paste("Mean:", mean(straw)))
print(paste("Median:", median(straw)))
print(paste("Variance:", var(straw)))
print(paste("Standard Deviation:", sd(straw)))
print("Quantiles:")
print(quantile(straw))
print(paste("Interquartile Range:", IQR(straw)))

# C) Define and calculate a new variable ‘yield.ratio’ which is the ratio 
# between grain and straw and obtain the summary statistics for it.

# Define and Calculate Yield Ratio
yield.ratio <- grain / straw

# Obtaining summary statistics for yield.ratio
print("Summary statistics for yield.ratio:")
summary(yield.ratio)


# Task 3 - Visualizing the Data

# A) Obtain a Stem and Leaf Plot for Grain and Straw Yields

# For Grain Yield
print("Stem and Leaf Display for Grain Yield:")
stem(MHW$grain)

# For Straw Yield
print("Stem and Leaf Display for Straw Yield:")
stem(MHW$straw)

# B) Obtain a simple histogram of the grain and of the straw yields

# Histogram for Grain Yield
hist(grain)

# Histogram for Straw Yield
hist(straw)


# C) Fancy Histograms for Grain and Straw Yields

# Fancy Histogram for Straw Yield
hist(straw, nclass = 30, col = "lightblue", border = "red", main = "The MHW Data - Straw Yield", xlab = "Straw Yield per Plot")

# Fancy Histogram for Grain Yield
hist(grain, breaks = seq(2.6, 5.2, by = 0.1), col = "lightblue", border = "red", main = "The MHW Data - Grain Yield", xlab = "Grain Yield per Plot")


# D) Boxplots for Grain and Straw Yields

# Boxplot for Grain Yield
boxplot(grain, main = "Boxplot of Grain Yield", ylab = "Grain Yield")

# Boxplot for Straw Yield
boxplot(straw, main = "Boxplot of Straw Yield", ylab = "Straw Yield")

# Combined Boxplot for Grain and Straw Yields
boxplot(grain, straw, names = c("Grain", "Straw"), main = "Combined Boxplot of Grain and Straw Yields", ylab = "Yield")


# Task 4 - Regression Analysis on the data. 
# Two ‘measured’ variables were studied: 
# - grain : Grain yield, lbs per plot
# - straw :  Straw yield, lbs per plot

# A) Calculate the mean and SD for the two variables, grain and straw

print("Mean of Grain and Straw:")
print(apply(cbind(grain, straw), 2, mean))

print("Standard Deviation of Grain and Straw:")
print(apply(cbind(grain, straw), 2, sd))


# B) Clearly labelled Scatterplot of Straw(Y) against Grain(X) 
plot(straw ~ grain, main = "Scatterplot of Straw Yield against Grain Yield", xlab = "Grain Yield", ylab = "Straw Yield")


# D) Mark the means of straw (horizontally) and of grain (vertically) on the plot
abline(h = mean(straw), col = 2) # Horizontal line for mean of Straw
abline(v = mean(grain), col = 2) # Vertical line for mean of Grain


# F) Calculate Covariance and Correlation between Straw and Grain
covariance <- cov(grain, straw)
print(paste("Covariance between Grain and Straw:", covariance))

correlation <- covariance / (sd(grain) * sd(straw))
print(paste("Correlation between Grain and Straw:", correlation))

# G) Calculate Least Squares Estimates of the regression coefficients for the regression of Straw on Grain
fit <- lm(straw ~ grain)
coefficients <- coef(fit)
print("Coefficients of the regression line:")
print(coefficients)


# Direct calculation using the formula
beta <- cor(straw, grain) * (sd(straw) / sd(grain))
alpha <- mean(straw) - (beta * mean(grain))

print(paste("Calculated Intercept (alpha):", alpha))
print(paste("Calculated Slope (beta):", beta))



# H) Fitted regression line
print("Fitted regression line: Straw Yield = a + b * Grain Yield")
cat("Straw Yield =", coefficients[1], "+", coefficients[2], "* Grain Yield\n")

print("Fitted regression line: Straw Yield = a + b * Grain Yield")
cat("Straw Yield =", alpha, "+", beta, "* Grain Yield\n")


# I) Fit the regression line to the plot
abline(alpha, beta, col = 2, lwd = 2)


# J) Verify results using the lm() function
yield.fit <- lm(straw ~ grain)
print("Linear Model Fit:")
print(summary(yield.fit))

# K) Predicting straw yield for a plot with 4.0 lb of grain yield
predicted_straw_yield <- predict(yield.fit, newdata = data.frame(grain = 4.0))
print(paste("Predicted Straw Yield for 4.0 lb of Grain Yield:", predicted_straw_yield))
Manual_Predicted_Straw_Yield <- alpha + beta * 4
print(paste("Predicted Straw Yield for 4.0 lb of Grain Yield:", Manual_Predicted_Straw_Yield))




# Misc Code - Not part of requirements of project
# Dimensions
dim(MHW)
names(MHW)