# Data set
setwd("~/Desktop/STAT-350")
data <- c(2.5, 3.2, 0.4, 3.2, 1.3, 0.2, 0.3, 6.2, 5.7, 6.6,
          0.6, 2.5, 1.5, 4.2, 5.8, 1.9, 4.4, 0.5, 4.2, 0.6,
          1.4, 0.5, 2.7, 5.3, 1, 5.5, 5.9, 5.7, 1.2, 0.6)

stem(data)
mean(data)
sd(data)

data_vector <- c(3.8, 2.8, 2.87, 2.97, 3.1, 1.84, 2.51, 3.15, 2.38, 2.2, 
                 2.78, 2.54, 2.73, 2.78, 3.54, 3.99, 3.46, 2.05, 2.93, 2.86,
                 3.25, 2.46, 2.11, 3.73, 3.09, 3.68, 2.32, 2.59, 3.41, 3.39)

mean(data_vector)
sd(data_vector)
min(data_vector)
max(data_vector)

# Create a relative frequency histogram
hist(data_vector, freq = FALSE, main = "Relative Frequency Histogram", 
     xlab = "Staff Salaries (in dollars per pupil)", col = "lightblue")

hist(data_vector, breaks = 8, freq = FALSE, main = "Relative Frequency Histogram", 
     xlab = "Staff Salaries (in dollars per pupil)", col = "lightblue")
stem(data_vector)


# Q5
lamp_lifetime <- c(963, 1211, 767, 1063, 862, 930, 1096, 912, 946, 1059, 1066, 1212, 1195, 982, 903, 914, 951, 
                   1010, 1123, 889, 1259, 1155, 1344, 1152, 885, 1003, 1305, 996, 1106, 1138, 945, 870, 984, 
                   1171, 1141, 972, 822, 998, 899, 1063, 1386, 794, 1208, 1127, 962, 956, 1280, 1063, 740, 903)

# Midterm grades (x)
midterm_grades <- c(78, 50, 71, 73, 82, 90, 97, 99, 69)

# Final exam grades (y)
final_grades <- c(83, 67, 78, 37, 48, 80, 99, 99, 71)

# Fitting the linear model
model <- lm(final_grades ~ midterm_grades)

# Extracting the coefficients (intercept and slope)
coefficients <- coef(model)

print(coefficients)


# Low Velocity
low_velocity <- c(75.90, 75.98, 76.08, 76.03, 76.30, 75.71, 76.00, 76.28, 76.31, 75.58)

# High Velocity
high_velocity <- c(93.32, 92.54, 92.38, 93.68, 93.33, 93.27, 93.72, 93.31, 94.29, 92.37)
mean(low_velocity)
mean(high_velocity)
sd(low_velocity)
sd(high_velocity)
