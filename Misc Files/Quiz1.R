setwd("~/Desktop/STAT-350")
# Create vectors for low and high temperatures
low_temp <- c(2.09, 2.13, 2.26, 2.06, 2.22, 2.06, 2.04, 2.15, 2.02, 2.13, 2.18, 2.03)
high_temp <- c(2.56, 2.11, 2.54, 2.06, 2.33, 2.04, 1.98, 2.49, 2.07, 2.49, 2.25, 2.01)

# Combine into a data frame
temperature_data <- data.frame(Low_Temperature = low_temp, High_Temperature = high_temp)

# View the data frame
print(temperature_data)

# Calculate mean and standard deviation for Low Temperature
mean_low_temp <- mean(temperature_data$Low_Temperature)
sd_low_temp <- sd(temperature_data$Low_Temperature)

# Calculate mean and standard deviation for High Temperature
mean_high_temp <- mean(temperature_data$High_Temperature)
sd_high_temp <- sd(temperature_data$High_Temperature)

# Print the results
cat("Mean of Low Temperature:", mean_low_temp, "\n")
cat("Standard Deviation of Low Temperature:", sd_low_temp, "\n")
cat("Mean of High Temperature:", mean_high_temp, "\n")
cat("Standard Deviation of High Temperature:", sd_high_temp, "\n")

