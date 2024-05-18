# ** Task 1 **
# ----------------------------------------------
# a) Generate random samples and calculate the sample means
# Define the parameters
M <- 1000  # Number of samples
n <- 128    # Number of observations per sample
lambda <- 0.1  # Rate parameter for the exponential distribution

# Generate random samples
XSamples <- replicate(M, rexp(n, rate=lambda))
dim(XSamples)

# b)	Calculate the sample mean of each sample
Xbars <- colSums(XSamples) / n

# c) Obtain sampling distribution sample mean
hist(Xbars, nclass=30, freq=FALSE, main="Sampling Distribution of Xbar when n=16")

# d) Adding approximate density curve to the histogram above
dens <- density(Xbars)
lines(dens$x, dens$y, col=2)

# e) Getting some summary statistics about distribution
# Calculate and display summary statistics
mean_Xbars <- mean(Xbars)
var_Xbars <- var(Xbars)
sd_Xbars <- sd(Xbars)

# Display the results
cat("Mean of Xbars:", mean_Xbars, "\n")
cat("Variance of Xbars:", var_Xbars, "\n")
cat("Standard Deviation of Xbars:", sd_Xbars, "\n")
summary(Xbars)


# B. Sampling from Normal Distribution
# Define the parameters
sample_sizes <- c(16, 25, 36)
means <- c(10, 10, 20, 20)  # Mean values to use
sds <- c(1, 3, 1, 3)        # Standard deviation values to use

# Loop over each combination of mean and sd
for (i in 1:4) {
  for (n in sample_sizes) {
    # Generate samples from the normal distribution
    XSamples <- replicate(M, rnorm(n, mean=means[i], sd=sds[i]))
    Xbars <- colSums(XSamples) / n
    
    # Plotting and summary statistics calculation skipped for brevity
    
    cat("Mean:", means[i], "SD:", sds[i], "Sample size:", n, "\n")
    cat("Mean of Xbars:", mean(Xbars), "\n")
    cat("Standard Deviation of Xbars:", sd(Xbars), "\n\n")
  }
}



# ----------------------------------------------
# ** Task 2 **
# ----------------------------------------------

# A. Simulate random sample of 50 observations from this distribution
n <- 50
p_success <- 0.2
XSample <- rbinom(n, 1, p_success)

# B. Calculate the sample proportion of successes
p_hat_obs <- mean(XSample)
cat("Observed sample proportion, p̂_obs:", p_hat_obs, "\n")

# C. Evaluating observed proportion and checking if it is within 2 s.d
# Calculate the standard deviation of the sampling distribution of p̂
std_error <- sqrt((p_success * (1 - p_success)) / n)

# Check if p̂_obs is within 2 standard deviations of p_0
lower_bound <- p_success - 2 * std_error
upper_bound <- p_success + 2 * std_error

cat("Lower bound:", lower_bound, "\n")
cat("Upper bound:", upper_bound, "\n")

# Determine if p̂_obs supports p_0 = 0.2
if (p_hat_obs >= lower_bound && p_hat_obs <= upper_bound) {
  cat("The observed p̂_obs supports the value of p being 0.2.\n")
} else {
  cat("The observed p̂_obs does not support the value of p being 0.2.\n")
}

# ----------------------------------------------
# ** Task 3 **
# ----------------------------------------------
# A. Compare Pobs with P_0 = 0.4
# New hypothesized probability of success
p_new_success <- 0.4

# Calculate the new standard deviation of the sampling distribution of p̂
std_error_new <- sqrt((p_new_success * (1 - p_new_success)) / n)

# Check if p̂_obs is within 2 standard deviations of the new p_0
lower_bound_new <- p_new_success - 2 * std_error_new
upper_bound_new <- p_new_success + 2 * std_error_new

cat("New lower bound:", lower_bound_new, "\n")
cat("New upper bound:", upper_bound_new, "\n")

# Determine if p̂_obs is reasonably close to the new hypothesized value p_0 = 0.4
if (p_hat_obs >= lower_bound_new && p_hat_obs <= upper_bound_new) {
  cat("The observed p̂_obs might be considered reasonably close to p_0 = 0.4.\n")
} else {
  cat("The observed p̂_obs is not reasonably close to p_0 = 0.4.\n")
}

# B. How typical or extreme p_obs is
# Simulate 10,000 random samples of size 50 each from Bernoulli distribution with p = 0.4
n <- 50
XSamples_new <- replicate(10000, rbinom(n, 1, p_new_success))

# b) Calculate the sample proportion for each sample
hatpk <- colSums(XSamples_new) / n

# c) Plot the histogram of these sample proportions
hist(hatpk, xlab=expression(bar(X)[n]), main="Sampling Distribution under p_0 = 0.4", prob=TRUE)

# d) Calculate the p-value: the proportion of simulated proportions ≤ observed proportion
p_value <- mean(hatpk <= p_hat_obs) # p̂_obs: 0.28 
cat("P-value:", p_value, "\n")




