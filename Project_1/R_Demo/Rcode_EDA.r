setwd("~/Desktop/STAT-350/Project_1/R_Demo")
classdata = read.table("class.txt", header = TRUE)
dim(classdata)
head(classdata)
touchdown = c(14, 29, 22, 18, 20, 15, 6, 9, 32, 18, 19, 18, 23, 28, 37, 21,
              14, 19, 21, 20, 16, 22, 33, 28, 12, 18, 22, 14, 33, 21, 12)
stem(touchdown)
stem(touchdown, scale = 0.5)
b = round(log2(length(touchdown))) + 1
b
hist(touchdown, breaks = seq(min(touchdown), max(touchdown), length = b + 1),
     include.lowest = TRUE, right = TRUE, xlab = "Touchdown", main = "Histogram of Touchdown")
hist(touchdown, breaks =b + 1,
     include.lowest = TRUE, right=FALSE, xlab = "Touchdown", main = "Histogram of Touchdown")
hist(touchdown, breaks = "Sturges",
     include.lowest = TRUE, right=FALSE, xlab = "Touchdown", main = "Histogram of Touchdown")

summary(touchdown)
boxplot(touchdown, main = "Boxplot of touchdown", horizontal = TRUE)
par(mfrow = c(1, 2))
boxplot(classdata$height, main = "Boxplot of Height")
boxplot(classdata$height ~ classdata$sex, xlab = "Sex", main = "Boxplot of Height")
basses = c(1.22, 1.51, 1.34, 1.6, 0.98, 1.71, 1.82, 1.04, 1.1, 0.85, 1.08)
mean(basses)
sort(basses)
median(basses)
mean(basses, 0.1)
quantile(basses, c(0.25, 0.5, 0.75))
summary(basses)
var(basses)
sd(basses)
plot(classdata$height ~ classdata$weight, xlab = "Weight", ylab = "Height",
     main = "Scatter Plot")
fit = lm(classdata$height ~ classdata$weight)
summary(fit)
result = replicate(1000, sum(sample(0:1, 4, replace = TRUE)))
table(result)/1000
