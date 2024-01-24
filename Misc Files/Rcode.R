# setwd("c:/Users/fangzi/Desktop") #set up the working direction
# setwd("C:\Users\fangzi\Desktop") #wrong
# setwd("C:/Users/fangzi/Desktop/Fang/Stat35000/Fall 2020/Lecture/Lecture 01")

setwd("~/Desktop/STAT-350")
install.packages("geometry")

seedlings=read.csv("seedlings.csv",header=T,na.strings="?") #loading Data
fix(seedlings)
dim(seedlings)
names(seedlings)
seedlings=na.omit(seedlings) # remove the rows with any missing values 
dim(seedlings)
names(seedlings)
X=seedlings$No.Nitrogen
Y=seedlings$Nitrogen
X
Y
X1=c(0.32, 0.53, 0.28, 0.37, 0.47, 0.43, 0.36, 0.42, 0.38, 0.43)
Y1=c(0.26, 0.43, 0.47, 0.49, 0.52, 0.75, 0.79, 0.86, 0.62, 0.46)
X1
Y1
cbind(X, Y)

mean(X)
median(X)
mean(Y)
median(Y)
mean(X, trim=0.1) # This is how you trim the mean in R

pH=c(7.07,7.00,7.10,6.97,7.00,7.03,7.01,7.01,6.98,7.08) # Example of sd, var and sd in lecture
mean(pH)
var(pH)
sd(pH)

height=read.csv("height.csv",header=T,na.strings="?") #load data - Five Number Summary
height=height$Height
range(height)[2]-range(height)[1]
max(height)-min(height)
var(height)
sd(height)
quantile(height,0.75)-quantile(height,0.25) # Represents Q3 and Q1 Respectively
summary(height)

hist(height, breaks="Sturges", right=FALSE) # frequency histogram
hist(height, breaks="Sturges", freq=FALSE, right=FALSE, xlab = "Height", main = "Histogram of Height") # density histogram
b = round(log(length(height))/log(2)) + 1
hist(height, breaks = b + 1, right=FALSE, xlab = "Height", main = "Histogram of Height")
boxplot(height) # boxplot
boxplot(height, horizontal = TRUE)


classdata = read.table("class.txt", header = TRUE) #load .txt data file
dim(classdata)
head(classdata)
stem(classdata$height) #stem leaf plot
# frequency histogram
hist(classdata$height, breaks="Sturges", right=FALSE) 
# density histogram
hist(classdata$height, breaks="Sturges", freq=FALSE, xlab = "Height", main = "Histogram of Height")
boxplot(classdata$height)
boxplot(height~sex, data=classdata)


b = round(log(length(classdata$height))/log(2)) + 1
b
hist(classdata$height, breaks = b + 1,
     include.lowest = TRUE,right=FALSE, xlab = "Height", main = "Histogram of Height")
hist(classdata$height, breaks = "Sturges", right=FALSE, xlab = "Height", main = "Histogram of Height")

# Regression

plot(classdata$height ~ classdata$weight, xlab = "Weight", ylab = "Height",main = "Scatter Plot")
var(classdata)
sd(classdata)
sdy=sd(classdata$height)
sdx=sd(classdata$weight)
covxy=cov(classdata$height, classdata$weight)
r=covxy/(sdx*sdy)
beta=r*sdy/sdx
alpha=mean(classdata$height)-beta*mean(classdata$weight)
r2=r^2


