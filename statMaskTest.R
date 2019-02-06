remove(list = ls())

x <- 2 + 2
v <- c(1,2,3,4)
A <- cbind(c(2,3,4), c(1,6,2), c(5,1,4))
B <- rbind(c(2,3,4), c(1,6,2), c(5,1,4))
print(A)
print(B)
solve(A) #Inverse
t(B) #Transpose
E <- A*B #Elementwise multiplication
F <- B%*%A #Matrix multiplication 

#Plotting example
age <- c(0,6,12,18,24)
length <- c(51,67,74,82,88)

plot(x = age, y = length, col = 1, pch = 0,main="Infant length", xlab="age", ylab="length (cm)")
legend(x = "topleft", legend = "Data",col = 1, pch=0)

# Solve normal equations
X <- cbind(matrix(1,5,1),age)
beta <- solve(t(X)%*%X)%*%t(X)%*%length

# Do predictions and plot
lengthhat <- beta[1]+age*beta[2]
lines(x=age, y=lengthhat ,col=2 , lty=1)
legend(x = "topleft", legend = c("Data","LR"),col = c(1,2), pch=c(0,NA), lty=c(NA,1))

# Create a data frame
infantdata <- data.frame(age,length)
infantdata

# Plot the data
plot(x = infantdata$age, y = infantdata$length,col = 1, pch = 0, main="Infant length",xlab="age", ylab="length (cm)")

#Linear regression example
# Learn the model
i=1
model.fit <- lm(formula = length ~ age, data = infantdata)
model.fit

# Do predictions and plot
model.pred <- predict(object = model.fit,newdata = infantdata)
lines(x=infantdata$age,y=model.pred, col=2, lty=1)
legend(x = "topleft", legend = c("Data","LR"),col = c(1,2), pch=c(0,NA), lty=c(NA,1))

#Working with datasets
# Install package containing the library we want
install.packages("MASS")
# Load library containing a lot of datasets
library(MASS)
# See all available datasets
data()
# Get information about the Boston dataset
?Boston
# Look at the column names in the data frame
names(Boston)
# Launch an editor for making changes (e.g., fix missing values)
fix(Boston)
# See a short statistical summary about the dataset
summary(Boston)
# make a boxplot
boxplot(Boston)
# Plot the data
plot(x=Boston$lstat, y=Boston$medv)
# Do linear regression
model1.fit <- lm(formula = medv~lstat,data = Boston)
model1.pred <- predict(object = model1.fit,newdata = Boston)
# Plot the result
lines(Boston$lstat,model1.pred, col=2, lty=1)
legend(x = "topright", legend = c("Data","LR"),col = c(1,2), pch=c(0,NA), lty=c(NA,1))

#Working with data sets with multiinput variables
# Input: lstat and age. Output: medv
model2.fit <- lm(formula=medv~lstat+age,data = Boston)
# Input: all input variables. Output: medv
model3.fit <- lm(formula = medv~., data = Boston)
# Input: all input variables except age. Output: medv
model4.fit <- lm(formula = medv~.-age, data = Boston)
# Compute RMSE to evaluate
model3.pred <- predict(object = model3.fit,newdata = Boston)
model3.RMSE <- sqrt(mean((model3.pred-Boston$medv)^2))
model4.pred <- predict(object = model4.fit, newdata = Boston)
model4.RMSE <- sqrt(mean((model4.pred-Boston$medv)^2))

# Draw 1000 samples from a normal distribution with mean 4 and standard deviation 1
y <- rnorm(n=1000,mean=4,sd=1)
# Plot
x <- seq(1,1000) plot(x,y)

#Checking number of rows first
nrow(Boston)
#Setting up training set
train = sample(x=1:nrow(Boston), size=250, replace=FALSE)
Boston.train = Boston[train,]
Boston.test = Boston[-train,]
nrow(Boston.train)
nrow(Boston.test)

#Example function
RMSE <- function(y, yhat){
  r = sqrt(mean((y-yhat)))
  return(r)
}

#Generall useful commands
#Clear workspace: rm(list=ls()) 
#Close all plots: graphics.off()