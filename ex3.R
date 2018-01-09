# call functions stored in Multi_Class_classification_Functions.R
source("Multi_Class_classification_Functions.R")

# Loading the data
data <- readMat('/Users/astrid/Documents/Machine_learning/machine-learning-ex3/ex3/ex3data1.mat')

# Variables
m= dim(data$y)[1]
X<-data$X
y<-data$y
num_labels <- 10

# Vectorize Logistic Regression
# data to test cost function
theta_t <- matrix(c(-2,-1,1,2), nrow=4, ncol= 1)
X_t <- cbind(matrix(1,nrow=5, ncol=1), (matrix(1:15,nrow = 5, ncol = 3))/10)
y_t <- matrix(c(1,0,1,0,1), nrow=5,ncol=1)
lambda_t <- 3
lrCostFunction(theta_t, X_t, y_t, lambda_t)
lrCostFunction_grad(theta_t, X_t, y_t, lambda_t)
# J <- lrCostFunction(theta_t, X_t, y_t, lambda_t)[1]
# grad <- lrCostFunction(theta_t, X_t, y_t, lambda_t)[2]

# One-vs-All Training
lambda <- 0.1
all_theta <- oneVsAll(X, y, num_labels, lambda)

# Predict for One-Vs-All
pred <- predictOneVsAll(all_theta, X)
Accuracy <- mean(pred==y) *100

print(paste("Training Set Accuracy:", Accuracy))






