# Call functions stored in Neural_networks_functions.R
source("Neural_networks_functions.R")

# Loading the data
data <- readMat('/Users/astrid/Documents/Machine_learning/machine-learning-ex3/ex3/ex3data1.mat')


# Variables
input_layer_size  <- 400
hidden_layer_size <- 25
num_labels <- 10

X<-data$X
y<-data$y
m <- nrow(X)

# Loading Pameters
# Load the weights into variables Theta1 and Theta2
w <- readMat('/Users/astrid/Documents/Machine_learning/machine-learning-ex3/ex3/ex3weights.mat')
Theta1 <- w$Theta1
Theta2 <- w$Theta2

# Implement Predict 
pred <- predict(Theta1, Theta2, X)

Accuracy <- mean(pred==y) *100

print(paste("Training Set Accuracy:", Accuracy))

