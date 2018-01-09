sigmoid <- function(z) {
  g<-1 / (1 + exp(-z))
  return(g)
}

lrCostFunction <- function (theta, X, y, lambda) {
  m = nrow(y)
  J <- 0
  theta_J <- theta
  theta_J[1] <- 0
  
  J <- ((-1/m)*sum(y*log(sigmoid(X%*%theta)) + ((1-y)*log(1-sigmoid(X%*%theta))))) + ((lambda/(2*m))*(t(theta_J) %*% theta_J))
  return(J)  

}

lrCostFunction_grad <- function (theta, X, y, lambda) {
  m = nrow(y)
  J <- 0
  theta_J <- theta
  theta_J[1] <- 0
  
  grad<- ((1/m)*(t(X)%*%((sigmoid(X%*%theta)-y))) + ((lambda/m)*theta_J))
  return(grad)
}

oneVsAll <- function(X, y, num_labels, lambda) {
  # Some useful variables
  m <- nrow(X)
  n <- ncol(X)
  
  # You need to return the following variables correctly 
  all_theta <- matrix(c(0), nrow = num_labels, ncol = n+1)
  
  # Add ones to the X data matrix
  X <- cbind(matrix(c(1),m,1), X)
  
  # Empty matrix
  all_theta <- matrix(0,nrow=num_labels, ncol=n + 1)
  
  for (label in 1:num_labels) {
  
    #Create a new vector replacing the values of ‘y’ for 0 or 1 based on the labels 
    class_label <- (label==y) * 1
    
    # Set Initial theta
    initial_theta <- matrix(c(0), nrow = (n+1), ncol=1)
    
      # Set options for fminunc
    theta_label <- optim(initial_theta, fn=lrCostFunction, X=X, y=class_label, lambda=lambda,
                   control=list(maxit=50),
                   gr=lrCostFunction_grad,
                   method="CG")
    all_theta[label,] <- theta_label$par
  }
  return(all_theta)
}


predictOneVsAll <- function(all_theta, X) {
  m <- nrow(X)
  n <- ncol(X)
  num_labels <- nrow(all_theta)
  
  p <- matrix(0,nrow = m, ncol = 1)
  
  # Add ones to the X data matrix
  X <- cbind(matrix(c(1),m,1), X)
  
  for (sample in 1:m) {
    X_sample <- t(X[sample,])
    p[sample,] <- which.max(sigmoid(X_sample %*% t(all_theta)))
  }
  return(p)
}



