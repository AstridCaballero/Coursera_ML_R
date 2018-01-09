predict<- function(Theta1, Theta2, X) {
  # Useful values
  m = nrow(X)
  num_labels =  nrow(Theta2)
  
  # You need to return the following variables correctly 
  p <- matrix(0,nrow = m, ncol = 1)
  
  # Add ones to the X data matrix
  X <- cbind(matrix(c(1),m,1), X)
  
  # Return values for hidden layer a^(2)
  num_a = nrow(Theta1)
  a <- matrix(0,nrow = m, ncol = num_a)
  
  # Loop to get the values for a^2
  for (sample in 1:m) {
    X_sample <- t(X[sample,])
    a[sample,] <- t(sigmoid(Theta1 %*% t(X_sample)))
    
  }
  # Add ones to the ‘a’ data matrix
  a <- cbind(matrix(1,m,1), a)
  
  # loop to get the values for a^3
  for (sample in 1:m) {
    a_sample <- t(a[sample,])
    p[sample,] <- which.max(sigmoid(Theta2 %*% t(a_sample)))
  }
  return(p)
}
