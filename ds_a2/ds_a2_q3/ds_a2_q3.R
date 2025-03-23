# Function to perform LDA
#
# INPUT
# Five vectors:
# x1: first predictor
# x2: second predictor
# y: classes
# new1: new first predictor
# new2: new second predictor
#
# OUTPUT
# A vector of predicted classes for new1 and new2.
get_lda2 <- function(x1, x2, y, new1, new2) {
  # Your Code Here
  # Input Check
  # Numeric Predictors
  if (!is.numeric(x1) || !is.numeric(x2) || !is.numeric(new1) || !is.numeric(new2)) {
    stop("All predictors (x1, x2, new1, new2) must be numeric!")
  }
  # Categorical Response Variables
  if (!is.character(y) && !is.factor(y)) {
    stop("y must be <chr> or <fct>!")
  }
  # Length Check
  if (length(x1) != length(x2) || length(x1) != length(y)) {
    stop("x1, x2 and y must have equal lengths!")
  }
  if (length(new1) != length(new2)) {
    stop("new1 and new2 must have equal lengths!")
  }
  
  # Turn response variables into factors
  y <- as.factor(y)
  
  # Edge cases check
  if (length(levels(y)) < 2) {
    stop("y must have 2 levels!")
  }
  if (any(table(y) < 2)) {
    stop("Each level of y must have 2 values!")
  }
  if (var(x1) == 0 || var(x2) == 0) {
    stop("Each predictors should have non-zero variances!")
  }
  
  # Combine predictors
  X <- cbind(x1, x2)
  new_data <- cbind(new1, new2)
  
  # LDA Calculation
  means <- t(sapply(levels(y), function(level) colMeans(X[y == level, , drop = FALSE])))
  cov_matrix <- cov(X)
  
  # Check whether the covariance matrix is not invertible
  if (det(cov_matrix) == 0) {
    stop("The Covariance Matrix is invertible!")
  }
  
  # Calculate the discriminant scores for new data
  inv_cov_matrix <- solve(cov_matrix)
  discriminant_scores <- t(apply(new_data, 1, function(row) {
    sapply(levels(y), function(level) {
      -0.5 * t(means[level, ]) %*% inv_cov_matrix %*% means[level, ] + 
        t(row) %*% inv_cov_matrix %*% means[level, ]
    })
  }))
  
  predicted_classes <- levels(y)[apply(discriminant_scores, 1, which.max)]
  
  return(predicted_classes)
}

