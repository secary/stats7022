# Function to implement k-means algorithm
#
# INPUT:
# x: numeric vector
# k: number of groups
#
# OUTPUT
# A vector of integer labels indicating group membership.
get_kmeans <- function(x, k) {
  # Input validation checks
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }
  if (!is.numeric(k) || length(k) != 1 || k %% 1 != 0) {
    stop("k must be a single integer value")
  }
  if (k < 1) {
    stop("k must be at least 1")
  }
  if (length(unique(x)) < k) {
    stop("x must have at least k unique observations")
  }
  
  # Handle trivial case where k == 1
  if (k == 1) {
    return(rep(1, length(x)))
  }
  
  # Initialize centroids with k unique values from x
  unique_x <- unique(x)
  if (length(unique_x) == k) {
    # If exactly k unique values, use them as initial centroids
    centroids <- sort(unique_x)
  } else {
    # Otherwise sample k unique values
    centroids <- sort(sample(unique_x, k))
  }
  
  # Initialize variables for iteration
  labels <- integer(length(x))
  changed <- TRUE
  max_iter <- 100
  iter <- 0
  
  # K-means algorithm
  while (changed && iter < max_iter) {
    iter <- iter + 1
    old_centroids <- centroids
    
    # Assign each point to the nearest centroid
    distances <- sapply(centroids, function(c) abs(x - c))
    new_labels <- apply(distances, 1, which.min)
    
    # Update centroids
    for (i in 1:k) {
      if (sum(new_labels == i) > 0) {
        centroids[i] <- mean(x[new_labels == i])
      }
    }
    
    # Check for convergence
    changed <- !all(new_labels == labels)
    labels <- new_labels
  }
  
  # Order labels by cluster means (group 1 has smallest mean)
  cluster_means <- sapply(1:k, function(i) mean(x[labels == i]))
  mean_order <- order(cluster_means)
  
  # Create mapping from old labels to ordered labels
  label_mapping <- integer(k)
  for (i in 1:k) {
    label_mapping[mean_order[i]] <- i
  }
  
  # Apply the new ordered labels
  ordered_labels <- label_mapping[labels]
  
  return(ordered_labels)
}