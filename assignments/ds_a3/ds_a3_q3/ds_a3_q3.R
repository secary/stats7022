# Function to implement k-means algorithm
#
# INPUT:
# x: numeric vector
# k: number of groups
#
# OUTPUT
# A vector of integer labels indicating group membership.
get_kmeans <- function(x, k) {
  # Check inputs
  if (!is.numeric(x)) {
    stop("Input x must be a numeric vector.")
  }
  if (!is.numeric(k) || length(k) != 1 || k != as.integer(k) || k < 1) {
    stop("Input k must be a single positive integer.")
  }
  if (length(unique(x)) < k) {
    stop("x must have at least k unique values.")
  }
  
  # Run k-means clustering
  km <- kmeans(x, centers = k)
  
  # Compute mean of each cluster
  cluster_means <- tapply(x, km$cluster, mean)
  
  # Order clusters by mean
  ordered_clusters <- order(cluster_means)
  
  # Create a mapping from old cluster number to new label
  new_labels <- match(km$cluster, ordered_clusters)
  
  return(new_labels)
}
  
