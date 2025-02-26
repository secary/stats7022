pacman::p_load(tibble, dplyr, ggplot2)
# Function to return ROC
# INPUT
# Two vectors:
# obs: a factor with two levels, "A" and "B"
# A: the predicted probability that each observation is "A"
# OUTPUT
# A tibble with 3 columns: threshold, specificity, and
# sensitivity.
get_ROC <- function(obs, A) {
  # YOUR CODE HERE
  obs <- factor(obs, levels = c("B", "A"))
  
  thresholds <- c(-Inf, sort(unique(A)), Inf)
  
  results <- lapply(thresholds, function(thresh){
    pred <- ifelse(A >= thresh, "A", "B")
    
    TP <- sum(pred == "A" & obs == "A")
    TN <- sum(pred == "B" & obs == "B")
    FP <- sum(pred == "A" & obs == "B")
    FN <- sum(pred == "B" & obs == "A")
    
    sen <- TP/(TP + FN)
    spe <- TN/(TN + FP)
    
    tibble(threshold = thresh, specificity = spe, sensitivity = sen)
  })
  
  return(bind_rows(results))
}