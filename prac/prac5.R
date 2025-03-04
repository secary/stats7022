# Load the Data and Libraries
pacman::p_load(tidyverse, tidymodels, ISLR, janitor, glmnet, ggplot2)
data("Hitters", package = "ISLR")
hitters <- as_tibble(Hitters)
hitters

# Introduction of Hitters dataset
# ??ISLR::Hitters

# Cleaning
# Clean Variable Names
hitters <- hitters %>% 
  janitor::clean_names() %>% 
  # Select Numeric Variables
  select(where(is.numeric))
hitters

# Workflows
# Recipe
hitters_recipe <- recipe(salary ~ ., data = hitters) %>% 
  step_naomit(salary) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(all_numeric_predictors())
design_matrix <- as_tibble(hitters_recipe %>% prep() %>% bake(new_data=NULL))

# Models
# penalty \lambda = 1
hitters_M1 <- linear_reg(mixture = 0, penalty = 1) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

# Create Workflows
hitters_WF1 <- workflow() %>% 
  add_recipe(hitters_recipe) %>% 
  add_model(hitters_M1)

# Fit Models
hitters_ridge_fit1 <- hitters_WF1 %>% 
  fit(hitters)

fit1_coefficients <- as_tibble(hitters_ridge_fit1 %>% tidy()) 
fit1_coefficients

# penalty \lambda = 1000
hitters_M1000 <- linear_reg(mixture = 0, penalty = 1000) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

hitters_WF1000 <- workflow() %>% 
  add_recipe(hitters_recipe) %>% 
  add_model(hitters_M1000)

hitters_ridge_fit1000 <- hitters_WF1000 %>% 
  fit(hitters)

fit1000_coefficients <- as_tibble(hitters_ridge_fit1000 %>% tidy()) 
fit1000_coefficients

# Visualising the Coefficients
get_coef <- function(penalties, data, recipe){
  coef <- list()
  model <-
    linear_reg(mixture = 0, penalty = i) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
  WF <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(model)
  for(i in penalties){
    coef[[i]] <- WF %>% fit(data) %>% tidy()
  }
  coef <- bind_rows(coef)
  return(coef)
}
coefs <- get_coef(c(1, 10, 50, 100, 250, 500, 1000), hitters, hitters_recipe) %>% 
  filter(term != "(Intercept)")

ggplot(coefs, aes(x = penalty, y = estimate, color = term, group = term)) +
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  labs(x = "Penalty",
       y = "Estimate",
       color = "Term")
