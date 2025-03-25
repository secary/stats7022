pacman::p_load(tidyverse, tidymodels, dplyr)
data("penguins", package = "palmerpenguins")

set.seed(20251)
penguin_split <- initial_split(penguins)
penguin_split

penguins_train <- training(penguin_split)
penguins_test <- testing(penguin_split)

penguin_CV <- vfold_cv(penguins_train)
penguin_CV

linear_model <- linear_reg() %>%
  set_engine("lm")

penguin_linear_workflow <- workflow() %>%
  add_model(linear_model) %>%
  add_formula(bill_length_mm ~ body_mass_g)

logistic_model <- logistic_reg() %>%
  set_engine("glm")

penguin_logistic_workflow <- workflow() %>%
  add_model(logistic_model) %>%
  add_formula(sex ~ body_mass_g)

penguin_linear_resamples <- 
  fit_resamples(
    penguin_linear_workflow,
    resamples = penguin_CV
    ) 
penguin_linear_resamples

control = control_resamples(save_pred = TRUE)

penguin_logistic_resamples <-
  fit_resamples(
    penguin_logistic_workflow,
    resamples = penguin_CV,
    control = control_resamples(save_pred = TRUE)
  )

penguin_linear_resamples %>% unnest(.metrics) %>% filter(id == "Fold02")
penguin_linear_resamples %>% collect_metrics()

penguin_linear_workflow %>%
  last_fit(penguin_split) %>%
  collect_metrics()

penguin_logistic_workflow %>%
  last_fit(penguin_split) %>%
  collect_metrics() 

split_preds <- penguin_logistic_resamples %>%
  collect_predictions() 

roc_data <- split_preds %>%
  group_by(id) %>% 
  roc_curve(truth = sex, .pred_female)
roc_data

ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity, color = id)) +
  geom_line() +
  geom_abline(linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(legend.title = element_blank())
