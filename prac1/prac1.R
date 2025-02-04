pacman::p_load(tidyverse, tidymodels, vip)

data(mpg, package = "ggplot2")
mpg

# Clean the Data
mpg <- mpg %>% 
  select(cty, displ, drv)
mpg

mpg <- mpg %>% 
  mutate(
    drv = factor(drv)
  )
mpg

# EDA
skimr::skim_without_charts(mpg)

mpg %>% 
  ggplot(aes(cty)) + 
  geom_histogram(col = "black", fill = "#3fbf7f")

# Initial Split
set.seed(20251)
mpg_split <- initial_split(mpg, strata = cty)
mpg_train <- training(mpg_split)
mpg_test <- testing(mpg_split)
mpg_train


# Pew-processing
mpg_recipe <- recipe(cty ~ ., data = mpg_train) %>% 
  step_dummy(all_nominal_predictors())
mpg_recipe %>% prep() %>% bake(new_data=NULL)

# Set Up the Model
mpg_model <- linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine("lm")

# Set Up the Workflow
mpg_wf <- workflow() %>% 
  add_model(mpg_model) %>% 
  add_recipe(mpg_recipe)
mpg_wf

# Fit the Model
mpg_fit <- mpg_wf %>% 
  fit(mpg_train)
mpg_fit

mpg_fit %>% tidy()

mpg_fit %>% glance()

# Assumption Checking
mpg_lm <- mpg_fit %>% 
  extract_fit_parsnip() %>% 
  pluck("fit") 

plot(mpg_lm)
gglm::gglm(mpg_lm)

# Prediction
new_data <- tibble(
  displ = 3, 
  drv = "4"
)
predict(mpg_fit, new_data = new_data)
predict(mpg_fit, new_data = new_data, type = "pred_int")

# Question 8
q8 <- tibble(
  displ = 2,
  drv = 'f'
)
predict(mpg_fit, new_data = q8)

# Variable Importance Plots (VIP)
mpg_fit %>% extract_fit_parsnip() %>% vip()
mpg_fit %>% extract_fit_parsnip() %>% vi()

# Assess the Model Fit Using the Test Data
metrics <- metric_set(rmse, rsq)
mpg_metrics <- mpg_test %>%
  add_column(
    predict(mpg_fit, mpg_test)
  ) %>% 
  metrics(
    truth = cty, estimate = .pred
  )
mpg_metrics