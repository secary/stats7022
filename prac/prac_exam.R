pacman::p_load(tidymodels, tidyverse, ggplot2, vip)

df <- readRDS('./data/sample_ex.rds')
df

ggplot(df, aes(Y))+
  geom_histogram()

skimr::skim(df)

df <- df %>% 
  mutate(Y= log(Y)) %>% 
  mutate(X2 = ifelse(X2 < -10, NA, X2)) %>% 
  mutate(X3 = as.numeric(X3)) %>% 
  mutate(C1 = ifelse(C1 %in% c("a", "b", "c", "d"), C1, NA)) %>% 
  mutate(C2 = ifelse(C2 %in% c("W", "X", "Y", "Z"), C2, NA))


skimr::skim(df)

mean(df$Y)
mean(df$X2, na.rm=TRUE)
mean(df$X3)

df <- df %>% na.omit()
df

set.seed(20251)
df_split <- initial_split(df, strata = Y) 
df_split

df_train <- training(df_split)
df_test <- testing(df_split)

df_cv <- vfold_cv(df_train, v = 20, strata = Y)
df_cv

df_recipe <- recipe(Y ~ ., data = df_train) %>%
  step_BoxCox(X4) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_interact(~ X1:X2) %>% 
  step_dummy(all_nominal_predictors())
df_recipe %>% prep() %>% bake(new_data = NULL)

lasso_reg <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

df_wf <- workflow() %>% 
  add_model(lasso_reg) %>% 
  add_recipe(df_recipe)

df_grid <- grid_regular(penalty(), levels = 100)
df_grid[98,]

doParallel::registerDoParallel()
df_reg <- tune_grid(lasso_reg, 
                    df_recipe, 
                    resamples = df_cv, 
                    grid = df_grid)
show_best(df_reg, metric = 'rmse')

best <- select_best(df_reg, metric = 'rmse')

df_wf_fin <- df_wf %>% 
  finalize_workflow(best) 

df_fit <- df_wf_fin %>% last_fit(df_split)
df_fit %>% collect_metrics()

df_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

df_model <- df_wf_fin %>% 
  fit(df_train) 

pred_data <- data.frame(
  X1 = -1,
  X2 = 0.5,
  X3 = 2,
  X4 = 1,
  C1 = factor("c", levels = c("a", "b", "c", "d")),
  C2 = factor("X", levels = c("W", "X", "Y", "Z"))
)

pred_value <- predict(df_model, pred_data)
pred_value
