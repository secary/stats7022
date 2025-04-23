# Sample Practical Exam
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

# Practical 1: Workflows
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
mpg_fit %>% extract_fit_parsnip() %>% vip()

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

# Practical 2: Data Cleaning
pacman::p_load(tidyverse, tidymodels, vip)

rbc0 <- readxl::read_xlsx('./data/rbc_clean.xlsx')
rbc0

inspectdf::inspect_na(rbc0)

colnames(rbc0)[1] <- 'id'
rbc0

rbc0$fitness <- as.double(rbc0$fitness)
inspectdf::inspect_na(rbc0)
rbc1 <- rbc0 %>% filter(complete.cases(.))

rbc1

rbc1 <- rbc1 %>%
  filter(fitness <= 100, RBC > 0, country <= 3)

rbc1 <- mutate(rbc1, sex=ifelse(rbc1$sex=='male', 'M', rbc1$sex))
rbc1

sex_c <- rbc1 %>%
  count(sex) %>%
  arrange(-n)
sex_c

country_c <- rbc1 %>%
  count(country) %>%
  arrange(-n)
country_c

inspectdf::inspect_num(rbc1)

# Practical 3: Exploratory Data Analysis
pacman::p_load(tidyverse)
data(mpg, package = "ggplot2")

mpg <- mpg %>%
  select(cty, displ, drv)
mpg

mpg %>%
  count(displ)

mpg <- mpg %>%
  mutate(drv = factor(drv))

mpg %>% 
  ggplot(aes(cty)) + 
  geom_histogram(col = "black", fill = "#9f4fff")
moments::skewness(mpg$cty)

skimr::skim_without_charts(mpg)

mpg %>% 
  ggplot(aes(displ)) + 
  geom_histogram(col = "black", fill = "#9f4fff")
moments::skewness(mpg$displ)

mpg %>%
  count(drv)

mpg %>%
  ggplot(aes(x = displ, y = cty)) +
  geom_point() +
  labs(x = "Displacement (litres)",
       y = "City Fuel Efficiency (miles per gallon)"
  ) +
  ggtitle(
    "Scatterplot of City Fuel Efficiency Against Displacement\nfor the mpg Dataset"
  )+
  geom_smooth()

mpg %>%
  ggplot(aes(x = drv, y = cty, fill = drv))+
  geom_boxplot()

# Practical 4: Recipes
# Load packages and data
pacman::p_load(recipes, workflows)
data(mpg, package = "ggplot2")
mpg

# Create a Recipe Object
mpg_recipe <- recipe(cty ~ displ + drv, data = mpg)
mpg_recipe

# Quantitative 
mpg_recipe <- mpg_recipe %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())
mpg_recipe

mpg_recipe %>% prep()
mpg_recipe %>% prep() %>% tidy()
mpg_recipe %>% prep() %>% tidy(n = 1)

# Categorical
mpg_recipe <- mpg_recipe %>% 
  step_dummy(all_nominal_predictors())
mpg_recipe
mpg_recipe %>% prep() %>% tidy()
mpg_recipe %>% prep() %>% tidy(n = 3)

mpg_recipe <- mpg_recipe %>%
  step_interact(terms = ~starts_with("drv"):displ)
mpg_recipe
mpg_recipe %>% prep() %>% tidy(n = 4)

# Adding to workflows
mpg_wf <- workflow()
mpg_wf <- mpg_wf %>%
  add_recipe(mpg_recipe)
mpg_wf

# Design Matrix
mpg_recipe %>%
  prep() %>%
  bake(new_data=NULL)

new_data <- tibble(displ = c(1,2),
                   drv = c("f", "r"))
new_data

mpg_recipe %>% prep() %>% bake(new_data)

# Bake the data with new data recipe
new_recipe <- recipe(~ displ, data = new_data) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(all_numeric_predictors())
new_recipe %>% prep() %>% bake(new_data)

# Practical 5: Regression
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

# Practical 6: Classification
pacman::p_load(tidymodels, palmerpenguins, tidyverse, discrim, viridis)
data("penguins", package = "palmerpenguins")
penguins

# LDA
penguin_recipe <- recipe(species ~ bill_length_mm + bill_depth_mm,
                         data = penguins) %>%
  step_impute_mean(all_predictors())
penguin_coef <- penguin_recipe %>% prep() %>% tidy(n = 1)
penguin_coef$value[penguin_coef$terms == "bill_length_mm"]

# Model
penguin_model <- discrim_linear() %>%
  set_mode("classification")
penguin_model

# Create Workflow
penguin_wf <- workflow() %>%
  add_recipe(penguin_recipe) %>%
  add_model(penguin_model)
penguin_wf

# Fit the Model
penguin_fit <- penguin_wf %>%
  fit(penguins)
penguin_fit

# Visualising
new_data <- crossing(
  bill_length_mm = seq(
    from = min(penguins$bill_length_mm, na.rm = TRUE), 
    to = max(penguins$bill_length_mm, na.rm = TRUE),
    length = 500
  ),
  bill_depth_mm = seq(
    from = min(penguins$bill_depth_mm, na.rm = TRUE), 
    to = max(penguins$bill_depth_mm, na.rm = TRUE),
    length = 500
  )
)

new_data %>% 
  add_column(
    predict(penguin_fit, new_data = new_data)
  ) %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, fill = .pred_class)) + 
  geom_raster() + 
  labs(
    fill = "Species", 
    x = "Bill length (mm)", 
    y = "Bill depth (mm)"
  ) + 
  theme_bw() + 
  viridis::scale_fill_viridis(option="D", discrete=TRUE)

# QDA
penguin_qda <- discrim_quad() %>% 
  set_mode("classification")
penguin_qda

penguin_wf2 <- workflow() %>% 
  add_recipe(penguin_recipe) %>% 
  add_model(penguin_qda)
penguin_wf2

penguin_fit2 <- penguin_wf2 %>% 
  fit(penguins)
penguin_fit2

new_data <- crossing(
  bill_length_mm = seq(
    from = min(penguins$bill_length_mm, na.rm = TRUE), 
    to = max(penguins$bill_length_mm, na.rm = TRUE),
    length = 500
  ),
  bill_depth_mm = seq(
    from = min(penguins$bill_depth_mm, na.rm = TRUE), 
    to = max(penguins$bill_depth_mm, na.rm = TRUE),
    length = 500
  )
)

new_data %>% 
  add_column(
    predict(penguin_fit, new_data = new_data)
  ) %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, fill = .pred_class)) + 
  geom_raster() + 
  labs(
    fill = "Species", 
    x = "Bill length (mm)", 
    y = "Bill depth (mm)"
  ) + 
  theme_bw() + 
  viridis::scale_fill_viridis(option="D", discrete=TRUE)

# Practical 7: Yardstick
pacman::p_load(tidyverse, tidymodels, yardstick)
data("penguins", package = "palmerpenguins")
penguins <- penguins %>% na.omit()

penguins_recipe1 <- recipe(flipper_length_mm ~ body_mass_g,
                           data = penguins)

penguins_model1 <- linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine("lm")
penguins_model1

penguins_wf1 <- workflow() %>% 
  add_recipe(penguins_recipe1) %>%
  add_model(penguins_model1)
penguins_wf1  

penguins_fit1 <- penguins_wf1 %>% 
  fit(penguins)
penguins_fit1

penguins_recipe2 <-  recipe(sex ~ body_mass_g, data = penguins)

penguins_model2 <- logistic_reg() %>% 
  set_mode("classification") %>% 
  set_engine("glm")
penguins_model2

penguins_wf2 <- workflow() %>% 
  add_recipe(penguins_recipe2) %>%
  add_model(penguins_model2)
penguins_wf2  

penguins_fit2 <- penguins_wf2 %>% 
  fit(penguins)
penguins_fit2

penguins_pred <- penguins %>%
  bind_cols(predict(penguins_fit1, penguins),
            predict(penguins_fit2, penguins),
            predict(penguins_fit2, penguins, type = "prob")) %>%
  select(sex, flipper_length_mm,starts_with(".pred"))
penguins_pred

penguins_pred %>%
  metrics(truth = flipper_length_mm,
          estimate = .pred)

quantitative_metrics <- metric_set(rmse, mae)
penguins_pred %>%
  quantitative_metrics(truth = flipper_length_mm,
                       estimate = .pred)

penguins_pred %>%
  conf_mat(truth = sex,
           estimate = .pred_class)

penguins_pred %>%
  spec(truth = sex,
       estimate = .pred_class) 

penguins_pred %>%
  sens(truth = sex,
       estimate = .pred_class)

penguins_pred %>%
  roc_curve(truth = sex,
            .pred_female)

penguins_pred %>% 
  roc_curve(truth = sex,
            .pred_female) %>%
  autoplot()

penguins_pred %>% 
  roc_auc(truth = sex,
          .pred_female)

# Practical 8: Cross Validation
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
  yardstick::roc_curve(truth = sex, .pred_female) 

ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity, color = id)) +
  geom_line(size = 1) +
  geom_abline(linetype = "dashed", color = "gray") +
  coord_equal() +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(title = "ROC Curve for Each CV Fold", x = "1 - Specificity", y = "Sensitivity")


# Practical 9: Tuning
pacman::p_load(tidyverse, tidymodels, ranger)
ikea <- read_rds("./data/ikea.rds")

ikea <- ikea %>%
  select(price, name, category, depth, height, width) %>%
  mutate(price = log10(price)) %>%
  mutate(across(where(is.character), factor))

ikea

set.seed(20251)
ikea_split <- initial_split(ikea, strata = price)
ikea_train <- training(ikea_split)
ikea_test <- testing(ikea_split)

ikea_folds <- vfold_cv(ikea_train, v = 15,  strata = price)
ikea_folds

pacman::p_load(textrecipes)
ikea_recipe <- recipe(price ~ ., data = ikea_train) %>%
  step_other(name, category, threshold = 0.01) %>%
  step_impute_mean(depth, height, width)
ikea_recipe %>% prep() %>% bake(new_data=NULL)

ikea_model <- rand_forest(mtry = tune(),
                          min_n = tune(),
                          trees = 500) %>%
  set_mode("regression") %>%
  set_engine("ranger")

ikea_model

ikea_wf <- workflow() %>%
  add_recipe(ikea_recipe) %>%
  add_model(ikea_model)
ikea_wf

ikea_grid <- grid_regular(mtry(c(1,5)),
                          min_n(),
                          levels = 5)
ikea_grid

doParallel::registerDoParallel()
ikea_tune <- tune_grid(ikea_wf,
                       resamples = ikea_folds,
                       grid = ikea_grid)
write_rds(ikea_tune,"./data/ikea_tune.rds")
# ikea_tune <- read_rds("ikea_tune.rds")

ikea_tune %>% autoplot()

show_best(ikea_tune, metric = "rmse")

ikea_wf <- ikea_wf %>%
  finalize_workflow(select_best(ikea_tune, metric = "rmse"))
ikea_wf

ikea_fit <- ikea_wf %>% last_fit(split = ikea_split)
ikea_fit %>% collect_metrics()

ikea_fit %>% collect_predictions() %>%
  ggplot(aes(price, .pred)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1)

# Practical 10: Interpretation
pacman::p_load(tidyverse, tidymodels, palmerpenguins, vip)
data("penguins", package = "palmerpenguins")
penguins


penguins_recipe <- recipe(sex ~ .,
                          data = penguins) %>%
  step_naomit(sex) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())
penguins_recipe %>% prep() %>% bake(new_data = NULL)


penguins_prep <- penguins_recipe %>% prep()
tidy(penguins_prep, number = 2) 
tidy(penguins_prep, number = 3)

penguins_M1 <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

penguins_M2 <- decision_tree(tree_depth = 4) %>%
  set_engine("rpart") %>%
  set_mode("classification")

penguins_WF1 <- workflow() %>%
  add_recipe(penguins_recipe) %>%
  add_model(penguins_M1)

penguins_WF2 <- workflow() %>%
  add_recipe(penguins_recipe) %>%
  add_model(penguins_M2)

penguins_fit1 <- penguins_WF1 %>% fit(penguins)
penguins_fit2 <- penguins_WF2 %>% fit(penguins)

penguins_fit1 %>% tidy()
penguins_fit2

penguins_fit1 %>% extract_fit_parsnip() %>% vip()
penguins_fit2 %>% extract_fit_parsnip() %>% vip()

pacman::p_load(DALEXtra)

pred <- penguins %>%
  select(-sex)
response <- as.integer(penguins$sex)

penguins_explain1 <- explain_tidymodels(penguins_fit1,
                                        data = pred,
                                        y = response,
                                        verbose = FALSE)

penguins_profile1 <- model_profile(penguins_explain1,
                                   variables = "bill_length_mm",
                                   N = NULL,
                                   groups = "species")
plot(penguins_profile1)
as.data.frame(penguins_profile1$agr_profiles) %>%
  mutate(bill_length_mm = `_x_`,
         prob = `_yhat_`,
         species = `_groups_`) %>%
  ggplot(aes(bill_length_mm, prob, col = species)) +
  geom_line() +
  labs(x = "Bill length (mm)",
       y = "Predicted probability of male",
       title = "Partial Dependence Plot for Penguins Data",
       subtitle = "Prediction of penguin sex from a logistic regression model") +
  viridis::scale_color_viridis(option = "D", begin = 0, end = 3/4, discrete = TRUE)


penguins_explain2 <- explain_tidymodels(penguins_fit2,
                                        data = pred,
                                        y = response,
                                        verbose = FALSE)

penguins_profile2 <- model_profile(penguins_explain2,
                                   variables = "bill_depth_mm",
                                   N = NULL,
                                   groups = "island")
plot(penguins_profile2)
as.data.frame(penguins_profile2$agr_profiles) %>%
  mutate(bill_depth_mm = `_x_`,
         prob = `_yhat_`,
         island = `_groups_`) %>%
  ggplot(aes(bill_depth_mm, prob, col = island)) +
  geom_line() +
  labs(x = "Bill depth (mm)",
       y = "Predicted probability of male",
       title = "Partial Dependence Plot for Penguins Data",
       subtitle = "Prediction of penguin sex from a logistic regression model") +
  viridis::scale_color_viridis(option = "D", begin = 0, end = 3/4, discrete = TRUE)

# Practical 12: Workflowsets
pacman::p_load(tidyverse, tidymodels, discrim, naivebayes, LiblineaR, magrittr)
star_trek <- read_rds("./data/star_trek.rds")

star_trek %>% 
  filter(char == "Picard")

star_trek %<>%
  distinct(value_id, .keep_all = TRUE) %>%
  select(char_type, interaction)
star_trek

set.seed(2025)
star_trek_split <- star_trek %>%
  initial_split(prop = 0.8, strata = char_type)
star_trek_train <- training(star_trek_split)
star_trek_test <- testing(star_trek_split)

star_trek_folds <- bootstraps(star_trek_train, strata = char_type)
star_trek_folds

pacman::p_load(textrecipes, stopwords)
basic_recipe <- recipe(char_type ~ interaction,
                       data = star_trek_train) %>%
  step_tokenize(interaction) %>%
  step_tokenfilter(interaction,
                   max_tokens = 80) %>%
  step_tfidf(interaction) %>%
  step_normalize(all_predictors())

stop_recipe <- recipe(char_type ~ interaction,
                      data = star_trek_train) %>%
  step_tokenize(interaction) %>%
  step_stopwords(interaction) %>%
  step_tokenfilter(interaction,
                   max_tokens = 80) %>%
  step_tfidf(interaction) %>%
  step_normalize(all_predictors())

basic_recipe %>% prep() %>% bake(new_data = NULL)
stop_recipe %>% prep() %>% bake(new_data = NULL)

NB_model <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")

SVM_model <- svm_linear() %>%
  set_mode("classification") %>%
  set_engine("LiblineaR")

star_trek_WFS <- workflow_set(
  preproc = list(basic = basic_recipe,
                 stop = stop_recipe),
  models = list(NB = NB_model,
                SVM = SVM_model),
  cross = TRUE)
star_trek_WFS

doParallel::registerDoParallel()

star_trek_rs <- star_trek_WFS %>%
  workflow_map("fit_resamples",
               resamples = star_trek_folds,
               metrics = metric_set(accuracy,
                                    sensitivity,
                                    specificity))

autoplot(star_trek_rs)
autoplot(star_trek_rs) + ylim(c(0,1))

rank_results(star_trek_rs) %>%
  filter(.metric == "accuracy")

star_trek_final_WF <- workflow(stop_recipe, SVM_model)
star_trek_final_fit <- star_trek_final_WF %>%
  fit(star_trek_train)

DS_statement <- tibble(
  person = c("Max",
             "Jono"),
  interaction = c("Data science involves models.",
                  "Enter the data."))
DS_statement %>%
  add_column(star_trek_final_fit %>%
               predict(new_data = DS_statement))

# Seminar 12 Model selection
pacman::p_load(tidyverse, tidymodels, discrim, magrittr)
data <- read_rds("./data/data.rds")

# fit the model
lm_1 <- lm(X2 ~ X1*X3, data = data)

# fit with tidymodels
recipe_lm <- recipe(X2 ~ ., data = data) %>%
  step_interact(terms = ~ X1:X3)
recipe_lm %>% prep() %>% bake(new_data = NULL)

model_lm <- linear_reg() %>% 
  set_engine("lm")
workflow_lm <- workflow() %>%
  add_recipe(recipe_lm) %>%
  add_model(model_lm)
fit_lm <- workflow_lm %>%
  fit(data)
fit_lm %>% tidy()

# model summary
summary(lm_1)

# tidymodels summary
fit_lm %>% extract_fit_parsnip() %>% pluck("fit") %>% summary()

metrics <- metric_set(rmse,rsq)
(metrics_lm <- data %>% 
    add_column(predict(fit_lm,data)) %>% 
    metrics(truth = X2, estimate = .pred))

# visualise the data
data %>%
  ggplot(aes(x = X1, y = X2, colour = X3)) +
  geom_point() +
  geom_smooth(method="lm")

# check the assumptions
plot(lm_1)

# SVM
# fit the model
svm_r <- recipe(X3 ~., data = data)
svm_m <- svm_rbf() %>%
  set_mode("classification") %>%
  set_engine("kernlab")
svm_wf <- workflow() %>%
  add_recipe(svm_r) %>%
  add_model(svm_m)
svm_fit <- svm_wf %>%
  fit(data)

# model summary
svm_fit %>% extract_fit_parsnip() %>% pluck("fit")

# visualise the data
new_data <- expand.grid(
  X1 = seq(min(data$X1), max(data$X1), length.out = 200),
  X2 = seq(min(data$X2), max(data$X2), length.out = 200)
)

new_data  %>%
  add_column(predict(svm_fit, new_data = new_data)) %>%
  ggplot(aes(x=X1,y=X2,fill=.pred_class)) +
  geom_tile() +
  labs(fill = "X3",
       x = "X1",
       y = "X2") +
  theme_bw() +
  viridis::scale_fill_viridis(option="G",
                              begin = 1/6,
                              end = 5/6,
                              discrete=TRUE)

# measure performance
svm_pred <- data %>%
  bind_cols(predict(svm_fit, data),
            predict(svm_fit, data, type = "prob"))
svm_pred %>% conf_mat(truth = X3,
                      estimate = .pred_class)

svm_pred %>% yardstick::roc_curve(truth = X3,
                                  .pred_0) %>%
  autoplot()

# LDA
# fit the model
lda_r1 <- recipe(X3 ~ ., data = data)
lda_m1 <- discrim_linear() %>%
  set_mode("classification") %>%
  set_engine("MASS")
lda_wf1 <- workflow() %>%
  add_recipe(lda_r1) %>%
  add_model(lda_m1)
lda_fit1 <- lda_wf1 %>%
  fit(data)

# model summary
lda_fit1 %>% extract_fit_parsnip() %>% pluck("fit")

# visualise the data
new_data %>%
  add_column(predict(lda_fit1, new_data = new_data)) %>%
  ggplot(aes(x=X1,y=X2,fill=.pred_class)) +
  geom_tile() +
  labs(fill = "X3",
       x = "X1",
       y = "X2") +
  theme_bw() +
  viridis::scale_fill_viridis(option="G",
                              begin = 1/6,
                              end = 5/6,
                              discrete=TRUE)

# measure performance
lda_pred1 <- data %>%
  bind_cols(predict(lda_fit1, data),
            predict(lda_fit1, data, type = "prob"))
lda_pred1 %>% conf_mat(truth = X3,
                       estimate = .pred_class)

lda_pred1 %>% yardstick::roc_curve(truth = X3,
                                   .pred_0) %>%
  autoplot()

# LDA With Interaction Term
# fit the model
lda_r2 <- recipe(X3 ~ ., data = data) %>%
  step_interact(terms = ~ X1:X2)
lda_m2 <- discrim_linear() %>%
  set_mode("classification") %>%
  set_engine("MASS")
lda_wf2 <- workflow() %>%
  add_recipe(lda_r2) %>%
  add_model(lda_m2)
lda_fit2 <- lda_wf2 %>%
  fit(data)

# model summary
lda_fit2 %>% extract_fit_parsnip() %>% pluck("fit")

# visualise the data
new_data %>%
  add_column(predict(lda_fit2, new_data = new_data)) %>%
  ggplot(aes(x=X1,y=X2,fill=.pred_class)) +
  geom_tile() +
  labs(fill = "X3",
       x = "X1",
       y = "X2") +
  theme_bw() +
  viridis::scale_fill_viridis(option="G",
                              begin = 1/6,
                              end = 5/6,
                              discrete=TRUE)

# measure performance
lda_pred2 <- data %>%
  bind_cols(predict(lda_fit2, data),
            predict(lda_fit2, data, type = "prob"))
lda_pred2 %>% conf_mat(truth = X3,
                       estimate = .pred_class)

lda_pred2 %>% yardstick::roc_curve(truth = X3,
                                   .pred_0) %>%
  autoplot()

# QDA
# fit the model
qda_r <- recipe(X3 ~ ., data = data)
qda_m <- discrim_quad() %>%
  set_mode("classification") %>%
  set_engine("MASS")
qda_wf <- workflow() %>%
  add_recipe(qda_r) %>%
  add_model(qda_m)
qda_fit <- qda_wf %>%
  fit(data)

# model summary
qda_fit %>% extract_fit_parsnip() %>% pluck("fit")

# visualise the data
new_data %>%
  add_column(predict(qda_fit, new_data = new_data)) %>%
  ggplot(aes(x=X1,y=X2,fill=.pred_class)) +
  geom_tile() +
  labs(fill = "X3",
       x = "X1",
       y = "X2") +
  theme_bw() +
  viridis::scale_fill_viridis(option="G",
                              begin = 1/6,
                              end = 5/6,
                              discrete=TRUE)

# measure performance
qda_pred <- data %>%
  bind_cols(predict(qda_fit, data),
            predict(qda_fit, data, type = "prob"))
qda_pred %>% conf_mat(truth = X3,
                      estimate = .pred_class)

qda_pred %>% yardstick::roc_curve(truth = X3,
                                  .pred_0) %>%
  autoplot()

# Logistic Regression
# fit the model
lr_r <- recipe(X3 ~ ., data = data)
lr_m <- logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")
lr_wf <- workflow() %>%
  add_recipe(lr_r) %>%
  add_model(lr_m)
lr_fit <- lr_wf %>%
  fit(data)

# model summary
lr_fit %>% extract_fit_parsnip() %>% pluck("fit")

# visualise the data
new_data %>%
  add_column(predict(lr_fit, new_data = new_data)) %>%
  ggplot(aes(x=X1,y=X2,fill=.pred_class)) +
  geom_tile() +
  labs(fill = "X3",
       x = "X1",
       y = "X2") +
  theme_bw() +
  viridis::scale_fill_viridis(option="G",
                              begin = 1/6,
                              end = 5/6,
                              discrete=TRUE)

# measure performance
lr_pred <- data %>%
  bind_cols(predict(lr_fit, data),
            predict(lr_fit, data, type = "prob"))
lr_pred %>% conf_mat(truth = X3,
                     estimate = .pred_class)

lr_pred %>% yardstick::roc_curve(truth = X3,
                                 .pred_0) %>%
  autoplot()

#  Comparing Model Performance
# get predictions
preds <- rbind(lda_pred1 %>% add_column(model = "LDA +"),
               lda_pred2 %>% add_column(model = "LDA x"),
               qda_pred %>% add_column(model = "QDA"),
               svm_pred %>% add_column(model = "SVM (RBF)"),
               lr_pred %>% add_column(model = "Logsitic Reg"))


# plot ROC curves
roc_df <- preds %>%
  group_by(model) %>%
  group_map(~ yardstick::roc_curve(.x, truth = X3, .pred_0) %>% mutate(model = .y$model)) %>%
  bind_rows()

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_path(size = 1) +
  viridis::scale_color_viridis(option = "D", begin = 1/6, end = 5/6, discrete = TRUE) +
  geom_abline(lty = 2) +
  coord_equal() +
  theme_bw()

# auc
preds %>%
  group_by(model) %>%
  group_map(~ yardstick::roc_auc(.x, truth = X3, .pred_0) %>%
              mutate(model = .y$model)) %>%
  bind_rows()

#  Which Model Should We Choose?
# assumption checking for LDA/QDA

## group 0
### X1 qqplot
qqnorm(data$X1[data$X3==0])
qqline(data$X1[data$X3==0])

### X2 qqplot
qqnorm(data$X2[data$X3==0])
qqline(data$X2[data$X3==0])

### variances and correlation
var(data$X1[data$X3==0])

var(data$X2[data$X3==0])

cor(data$X1[data$X3==0],data$X2[data$X3==0])

## group 1
### X1 qqplot
qqnorm(data$X1[data$X3==1])
qqline(data$X1[data$X3==1])

### X2 qqplot
qqnorm(data$X2[data$X3==1])
qqline(data$X2[data$X3==1])

### variances and correlation
var(data$X1[data$X3==1])

var(data$X2[data$X3==1])

cor(data$X1[data$X3==1],data$X2[data$X3==1])

