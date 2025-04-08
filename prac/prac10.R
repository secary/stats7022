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

