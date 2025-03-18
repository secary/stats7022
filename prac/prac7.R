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
          