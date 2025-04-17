pacman::p_load(tidyverse, tidymodels, kernlab)

# a
a3 <- read_rds('./data/a3_data.rds')
a3

# b
skimr::skim(a3)

# c_i
a3 <- a3 %>% 
  mutate(X1 = as.numeric(X1)) 

skimr::skim(a3)

# c_ii
a3 %>% 
  filter(X2 < 10 * -0.950)
a3 %>% 
  filter(X4 > 10 * 0.691)

a3 <- a3 %>% 
  mutate(X2 = case_when(X2 >= 10 * -0.950 ~ X2,
                        TRUE ~ NA)) %>% 
  mutate(X4 = case_when(X4 <= 10 * 0.691 ~ X4, 
                        TRUE ~ NA))

# c_iii
levels(a3$C1)
levels(a3$C2)

a3 <- a3 %>% 
  mutate(Y = case_when(Y %in% c("A", "B") ~ Y,
                       TRUE ~ NA)) %>% 
  mutate(C2 = case_when(C2 %in% c("T", "U", "V") ~ C2,
                        TRUE ~ NA)) %>% 
  mutate(Y = factor(Y, levels = c("A", "B"))) %>% 
  mutate(C2 = factor(C2, levels = c("T", "U", "V")))

# c_iv
a3 <- a3 %>% 
  na.omit() 
  
a3
skimr::skim(a3)

# d
a3_recipe <- recipe(Y ~ ., data = a3) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())
a3_recipe

# e
m1 <- svm_poly(mode = "classification", degree = 3) %>% 
  set_engine("kernlab") 
  
m2 <- svm_rbf(mode = "classification") %>% 
  set_engine("kernlab")

# f
a3_wf1 <- workflow() %>% 
  add_recipe(a3_recipe) %>% 
  add_model(m1)

a3_wf2 <- workflow() %>% 
  add_recipe(a3_recipe) %>% 
  add_model(m2)

# g
a3_poly <- a3_wf1 %>% 
  fit(a3)
a3_poly

a3_radial <- a3_wf2 %>% 
  fit(a3)
a3_radial

a3_pred <- a3 %>% 
  bind_cols(
    predict(a3_poly, a3),
    predict(a3_poly, a3, type = "prob")
    ) %>% 
  mutate(type = "SVM Poly") %>% 
  bind_rows(
    a3 %>% 
      bind_cols(
        predict(a3_radial, a3),
        predict(a3_radial, a3, type = "prob")
        ) %>% 
      mutate(type = "SVM Radial")
  )
a3_pred

# h
# i
a3_pred %>% 
  group_by(type) %>% 
  accuracy(truth = Y, estimate = .pred_class)

# ii
a3_pred %>% 
  group_by(type) %>% 
  sens(truth = Y, estimate = .pred_class)

# iii
a3_pred %>% 
  group_by(type) %>% 
  spec(truth = Y, estimate = .pred_class)

# i
a3_pred %>% 
  group_by(type) %>% 
  roc_curve(Y,
            .pred_A) %>% 
  autoplot()

a3_pred %>% 
  group_by(type) %>% 
  roc_auc(Y, .pred_A)

acc <- a3_pred %>%
  group_by(type) %>%
  accuracy(truth = Y, estimate = .pred_class)

sen <- a3_pred %>%
  group_by(type) %>%
  sens(truth = Y, estimate = .pred_class)

spe <- a3_pred %>%
  group_by(type) %>%
  spec(truth = Y, estimate = .pred_class)

auc <- a3_pred %>%
  group_by(type) %>%
  roc_auc(truth = Y, .pred_A)

a3_metrics <- acc %>%
  select(type, .metric, .estimate) %>%
  bind_rows(
    sen %>% select(type, .metric, .estimate),
    spe %>% select(type, .metric, .estimate),
    auc %>% select(type, .metric, .estimate)
  ) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)
a3_metrics
    

# k
new_data <- data.frame(
  "X1" = -0.11,
  "X2" = 1.19,
  "X3" = -1.37,
  "X4" = -0.46,
  "C1" = "y",
  "C2" = "T"
)
predict(a3_poly, new_data)
