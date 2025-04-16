pacman::p_load(tidyverse, tidymodels)

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
  mutate(C2 = case_when(C2 %in% c("T", "U", "V") ~ fct_recode(as.character(C2)),
                        TRUE ~ NA)) 

# c_iv
a3 <- a3 %>% 
  na.omit()
a3

# d
a3_recipe <- recipe(Y ~ ., data = a3) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

# e

  