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
