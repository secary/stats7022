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

