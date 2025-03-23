# 1 Load Libraries
pacman::p_load(dplyr, tidymodels)

# 2 Load Data
bird_bath <- readRDS("./data/bird_bath.rds")
bird_bath

# 3 Data Cleaning
# 3.1 Remove Missing Years
bird_bath <- bird_bath %>% na.omit(survey_year)
bird_bath

# 3.2 Identify the Top 15 Types of Bird
top_15_birds <- filter(bird_bath, bird_count == 1)%>%
  count(bird_type, sort = TRUE) %>%
  slice_head(n = 15)
top_15_birds

bird_bath <- bird_bath %>%
  filter(bird_type %in% top_15_birds$bird_type)
bird_bath

# 3.3 Create the Response Variable
bird_bath <- bird_bath %>% 
  mutate(bird_present = ifelse(bird_count > 0, "yes", "no"))

# 3.4 Select Columns
bird_bath <- bird_bath %>% 
  select(urban_rural, bird_type, bird_present)
bird_bath

# 3.5 Convert Variables to Categorical
bird_bath <- bird_bath %>% 
  mutate_all(factor)
bird_bath

# 4 Models
# Model 1
bird_bath_recipe1 <- recipe(bird_present ~ urban_rural + bird_type, data = bird_bath) %>%
  step_dummy()

bird_bath_model1 <- logistic_reg() %>% 
  set_mode("classification") %>% 
  set_engine("glm")

bird_bath_wf1 <- workflow() %>% 
  add_recipe(bird_bath_recipe1) %>% 
  add_model(bird_bath_model1)
bird_bath_wf1

# Model2
bird_bath_recipe2 <- recipe(bird_present ~ urban_rural + bird_type, data = bird_bath) %>%
  step_dummy() %>% 
  step_interact(bird_present ~ urban_rural:bird_type)

bird_bath_model2 <- logistic_reg() %>% 
  set_mode("classification") %>% 
  set_engine("glm")

bird_bath_wf2 <- workflow() %>% 
  add_recipe(bird_bath_recipe2) %>% 
  add_model(bird_bath_model2)
bird_bath_wf2

# Predictions
bird_bath_fit1 <- bird_bath_wf1 %>% 
  fit(bird_bath)

bird_bath_fit2 <- bird_bath_wf2 %>% 
  fit(bird_bath)

bird_bath_pred1 <- bird_bath %>%
  bind_cols(predict(bird_bath_fit1, bird_bath),
            predict(bird_bath_fit1, bird_bath, type = "prob")) %>% 
  select(bird_present, starts_with(".pred"))

bird_bath_pred2 <- bird_bath %>%
  bind_cols(predict(bird_bath_fit2, bird_bath),
            predict(bird_bath_fit2, bird_bath, type = "prob")) %>%
  select(bird_present, starts_with(".pred"))

#ROC
roc_1 <- bird_bath_pred1 %>% 
  roc_curve(truth = bird_present, .pred_no) %>% 
  mutate(model = "Model 1")

roc_2 <-  bird_bath_pred2 %>% 
  roc_curve(truth = bird_present, .pred_no) %>% 
  mutate(model = "Model 2")

roc_bind <- bind_rows(roc_1, roc_2)

ggplot(roc_bind, aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    scale_color_manual(values = c("Model 1" = "#F66359", "Model 2" = "#1AC5CA")) +
  labs(color = "model") +
  coord_equal() +
  theme_bw() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = "right"
  )
