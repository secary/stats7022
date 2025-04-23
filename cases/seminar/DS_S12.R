pacman::p_load(tidyverse, tidymodels, discrim, magrittr)
data <- read_rds("./seminar/data.rds")

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

new_data <- expand.grid(
  X1 = seq(min(data$X1), max(data$X1), length.out = 200),
  X2 = seq(min(data$X2), max(data$X2), length.out = 200)
)

# visualise the data
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
