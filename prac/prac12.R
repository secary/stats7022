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
