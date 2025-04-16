pacman::p_load(tidyverse)
pacman::p_load(gapminder, broom)
data(gapminder)
gapminder

gapminder_nest <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()
gapminder_nest

gapminder_nest$data[[1]]

fit_lm <- function(df){
  lm(lifeExp ~ year, data = df)
}

standardize <- function(df) {
  df %>% mutate(year = as.numeric(scale(year)))
}

gapminder_nest <- gapminder_nest %>% 
  mutate(data = map(data, standardize))

gapminder_nest <- gapminder_nest %>% 
  mutate(model = map(data, 
                     fit_lm))
gapminder_nest$model[[1]]

gapminder_nest <- gapminder_nest %>% 
  mutate(residuals = map(model,
                         broom::augment))
gapminder_nest %>% 
  unnest(residuals)

gapminder_nest <- gapminder_nest %>% 
  mutate(tidy_model = map(model, broom::tidy))

gapminder_coef <-  gapminder_nest %>% 
  unnest(tidy_model) %>% 
  select(country, continent, term, estimate) %>% 
  pivot_wider(names_from = term, values_from = estimate)

ggplot(gapminder_coef, aes(x = `(Intercept)`, 
                           y = year,
                           color = continent,
                           shape = continent)) +
  geom_point(size = 3) +
  labs(x = "intercept", y = "slope") +
  theme_minimal()
