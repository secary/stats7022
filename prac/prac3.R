pacman::p_load(tidyverse)
data(mpg, package = "ggplot2")

mpg <- mpg %>%
  select(cty, displ, drv)
mpg

mpg %>%
  count(displ)

mpg <- mpg %>%
  mutate(drv = factor(drv))

mpg %>% 
  ggplot(aes(cty)) + 
  geom_histogram(col = "black", fill = "#9f4fff")
moments::skewness(mpg$cty)

skimr::skim_without_charts(mpg)

mpg %>% 
  ggplot(aes(displ)) + 
  geom_histogram(col = "black", fill = "#9f4fff")
moments::skewness(mpg$displ)

mpg %>%
  count(drv)

mpg %>%
  ggplot(aes(x = displ, y = cty)) +
  geom_point() +
  labs(x = "Displacement (litres)",
       y = "City Fuel Efficiency (miles per gallon)"
  ) +
  ggtitle(
    "Scatterplot of City Fuel Efficiency Against Displacement\nfor the mpg Dataset"
  )+
  geom_smooth()

mpg %>%
  ggplot(aes(x = drv, y = cty, fill = drv))+
    geom_boxplot()
