pacman::p_load(tidyverse, plotly, patchwork, glue)

data(mpg)

p <- mpg %>% 
  ggplot(aes(displ, cty, col = drv, text = glue::glue("Model: {model}")))+
  geom_point()
ggplotly(p)



mpg %>%
  ggplot(aes(displ, cty)) +
  geom_point() +
  labs(
    x = expression(mu[1] + rhoˆ2),
    y = expression(pi),
    title = expression(
      paste("Plot with ", alpha, " and ", beta)
    )
  ) +
  annotate("text",
           x = 4, y = 30,
           label = expression(
             Y[i] == beta[0] + beta[1] * x[i] + epsilon[i]
           ),
           size = 5)

p1 <- ggplot(mtcars) +
  geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) +
  geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot(mtcars) +
  geom_bar(aes(carb))

(p1 | p2) / p3
