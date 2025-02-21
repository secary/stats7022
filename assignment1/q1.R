# Library packages and load data
pacman::p_load(tidyverse, ggplot2, dplyr)
data(mpg, package ="ggplot2")

# Calculate the mean efficiency
mean_eff <- mpg %>% 
  group_by(class) %>% 
  summarise(cty = mean(cty),
            hwy = mean(hwy))  

# Sort by city fuel efficiency and convert the tibble to long fourm
mean_eff <- mean_eff %>% 
  arrange(cty) %>% 
  mutate(class = factor(class, levels = class)) %>% 
  gather(key = "measure", value = "mean", cty, hwy)

# Plot
ggplot(mean_eff, aes(x = class, y = mean, fill = measure)) +
  geom_col(position = "dodge", color = "black", size = 0.5) +
  # Add title and axis title
  labs(
    title = "Column plot of mean fuel efficiency",
    x = "Vehicle class",
    y = "Mean fuel efficiency (miles per gallon)"
  ) +
  # Set filling colours and themes
  scale_fill_manual(values = c("cty" = "navy", "hwy" = "salmon")) +
  theme_bw() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "bottom"
  ) 

