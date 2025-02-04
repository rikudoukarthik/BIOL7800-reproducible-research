library(tidyverse)

diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(col = "yellow", size = 0.5) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))
