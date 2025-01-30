require(tidyverse)

smaller <- diamonds |> 
  filter(carat <= 2.5)

smaller_plot <- smaller |> 
  ggplot(aes(x = carat)) + 
  geom_freqpoly(binwidth = 0.01)