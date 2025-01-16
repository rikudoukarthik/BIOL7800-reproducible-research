1:10 
sum(1:10)


print("To git or not to git")


library(tidyverse)
ggplot(iris) +
  geom_point(mapping = aes(x = Species, y = Sepal.Length, col = Species), 
             alpha = 0.75, position = position_jitter(height = 0, width = 0.25)) +
  theme_classic()


set.seed(123)
sample(c("apples", "bananas", "dacoconutnutisagiantnut"), 5, TRUE)

