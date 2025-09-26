library(tidyverse)
#set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mu_x <- mean(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_x <- sd(female_heights$mother)
s_y <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)

female_heights |> 
  ggplot(aes(mother, daughter)) + 
  geom_point(alpha = 0.5) +
  geom_abline(slope = r * s_y/s_x, intercept = mu_y - r * s_y/s_x * mu_x)

test_height <- 60
exp_daughter <- mu_y + ((test_height - mu_x)/s_x) * mu_x

Y0 <- mu_y - r * s_y/s_x * mu_x
slope <- r * s_y/s_x

Y60 <- Y0 + slope * test_height