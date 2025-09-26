
library(dslabs)
library(tidyverse)
data("heights")
data()
# MISSING CODE



heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarise(p = mean(sex == "Male")) %>%
qplot(height, p, data =.)


ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarise(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)


Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)


library(tidyverse)
library(caret)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) |>
  data.frame() |> setNames(c("x", "y"))

test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)


#dat <- replicate(2, MASS::mvrnorm(n = 100, c(69, 69), Sigma) |>
#  data.frame() |> setNames(c("x", "y")))



