# generate the Monte Carlo simulation
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))

# calculate correlation between X,Y for each group
res <- sim_data %>% 
  group_by(group) %>% 
  summarize(r = cor(x, y)) %>% 
  arrange(desc(r))
res

# plot points from the group with maximum correlation
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_smooth(method = "lm")

# histogram of correlation in Monte Carlo simulations
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, colour = "black")

# linear regression on group with maximum correlation
library(broom)
sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  summarise(tidy(lm(y ~ x)))


# generate the Monte Carlo simulation
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))

# calculate correlation between X,Y for each group
res <- sim_data %>% 
  group_by(group) %>% 
  summarize(r = cor(x, y)) %>% 
  arrange(desc(r))
res

# plot points from the group with maximum correlation
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_smooth(method = "lm")
  
# histogram of correlation in Monte Carlo simulations
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")

# linear regression on group with maximum correlation
library(broom)
sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  summarize(tidy(lm(y ~ x)))

library(dslabs)
data("research_funding_rates")
research_funding_rates

summary <- research_funding_rates |> summarise (gender = c("m", "f"), 
                                     applied = c(sum(applications_men), sum(applications_women)), 
                                     awarded = c(sum(awards_men), sum(awards_women)), 
                                     not_awarded = applied-awarded, 
                                     prop_awarded = awarded/applied)


two_by_two <- data.frame(awarded = c("no", "yes"),
                         men = c(sum(research_funding_rates$applications_men)-sum(research_funding_rates$awards_men),
                                 sum(research_funding_rates$awards_men)), 
                         women = c(sum(research_funding_rates$applications_women)-sum(research_funding_rates$awards_women),
                                   sum(research_funding_rates$awards_women)))

chisq_test <- two_by_two |> select(-awarded) |> chisq.test()

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  pivot_longer(-discipline) %>%
  separate(name, c("type", "gender")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  filter(gender == "total")
dat |> ggplot(aes(x=discipline, y=success, colour = gender)) + geom_point(size=3)


