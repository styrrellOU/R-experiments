library(tidyverse)
library(Lahman)
data(Teams)


# stratify by HR
dat <- Teams |> filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) |>
  select(HR, BB, R) |>
  filter(HR >= 0.4 & HR<=1.2)

# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

annual <- Teams |> filter (yearID %in% 1961:2018) |> 
  mutate(HR = HR/G, 
         BB = BB/G,
         R = R/G) |>
  select (yearID, HR, BB, R) |>
  group_by(yearID) |> 
  do(tidy(lm(R ~ BB + HR, data = _), conf.int = T)) |>
  ungroup()

#### it's easier than I was thinking !

res <- Teams |>
  filter(yearID %in% 1961:2018) |>
  group_by(yearID) |>
  do(tidy(lm(R ~ BB + HR, data = .))) |>
  ungroup() 

res |> filter(term == "BB") |>
  group_by(yearID) |>
  lm(estimate ~ yearID, data = _) |> 
  tidy()

### But I still don't completely understand ! 







columns <- colnames(annualTotals)
x <- tibble()
for (i in 1961:2018) {
  ## x <- rbind (x, annualTotals |> filter (yearID == i))
  tmp <- annualTotals |> filter (yearID == i)
  fit[i] = tidy(lm(RpG ~ BBpG + HRpG, data = tmp), conf.int = T)
}

|>
  do(fit = tidy(lm(RpG ~ BBpG, data = _)))

|>
  mutate(fit = tidy(lm(RpG ~ BBpG + HRpG), conf.int = T))

fit -> tidy(lm(RpG ~ BBpG + HRpG, data = annualTotals)) 
fit
  



# use lm to get estimated slopes - lm does not work with grouped tibbles
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

# include the lm inside a summarize and it will work
dat %>%  
  group_by(HR) %>%
  summarize(slope = lm(R ~ BB)$coef[2])

# tidy function from broom returns estimates in and information in a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)

# add confidence intervals
tidy(fit, conf.int = TRUE)

# combine with group_by and summarize to get the table we want
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE))

# it's a data frame so we can filter and select the rows and columns we want
dat %>%  
  # group_by(HR) %>%
  summarize(tidy(lm(R ~ HR + BB), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

# visualize the table with ggplot
dat |>  
  group_by(HR) |>
  summarise(tidy(lm(R ~ BB), conf.int = TRUE)) |>
  filter(term == "BB") |>
  select(HR, estimate, conf.low, conf.high) |>
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

#### Q5

get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

dat %>% 
  group_by(HR) %>% 
  summarize(slope = get_slope(across()))

### Q7

dat2 <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)



dat2 %>% 
  group_by(lgID) %>% 
  summarize(tidy(lm(R ~ HR, data = across()), conf.int = T)) %>% 
  filter(term == "HR") 
### what is this doing?

dat3 <- dat2 |> group_by(lgID)

dat4 <- dat3 |> summarise(tidy(lm(R ~ HR))
)

### part 2

library(tidyverse)
library(HistData)
data("GaltonFamilies")
# set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

galton |> group_by (pair) |> summarise(nbr_pairs = n(), cor(parentHeight, childHeight))
galton |> group_by (pair) |> summarise(tidy(lm(childHeight ~ parentHeight), data = across(), conf.int = T)) |> 
  filter(term == "parentHeight") |>
  select(pair, estimate, conf.low, conf.high) |>
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

# linear regression with two variables - §2.4
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs


# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, colour = I("black"))

# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
# NOTE: In old versions of the Lahman library, the "People" dataset was called "Master"
# The following code may need to be modified if you have not recently updated the Lahman library.
players <- People %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, colour = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, colour = POS)) + 
  geom_point() +
  scale_x_log10()

### Measurement Error Models
library(dslabs)
falling_object <- rfalling_object()

falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in metres") +
  xlab("Time in seconds")

fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)

tidy(fit)

augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")

tidy(fit, conf.int = TRUE)

Q3_teams <- tibble (Team = c("A","B"), BB = c(2,1), S = c(4,6), D = c(1,2), T = c(0,1), HR = c(1,0))
                          