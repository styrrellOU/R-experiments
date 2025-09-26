library(tidyverse)
library(broom)
library(Lahman)

res <- Teams |> 
  filter(yearID %in% 1961:2001) |> 
  mutate(avg_attendance = attendance/G, BB = BB/G, HR = HR/G, R = R/G, W_stratum = round(W/10,0)) |>
  filter (W_stratum %in% 5:10) |>
  select (yearID, avg_attendance, R, BB, HR, W, W_stratum) |>
  group_by(W_stratum) |>
  do(tidy(lm(avg_attendance ~ R, data = .))) |>
  ungroup()
     
res <- Teams |> 
  filter(yearID %in% 1961:2001) |> 
  mutate(avg_attendance = attendance/G, BB = BB/G, HR = HR/G, R = R/G) |>
  select (yearID, avg_attendance, R, BB, HR, W) |>
  # group_by(yearID) |>
  do(tidy(lm(avg_attendance ~ R + HR + W + yearID, data = .))) # |>
  # ungroup()

res |> filter (term == "R")

hist_data <- Teams |> 
  filter(yearID %in% 1961:2018) |> 
  mutate(avg_attendance = attendance/G, BB = BB/G, HR = HR/G, R = R/G) |>
  select (yearID, avg_attendance, R, BB, HR, W)

fit <- hist_data |> lm (avg_attendance ~ R + HR + W + yearID, data = _)

val2002 <- hist_data |> filter (yearID == 2002)
preds <- predict (fit,  val2002)
cor(preds, val2002$avg_attendance)


fit <- Teams |> 
  filter(yearID %in% 1961:2001) |> 
  mutate(avg_attendance = attendance/G, BB = BB/G, HR = HR/G, R = R/G) |>
  select (yearID, avg_attendance, R, BB, HR, W) |>
  lm (avg_attendance ~ R + HR + W + yearID, data = _)
  
y_hat = predict (fit, se.fit = TRUE)

Teams_small <- Teams |> 
  filter(yearID %in% 1961:2001) |> 
  mutate(avg_attendance = attendance/G) |>
  filter (W >= 45) |>
  select (yearID, avg_attendance, W)


sum(Teams_small$W_stratum == 8)

sum(Teams_small$W >= 75)-sum(Teams_small$W > 85)
Teams_small


x <- 76
y <- round(x/10, 1)
