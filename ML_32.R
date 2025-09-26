# see that the trend is wobbly

library(tidyverse)
set.seed(1)
n <- 100
x <- seq(-pi*4, pi*4, len = n)
tmp <- data.frame(x = x , f = sin(x) + x/8, e = rnorm(n, 0, 0.5)) 
p1 <- qplot(x, f, main = "smooth trend", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p2 <- qplot(x, e, main = "noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p3 <- qplot(x, f+e, main = "data = smooth trend + noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
gridExtra::grid.arrange(p1, p2, p3)

# estimate the time trend in the 2008 US popular vote poll margin
library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

# use regression to estimate
resid <- ifelse(lm(margin~day, data = polls_2008)$resid > 0, "+", "-")
polls_2008 |> 
  mutate(resid = resid) |> 
  ggplot(aes(day, margin)) + 
  geom_smooth(method = "loess", se = FALSE, colour = "black") +
  geom_point(aes(colour = resid), size = 3)

# bin smoothers
span <- 3.5
tmp <- polls_2008 |>
  crossing(centre = polls_2008$day) |>
  mutate(dist = abs(day - centre)) |>
  filter(dist <= span) 

tmp |> filter(centre %in% c(-125, -55)) |>
  ggplot(aes(day, margin)) +   
  geom_point(data = polls_2008, size = 3, alpha = 0.5, colour = "grey") +
  geom_point(size = 2) +    
  geom_smooth(aes(group = centre), 
              method = "lm", formula=y~1, se = FALSE) +
  facet_wrap(~centre)

#this code shows just the bins and the bin average

# larger span
span <- 7 
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "box", bandwidth = span))

# fit is a new set of the mean data points around each bin
# ksmooth 

polls_2008 |> mutate(smooth = fit$y) |>
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, colour = "grey") + 
  geom_line(aes(day, smooth), colour="red")

# kernel
span <- 21
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "box", bandwidth = span))

polls_2008 |> mutate(smooth = fit$y) |>
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, colour = "grey") + 
  geom_line(aes(day, smooth), colour="red")

####

library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_tibble() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

library(broom)

span <- 60 / as.numeric (diff(range(dat$date)))
fit <- dat %>% mutate (dayNbr = as.numeric(date)) %>% loess (deaths ~ dayNbr, data = ., span = span, degree = 1)
# doesn't work with standard pipe?

# fit <- loess(deaths ~ day, degree=1, span = 60, data=dat)
# loess_fit <- augment(fit)

dat |> mutate(smooth = predict(fit, as.numeric(date))) |>
  ggplot(aes(date, deaths)) +
  geom_point(size = 3, alpha = .5, colour = "grey") +
  geom_line(aes(date, smooth), colour="red")

#dat %>% 
#  mutate(smooth = predict(fit), day = yday(date), year = as.character(year(date))) %>%
#  ggplot(aes(day, smooth, col = year)) +
#  geom_line(lwd = 2)

dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

## Q3

library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

qplot(x_2, y, data = mnist_27$train)

mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%    # convert to a binary yes or no
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")

fit <- mnist_27$train |>
  mutate(y = ifelse(y==7, 1, 0)) |>
  loess (y ~ x_2, degree = 1, data = _)

library(caret)
p_hat <- predict(fit, newdata = mnist_27$test, degree = 1)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]]


