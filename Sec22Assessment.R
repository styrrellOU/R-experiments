set.seed(16, sample.kind = "Rounding")
nbr <- 10000
mu <- 20.9
sig <- 5.7
act_scores <- rnorm(nbr, mu, sig)
mean(act_scores)
sd(act_scores)

x <- seq(1:36)
f_x <- dnorm(x, mu, sig)
plot (x, f_x)

