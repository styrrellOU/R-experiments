library(dslabs)
library(dplyr)
head(death_prob)

n <- 1000

payout <- -150000
#prem <- 1150

p <- death_prob |> filter(sex=="Male" & age==50)
q <- 0.015 #p$prob
#q <- seq(.01, .03, .0025)

# expValue <- 0 #q*payout + (1-q)*prem
#prem <- (expValue - q*payout)/(1-q)

se_1p <- abs(payout-prem)*sqrt(q*(1-q))

expProfit <- n*expValue
se_np <- sqrt(n)*se_1p

chanceLoss <- pnorm (-10^6, expProfit, se_np)

## Monte Carlo 

B <- 10000
set.seed(29, sample.kind = "Rounding")

randomSim <- function (thisProb) {
  thisProb <-thisProb + sample(seq(-0.01, 0.01, length = 100), 1)
  sum(sample (c(payout, prem), size = n, replace = TRUE, prob = c(thisProb, 1-thisProb)))
  
}

q <- 0.015
results <- replicate (B, randomSim(q))

sum(results)
mean(results <= 0)



## calc premium to reduce loss rate

z <- qnorm(0.05)
p <- q
prem <- -payout*(( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p))))
expProfit <- p*payout + (1-p)*prem
