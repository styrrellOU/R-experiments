p_special <- 5/38
p_not_special <- 1-p_special

v_special <- 6
v_not_special <- -1

n <- 500

exp_v <- p_special * v_special + p_not_special * v_not_special
se_v <- abs(v_special - v_not_special) * sqrt (p_special * p_not_special)

se_mean <- se_v / sqrt(n)
exp_sum <- exp_v * n

se_sum <- se_v * sqrt(n)

pnorm(0, exp_sum, se_sum)