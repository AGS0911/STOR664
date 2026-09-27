n <- 40
u <- rep(c(1, -1), length.out = n) / sqrt(n)
v <- rep(c(1, 1, -1, -1), length.out = n) / sqrt(n)
rho <- 0.9
s <- sqrt(1 - rho^2)
X.orth <- cbind(u, v)
X.col <- cbind(u / s, rho * u / s + v)
beta <- c(1, 1)
sigma <- 1

set.seed(664)       # set ONCE, so your results are reproducible
n.sims <- 5000

simulate_design <- function(X) {
  out <- matrix(NA, nrow = n.sims, ncol = 3)     # empty 5000 x 3 table to fill in
  colnames(out) <- c("b1", "b2", "sum")
  for (i in 1:n.sims) {                          # repeat 5000 times:
    y <- X %*% beta + rnorm(n, mean = 0, sd = sigma)   #  1. fake data: y = Xβ + ε
    fit <- lm(y ~ X - 1)                               #  2. fit; "- 1" = no intercept
    b <- coef(fit)                                     #  3. pull out β̂
    out[i, ] <- c(b[1], b[2], b[1] + b[2])             #  4. save row i
  }
  out
}

res.orth <- simulate_design(X.orth)
res.col  <- simulate_design(X.col)

tab <- rbind(
  orth.mean = colMeans(res.orth),
  orth.var  = apply(res.orth, 2, var),
  col.mean  = colMeans(res.col),
  col.var   = apply(res.col, 2, var)
)
round(tab, 3)

par(mfrow = c(2, 3))      # split the plot window into 2 rows x 3 columns
labels <- c("beta1 hat", "beta2 hat", "beta1 hat + beta2 hat")
for (j in 1:3) {          # j = 1, 2, 3 → one column each
  lims <- range(res.orth[, j], res.col[, j])   # same x-axis for both rows in this column
  hist(res.orth[, j], xlim = lims, main = paste("Orthogonal:", labels[j]), xlab = "")
}
for (j in 1:3) {
  lims <- range(res.orth[, j], res.col[, j])
  hist(res.col[, j],  xlim = lims, main = paste("Correlated:", labels[j]), xlab = "")
}
par(mfrow = c(1, 1))      # reset to one plot per window

par(mfrow = c(1, 2))
plot(res.orth[, 1], res.orth[, 2], pch = ".", main = "Orthogonal",
     xlab = "beta1 hat", ylab = "beta2 hat")
plot(res.col[, 1],  res.col[, 2],  pch = ".", main = "Correlated",
     xlab = "beta1 hat", ylab = "beta2 hat")
par(mfrow = c(1, 1))
