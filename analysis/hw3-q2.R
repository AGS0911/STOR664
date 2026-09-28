beta.hat <- c(128, -10, 3)
RSS <- 54
n <- 9
p <- 3
XtXinv <- matrix(c( 1, -1,  1,
                    -1,  2, -3,
                    1, -3,  6), nrow = 3, byrow = TRUE) / 3
gamma <- c(0, 1, 1)

theta.hat <- sum(gamma * beta.hat)                  # -7
s2 <- RSS / (n - p)                                 # 9
s  <- sqrt(s2)                                      # 3
v.gamma <- drop(t(gamma) %*% XtXinv %*% gamma)      # 2/3
se <- s * sqrt(v.gamma)                             # 2.449

se.wrong <- s * sqrt(XtXinv[2, 2] + XtXinv[3, 3])   # 4.899 (ignores covariance)

t.crit <- qt(0.975, n-p)                  # 2.447
ci <- theta.hat + c(-1, 1) * t.crit * se      # (-12.99, -1.01)

t.obs <- (theta.hat - 0) / se                 # -2.858
p.value <- 2 * pt(-abs(t.obs), n-p)       # 0.0289

ci
t.obs
p.value

