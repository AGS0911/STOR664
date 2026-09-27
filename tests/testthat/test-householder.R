test_that("householder maps x to -||x|| e1", {
  x <- c(4, 3)
  H <- householder(x)
  expect_equal(as.vector(H %*% x), c(-5, 0))
})

test_that("householder returns an orthogonal matrix", {
  H <- householder(c(4, 3))
  expect_equal(t(H) %*% H, diag(2))
})
