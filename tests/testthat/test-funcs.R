test_that("I know how to do matrix multiplication by hand", {
  A <- matrix(1:4, nrow = 2, ncol = 2)
  B <- matrix(1:6, nrow = 2, ncol = 3)

  # FIX: Vector elements 22 and 23 are placed in column-major sequence
  prod <- matrix(c(7, 10, 15, 22, 23, 34), nrow = 2, ncol = 3)
  expect_equal(A %*% B, prod)
})

test_that("Built-in Multiplication Works", {
  A <- matrix(1:4, nrow = 2, ncol = 2)
  B <- matrix(1:6, nrow = 2, ncol = 3)

  prod <- matrix(c(7, 10, 15, 22, 23, 34), nrow = 2, ncol = 3)
  expect_equal(matmul_base(A, B), prod)
})

test_that("All matrix multiplication views match the base operator", {
  A <- matrix(runif(4), nrow = 2, ncol = 2)
  B <- matrix(runif(6), nrow = 2, ncol = 3)
  target <- matmul_base(A, B)

  expect_equal(matmul_col_perspective(A, B), target)
  expect_equal(matmul_row_perspective(A, B), target)
  expect_equal(matmul_outer_product(A, B), target)
  expect_equal(matmul_linear_transform(A, B), target)

})
