#' Householder reflection
#'
#' @param x numeric vector of length 2 with x[1] > 0
#' @return 2x2 matrix H with H %*% x = -||x|| e1
#' @export
householder <- function(x) {          # define a function that takes an input x
  e1 <- c(1, 0)                       # c() = "combine" → the vector (1, 0)
  xnorm <- sqrt(sum(x^2))             # x^2 squares EACH entry; sum adds; sqrt → ||x||
  b <- x / xnorm + e1                 # unit vector in x's direction, plus e1
  H <- diag(2) - 2 * (b %*% t(b)) / sum(b^2)   # diag(2) = 2x2 identity; t() = transpose
  return(H)                           #   %*% = MATRIX multiply (plain * is entry-by-entry!)
}
