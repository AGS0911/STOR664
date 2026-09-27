#' Standard Matrix Multiplication
#'Computes standard matrix multiplication of two matrices using R's standard built-in row-column operator.
#' @param A A numeric matrix of dimensions m x n.
#' @param B A numeric matrix of dimensions n x p.
#'
#' @return A numeric matrix of dimensions m x p representing the product of A and B.
#' @export
#'
#' @examples
#' mat_a <- matrix(1:4, nrow = 2)
#' mat_b <- matrix(5:8, nrow = 2)
#' matmul_base(mat_a, mat_b)
matmul_base <- function(A, B) {
    # View 1: Standard built-in row-column dot product view
    A %*% B
  }

#' Column Perspective Matrix Multiplication
#'
#' Computes the matrix product by viewing the operation as a series of
#' matrix-vector products with the columns of B.
#'
#' @param A A numeric matrix.
#' @param B A numeric matrix.
#' @return A numeric matrix representing the product of A and B.
#' @export
matmul_col_perspective <- function(A, B) {
  # Map over each column index of B and multiply matrix A by that column vector
  res_list <- lapply(1:ncol(B), function(j) A %*% B[, j])

  # Bind the resulting column vectors back into a matrix structure
  do.call(cbind, res_list)
}

#' Row Perspective Matrix Multiplication
#'
#' Computes the matrix product by viewing the rows of the output matrix
#' as linear combinations of the rows of B.
#'
#' @param A A numeric matrix.
#' @param B A numeric matrix.
#' @return A numeric matrix representing the product of A and B.
#' @export
matmul_row_perspective <- function(A, B) {
  # Map over each row index of A
  res_list <- lapply(1:nrow(A), function(i) {
    # Each row element multiplies the corresponding row vector of B
    colSums(A[i, ] * B)
  })

  # Bind the resulting row vectors back into a matrix structure
  do.call(rbind, res_list)
}

#' Outer Product Matrix Multiplication
#'
#' Computes the matrix product by summing up the rank-1 outer products
#' of the columns of A and the rows of B.
#'
#' @param A A numeric matrix.
#' @param B A numeric matrix.
#' @return A numeric matrix representing the product of A and B.
#' @export
matmul_outer_product <- function(A, B) {
  # Generate a list of rank-1 matrices using columns of A and rows of B
  outer_products <- lapply(1:ncol(A), function(k) outer(A[, k], B[k, ]))

  # Sum the matrices together sequentially
  Reduce("+", outer_products)
}

#' Linear Transformation Composition Matrix Multiplication
#'
#' Computes the matrix product by treating A and B as linear transformations
#' and evaluating their sequential functional composition on the standard basis vectors.
#'
#' @param A A numeric matrix.
#' @param B A numeric matrix.
#' @return A numeric matrix representing the product of A and B.
#' @export
matmul_linear_transform <- function(A, B) {
  # Create an identity matrix representing the standard basis vectors of the domain
  I <- diag(ncol(B))

  # Map over each basis vector, applying the transformation B first, then A
  res_list <- lapply(1:ncol(B), function(j) {
    x <- I[, j]
    A %*% (B %*% x)  # Composition: A(B(x))
  })

  # Reconstruct the transformed space into the output matrix
  do.call(cbind, res_list)
}


#' Title
#'
#' @param A
#' @param B
#'
#' @return
#' @export
#'
#' @examples
matmul_elementwise_fallback <- function(A, B) {
  # Alternative View: Helper or element-wise tracking fallback
  ## STUB
}
