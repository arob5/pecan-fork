# Probabilistic modeling utility/helper functions.

#' Check if a matrix is positive definite, and return Cholesky factor.
#'
#' Defines "positive definite" in the numerical sense of being able to run 
#' `chol(m)` without an error. To avoid wasting this computation, returns 
#' the result of `chol(m)` in addition to the logical.
is_positive_definite <- function(m) {
  out <- tryCatch({
    chol_upper <- chol(m)
    list(is_pos_def=TRUE, chol_upper=chol_upper)
  }, error = function(e) {
    list(is_pos_def=FALSE, chol_upper=NULL)
  })
  return(out)
}

is_lower_tri <- function(m) {
  all(m[upper.tri(m)] == 0)
}

is_upper_tri <- function(m) {
  all(m[lower.tri(m)] == 0)
}

