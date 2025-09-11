# utils/R/type_assertions.r
#
# Depends: assertthat

# Assertions that R objects possess various properties, extending the
# functionality in the `assertthat` package.


#' Check that R object has full set of names
#' 
#' Check if an R object has a unique set of names. Every element of the object
#' must have a non-NA, non-empty string name. 
#' 
#' @param x An R object
#' 
#' @returns logical, \code{TRUE} if object has full set of unique names.
#' 
#' @author Andrew Roberts
#' @export
has_unique_names <- function(x) {
  assertthat:::is.named(x) && !anyDuplicated(names(x))
}


#' Check if an R object is an atomic scalar
#' 
#' Must be atomic and length one. In contrast to \code{assertthat::is.scalar()},
#' singleton arrays, scalars, and one-element lists are not considered 
#' raw scalars.
#' 
#' @param x An R object
#' @returns logical, \code{TRUE} if a raw scalar.
#'
#' @author Andrew Roberts
#' @export
is_raw_scalar <- function(x) {
  is.atomic(x) &&
    !is.array(x) &&
    length(x) == 1L
}


#' Check for integerish vector with non-negative entries.
#' 
#' @param x An R object
#' 
#' @author Andrew Roberts
#' @export
is_nonneg_integer_vector <- function(x) {
  is.atomic(x) &&
    assertthat:::is.integerish(x) &&
    all(x >= 0)
}


#' Check if a matrix is positive definite, and return Cholesky factor
#'
#' Defines "positive definite" in the numerical sense of being able to run 
#' \code{chol(m)} without an error. To avoid wasting this computation, returns 
#' the result of \code{chol(m)} (the upper Cholesky factor) as an attribute 
#' \code{chol_upper} of the returned logical value. This attribute is only set 
#' when Cholesky decomposition does not throw an error.
#' 
#' @param m A matrix 
#'
#' @returns Logical, \code{TRUE} if \code{chol(m)} does not throw an error.
#' If \code{TRUE}, the \code{chol_upper} attribute is defined and contains
#' the upper Cholesky factor.
#' 
#' @author Andrew Roberts
#' @export
is_positive_definite <- function(m) {
  out <- tryCatch({
    chol_upper <- chol(m)
    structure(TRUE, chol_upper=chol_upper)
  }, error = function(e) {
    FALSE
  })
  
  return(out)
}


#' Check if a matrix is lower triangular
#'
#' A lower triangular matrix is one in which all entries strictly above the 
#' main diagonal are zero. Note that this function will run for non-square 
#' matrices, but it is primarily designed for square matrices.
#' 
#' @param m A matrix 
#'
#' @returns \code{TRUE} if \code{m} is lower triangular, else \code{FALSE}.
#' @seealso \code{\link{is_upper_tri}}
#' 
#' @author Andrew Roberts
#' @export
is_lower_tri <- function(m) {
  all(m[upper.tri(m)] == 0)
}

#' Check if a matrix is upper triangular
#'
#' A upper triangular matrix is one in which all entries strictly above the 
#' main diagonal are zero. Note that this function will run for non-square 
#' matrices, but it is primarily designed for square matrices.
#' 
#' @param m A matrix 
#'
#' @returns \code{TRUE} if \code{m} is upper triangular, else \code{FALSE}.
#' @seealso \code{\link{is_upper_tri}}
#' 
#' @author Andrew Roberts
#' @export
is_upper_tri <- function(m) {
  all(m[lower.tri(m)] == 0)
}
