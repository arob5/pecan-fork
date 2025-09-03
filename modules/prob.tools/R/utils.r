# prob.tools/R/utils.r

#' Check if an R object is a named vector, with all names provided (empty
#' strings are not allowed).
#' 
#' @param x An R object
#' @param check_unique logical, if \code{TRUE} then impose uniqueness requirement on names.
#' @returns logical, \code{TRUE} if a vector with all names provided. 
#' @author Andrew Roberts
is_named_numeric_vector <- function(x, check_unique=TRUE) {
  is.numeric(x) &&
  is.atomic(x) &&
  !is.array(x) &&
  has_names(x, check_unique)
}


#' Check if an R object is a named list, with all names provided (empty
#' strings are not allowed).
#' 
#' @param x An R object
#' @param check_unique logical, if \code{TRUE} then impose uniqueness requirement on names.
#' @returns logical, \code{TRUE} if a list with all names provided. 
#' @author Andrew Roberts
is_named_list <- function(x, check_unique=TRUE) {
  is.list(x) &&
  length(x) > 0 &&
  has_names(x, check_unique)
}

#' Check that R object has full set of names
#' 
#' Check if an R object has names, and that every element of the object
#' has a name (empty string does not count as a name). Optionally check 
#' that the names are unique. An object without a names attribute is considered 
#' to not have names.
#' 
#' @param x An R object
#' @param check_unique logical, if \code{TRUE} then impose uniqueness requirement on names.
#' 
#' @returns logical, \code{TRUE} if object has full set of (unique) names.
#' @author Andrew Roberts
has_names <- function(x, check_unique=TRUE) {
  !is.null(names(x)) &&
  length(names(x)) == length(x) &&
  all(nzchar(names(x))) && # Ensure no "" names.
  (!check_unique || !anyDuplicated(names(x)))
}


#' Check if a matrix is positive definite, and return Cholesky factor
#'
#' Defines "positive definite" in the numerical sense of being able to run 
#' \code{chol(m)} without an error. To avoid wasting this computation, returns 
#' the result of \code{chol(m)} (the upper Cholesky factor) in addition to the logical.
#' 
#' @param m A matrix 
#'
#' @returns A \code{list} with names \code{is_pos_def} and \code{chol_upper}.
#' @author Andrew Roberts
is_positive_definite <- function(m) {
  out <- tryCatch({
    chol_upper <- chol(m)
    list(is_pos_def=TRUE, chol_upper=chol_upper)
  }, error = function(e) {
    list(is_pos_def=FALSE, chol_upper=NULL)
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
#' @author Andrew Roberts
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
#' @author Andrew Roberts
is_upper_tri <- function(m) {
  all(m[lower.tri(m)] == 0)
}
