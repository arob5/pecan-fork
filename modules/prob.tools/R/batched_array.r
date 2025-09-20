# batched_array.r
#
# Depends: assertthat

# Contains functions for manipulating and converting between three array-based
# data structures:
# 1.) Batched array: An array in which the first dimension is interpreted as
#     a batch dimension. In other words, a batched array is an ordered set of
#     arrays of equal dimension.
# 2.) Array list: A list of arrays, not necessarily of the same dimension.
# 3.) Flattened array: a flattened matrix representation of one or more arrays.
#     The number of rows corresponds to the number of arrays (batch size). Each
#     row is a flattened vector representation of an array or an array list.


#' Check if object is array or vector
#' 
#' A vector is viewed as a single-row matrix (which is an array).
#'
#' @returns logical, \code{TRUE} is \code{x} is an array or vector, else
#'  \code{FALSE}.
#' 
#' @author Andrew Roberts
is_array_like <- function(x) {
  is.array(x) || (is.vector(x) && is.atomic(x))
}


get_array_like_dim <- function(x) {
  assert_that(is_array_like(x))
  
  if(is.array(x)) dim(x)
  else c(1,length(x))
}


is_array_list_equal_dim <- function(x) {
  
  if(!is.list(x)) return(FALSE)
  
  arr_dims <- lapply(x, get_array_like_dim)
  
  if(length(arr_dims) == 1L) {
    all_dims_equal <- TRUE
  } else {
    all_dims_equal <- Reduce(function(dim1, dim2) all(dim1==dim2), arr_dims)
  }
  
  return(all_dims_equal)
}


#' Convert batched array to flattened (matrix) representation
#' 
#' Converts an array (or vector), to a standardized flat representation.
#' 
#' @details
#' The input is \code{x} is an array whose first dimension is interpreted as
#' a batch dimension. For example, an array with dimension \code{(4,2,3)} is
#' interpreted as a batch of 4 two-by-three arrays. This array would be 
#' flattened to a matrix of shape \code{(4,6)} with each row representing a
#' flattened version of one of the \code{(2,3)} arrays. In general, an input
#' with shape \code{c(n, shape)} will be converted to a matrix of shape
#' \code{(n, prod(shape))}. 
#'
#' This method exactly reverses the steps of  \code{\link{.flat_to_array()}} 
#' to ensure consistent and reproducible conversion.
#' 
#' @param x An array. The first dimension is interpreted as the batch dimension.
#'  An array with a single dimension is treated as an array with batch size one.
#'  A vector of length \code{len} is treated as a \code{(1,1,len)} batched
#'  array.
#' 
#' @returns Matrix with number of rows equal to the number of arrays in the
#'  batch and number of columns equal to the number of elements in each 
#'  individual array. See details for specifics.
#' 
#' @author Andrew Roberts
.batched_array_to_flat = function(x) {
  
  .check_input_is_batched_array(x)
  x <- .wrap_singleton_as_batched_array(x)
  
  dim_x <- dim(x)
  n <- dim_x[1] # Batch size
  n_dims_total <- length(dim_x) # Including batch dimension
  len <- prod(dim_x[2:n_dims_total]) # Number of entries in individual array
  
  x <- aperm(x, c(2:n_dims_total, 1L)) # dim = c(shape, n)
  x_flat <- matrix(x, nrow=len, ncol=n) # dim = (len, n)
  x_flat <- t(x_flat) # dim = (n, len)
  
  return(x_flat)
}


#' Invert the flattening operation for a batched array 
#' 
#' Converts a flattened representation of an array (a vector), or multiple 
#' such vectors, to a (potentially multidimensional) array.
#' 
#' @details
#' Let \code{len} denote \code{prod(target_shape)}, the number of entries in
#' the target array. The input \code{x} is required to be a vector with length 
#' equal to \code{len} or a matrix of shape \code{(n, len)}. In the former 
#' case the input is converted to an array of shape \code{c(1,target_shape)}.
#' In the latter case the input is converted to an array of shape 
#' \code{c(n, target_shape)}. The reshaping is done in row major 
#' order (i.e., "C" style).
#' 
#' @param x values in flat (vector or matrix) format.
#' @param target_shape integer vector, the shape of the target array.
#' 
#' @returns In general, returns array of shape \code{c(n, target_shape)}, when
#'  the input contains \code{n} flattened values.
#'
#' @author Andrew Roberts
.flat_to_batched_array = function(x, target_shape) {
  
  .check_input_is_flat(x, target_shape)
  x <- .wrap_singleton_as_flat(x)
  
  n <- nrow(x)
  n_dims <- length(target_shape)
  
  # R stores in column-major order (columns are contiguous in memory).
  # We thus transpose `x` so that each value becomes a column. Values
  # are assigned to array in column-major order, then permuted afterwards
  # to maintain the convention that the first dimension is the number 
  # of values.
  arr <- array(t(x), dim=c(target_shape, n)) # dim = c(target_shape, n)
  arr <- aperm(arr, c(n_dims + 1L, seq_along(target_shape))) # dim = c(n, target_shape)
  
  return(arr)
}


#' Convert array list to flat representation
#'
#' Converts a list of arrays (possibly of differing dimensions) to a flat
#' representation by flattening each array and then appending them.
#' 
#' @details
#' If \code{arrays_are_batched = FALSE} then each array in the list is viewed
#' as a single array (not a batched array), and the output is a matrix of
#' shape \code{(1, len)}, where \code{len} is the sum of the number of entries
#' across all of the arrays. If \code{arrays_are_batched = TRUE}, then each
#' array is viewed as a batch array and all arrays are required to have equal
#' batch size (i.e., same number of elements in the first dimension). The 
#' remaining dimensions need not be equal. In this case, the returned matrix
#' is of shape \code{(n, len)} where \code{n} is the batch size and 
#' \code{len} is the sum of the number of elements in all of the sub-arrays
#' excluding the first dimension (e.g., a \code{(n,2,3)} batched array has length
#' 6).
#' 
#' @param x list of arrays or batched arrays.
#' @param arrays_are_batched logical(1) whether to view elements of the list as
#'  arrays or batched arrays.
#'  
#' @returns matrix, with number of rows corresponding to the batch size and
#'  number of columns corresponding to the length of the flattened array
#'  representation.
#'
#' @author Andrew Roberts
.array_list_to_flat <- function(x, arrays_are_batched=FALSE) {
  
  # Convert to batched arrays with batch size 1.
  if(!arrays_are_batched) {
    .check_input_is_array_list(x)
    x <- lapply(x, .wrap_singleton_as_batched_array)
  } else {
    .check_input_is_batched_array_list(x)
  }
  
  x_flat_list <- lapply(x, .batched_array_to_flat)
  do.call(cbind, x_flat_list)
}


#' Inverts .array_list_to_flat
#'
#' Converts flattened matrix representation to a list of batched arrays.
#' 
#' @details
#' The argument \code{target_shapes} provides the blueprint for how to expand
#' the flattened representation. It is a list of array dimensions, containing
#' the dimensions for the individual arrays (i.e., not including the batch
#' dimension). For example, an array list \code{list(c(2,3), c(4,3,6)} implies
#' a target list of length two, in which the two list elements will be arrays
#' of shapes \code{(n,2,3)} and \code{n,4,3,6}, respectively, where \code{n}
#' is the batch dimension. The batch dimension is given by the number of rows
#' in \code{x}. If \code{x} is a single dimension or a vector, then the batch
#' dimension is assumed to be one. 
#'
#' @returns List of length \code{length(target_shapes)}, with each element 
#'  containing a batched array (see details). Throws error if 
#'  \code{target_shapes} is inconsistent with the dimensions of \code{x}.
#'
#' @author Andrew Roberts
.flat_to_batched_array_list <- function(x, target_shapes) {
  
  .check_input_is_flat(x, target_shapes)
  x <- .wrap_singleton_as_flat(x)
  if(!is.list(target_shapes)) target_shapes <- list(target_shapes)
  
  len_cutoffs <- c(1, cumsum(vapply(target_shapes, prod, numeric(1))))
  
  # Helper to extract correct subset of x and unflatten.
  unflatten_arr <- function(i) {
    col_idx_start <- len_cutoffs[i]
    col_idx_end <- len_cutoffs[i+1]
    
    .flat_to_batched_array(x[,col_idx_start:col_idx_end, drop=FALSE],
                           target_shapes[[i]])
  }
  
  lapply(seq_along(target_shapes), unflatten_arr)
}


#' Validate that input is flattened array (or array list) of particular shape
#' 
#' Checks whether \code{x} can be mapped to a list of batched arrays, with
#' array shapes given by \code{target_shapes}.
#'
#' @param x An R object
#' @param target_shapes list or integer vectors. If a list, then each element
#'  should be an integer vector giving the dimensions of an array. The batch
#'  dimension should not be included in these dimensions. A single integer vector
#'  will be wrapped as a one-element list.
#'  
#' @returns invisibly returns \code{TRUE} if \code{x} can be viewed as a flattened
#'  list of batched arrays of the form determined by \code{target_shapes}.
#'  Otherwise throws an error.  
#'
#' @author Andrew Roberts
.check_input_is_flat <- function(x, target_shapes) {
  
  if(!is.list(target_shapes)) target_shapes <- list(target_shapes)
  
  assert_that(is_array_like(x),
              msg="Flattened batched array representation requires vector or matrix.")
  x <- .wrap_singleton_as_flat(x)
  
  # Ensure dimensions of x are consistend with target_shapes.
  len <- sum(vapply(shape_list, prod, numeric(1)))
  
  assert_that(ncol(x) == len,
              msg=paste0("Flattened array with length ", ncol(x), " is not ",
                         "in agreement with target length ", len))
  
  invisible(TRUE)
}


.check_input_is_batched_array <- function(x) {
  
  assert_that(is.vector(x) || is.array(x),
              msg="Batched array representation requires vector or array.")
  
  invisible(TRUE)
}


# Requires x to be a list of arrays, not necessarily of the same dimension.
.check_input_is_array_list <- function(x) {
  assert_that(is.list(x))
  assert_that(all(vapply(x, is_array_like, logical(1))),
              msg="An array list requires each element to be an array or vector.")
}


# Batched array list requires every element to be a batched array with equal
# batch size.
.check_input_is_batched_array_list <- function(x) {
  
  assert_that(is.list(x))

  # Ensure each element is a batched array.
  for(i in seq_along(x)) {
    tryCatch(
      .check_input_is_batched_array(x[[i]]),
      error = function(e) {
        nm <- names(x)[i]
        stop(sprintf("Element '%s' in list is not batched array: %s", 
                     nm, e$message), call.=FALSE)
      }
    )
  }
  
  # Ensure all batched arrays have the same batch size. 
  arr_dims <- vapply(x, function(arr) .get_batched_array_dim(arr)[1], integer(1))
  
  if(length(unique(arr_dims)) > 1L) {
    stop("Arrays in list have different batch sizes.")
  }
  
  invisible(TRUE)
}


.wrap_singleton_as_flat <- function(x) {
  if(is.vector(x) && is.atomic(x)) x <- matrix(x, nrow=1L)
  else x
}


# Already assumes that x is vector or array
.wrap_singleton_as_batched_array <- function(x) {
  if(is.vector || length(dim(x)) == 1L) matrix(drop(x), nrow=1L)
  else x
}


.get_batched_array_dim <- function(x) {
  x <- .wrap_singleton_as_batched_array(x)
  dim(x)
}

