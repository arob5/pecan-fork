# forward_model/runtime_input.r


#' Create an \code{RuntimeInput} Object
#'
#' A lightweight container for model inputs that can be provided at runtime
#' (i.e., when writing configs). Currently supports initial conditions (\code{ic})
#' and parameter vectors (\code{param}).
#' 
#' @details
#' The set of inputs required to run a model is defined by a 
#' \code{\link{EnsembleInput}}, which provides a light wrapper 
#' around a table of \code{RuntimeInput} and \code{settings} objects. In reality, only 
#' a \code{settings} object is needed to run a forward model; \code{RuntimeInput}
#' can be thought of as a way to overwrite default values that are specified 
#' in \code{settings}. Therefore, if there is overlap between inputs specified
#' in these two objects, the values in \code{RuntimeInput} take precedence.
#' 
#' At present, \code{RuntimeInput} is effectively a light wrapper around 
#' initial conditions \code{ic} and parameter values \code{param}. See 
#' documentation on the function arguments for the required format for these
#' values.
#'
#' @param ic Initial conditions, a named list with names set to initial condition names.
#' @param param Named numeric vector of model parameters.
#' @param ... Reserved for future input types.
#' 
#' @return An object of class \code{RuntimeInput}.
#' @seealso \code{\link{prep_model_ensemble_run}}, \code{\link{run_model_ensemble}}, \code{\link{EnsembleInput}}
#' 
#' @author Andrew Roberts
#' @export
RuntimeInput <- function(param=NULL, ic=NULL, ...) {
  input <- list(param=param, ic=ic, ...)
  class(input) <- "RuntimeInput"
  validate_runtime_input(input)
}


#' Validate a \code{RuntimeInput} object
#'
#' @param x An object of class `RuntimeInput`.
#' @return Invisibly returns `x` if valid, otherwise throws an error.
#' @seealso \code{\link{RuntimeInput}}
#' @author Andrew Roberts
#' @export
validate_runtime_input <- function(x) {
  
  if(!inherits(x, "RuntimeInput")) {
    stop("`x` does not inherit from `RuntimeInput`.")
  }
  
  if(!is_named_list(x, check_unique=TRUE)) {
    stop("`RuntimeInput` objects must be named lists.")
  }

  # Validate parameters.
  if(!is.null(x$param)) {
    if(!param_format_is_valid(x$param)) {
      stop("`param` element of RuntimeInput object `x` is invalid.")
    }
  }
  
  # Validate initial conditions.
  if(!is.null(x$ic)) {
    if(!ic_format_is_valid(x$ic)) {
      stop("`ic` element of RuntimeInput object `x` is invalid.")
    }
  }
  
  invisible(x)
}


#' Check if initial condition runtime input is in valid format
#' 
#' The \code{ic} element of \code{\link{RuntimeInput}} is required to
#' be a named numeric vector.
#'
#' @param x An R object.
#' @returns logical, \code{TRUE} if \code{param} is valid.
#' @author Andrew Roberts
#' @export
param_format_is_valid <- function(param) {
  is_named_numeric_vector(param, check_unique=TRUE)
}


#' Check if parameter runtime input is in valid format
#' 
#' The \code{ic} element of \code{\link{RuntimeInput}} is required to
#' be a list.
#'
#' @param x An R object.
#' @returns logical, \code{TRUE} if \code{ic} is valid.
#' @author Andrew Roberts
#' @export
ic_format_is_valid <- function(ic) {
  is_named_list(ic, check_unique=TRUE)
}


#' Print method for \code{RuntimeInput}
#' 
#' @seealso \code{\link{RuntimeInput}}
#' @author Andrew Roberts
#' @export
print.RuntimeInput <- function(x, include_param_names=FALSE, include_ic_names=FALSE, ...) {
  cat("<RuntimeInput>\n")
  
  # Print parameter information
  if(!is.null(x$param)) {
    cat("  param: ", length(x$param), " parameters\n", sep="")
    if(include_param_names) {
      for(param_name in names(x$param)) cat(param_name, " ")
      cat("\n")
    }
  }
  
  # Print initial condition information
  if(!is.null(x$ic)) {
    cat("  ic: ", length(x$ic), " initial conditions\n", sep="")
    if(include_ic_names) {
      for(ic_name in names(x$ic)) cat(ic_name, " ")
      cat("\n")
    }
  }
  
  # Any other elements included in the list
  other <- setdiff(names(x), c("param", "ic"))
  if(length(other) > 0L) {
    cat("  other: ", paste(other, collapse = ", "), "\n", sep = "")
  }
  
  invisible(x)
}


#' Check if object inherits from \code{RuntimeInput}
#' 
#' @seealso \code{\link{RuntimeInput}}
#' @author Andrew Roberts
#' @export
is_runtime_input <- function(x) {
  inherits(x, "RuntimeInput")
}
