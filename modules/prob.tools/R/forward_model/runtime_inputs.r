# forward_model/runtime_inputs.r

#' Runtime inputs for model runs
#'
#' A lightweight container for model inputs that can be provided at runtime
#' (i.e., when writing configs). Currently supports initial conditions (\code{ic})
#' and parameter vectors (\code{param}).
#' 
#' @details
#' The set of inputs required to run a model is defined by a \code{RunSpec}
#' object (see \code{\link{make_run_spec}}), which provides a light wrapper 
#' around a \code{RuntimeInputs} and \code{settings} object. In reality, only 
#' a \code{settings} object is needed to run a forward model; \code{RuntimeInputs}
#' can be thought of as a way to overwrite default values that are specified 
#' in \code{settings}. Therefore, if there is overlap between inputs specified
#' in these two objects, the values in \code{RuntimeInputs} take precedence.
#' 
#' At present, \code{RuntimeInputs} is effectively a light wrapper around 
#' initial conditions \code{ic} and parameter values \code{param}. See 
#' documentation on the function arguments for the required format for these
#' values.
#'
#' @param ic Initial conditions, a named list with names set to initial condition names.
#' @param param Named numeric vector of model parameters.
#' @param ... Reserved for future input types.
#' @return An object of class \code{RuntimeInputs}.
#' @seealso \code{\link{prep_model_run}}, \code{\link{run_model}}, \code{\link{make_run_spec}}
#' @author Andrew Roberts
#' @export
make_runtime_inputs <- function(param=NULL, ic=NULL, ...) {
  inputs <- list(param=param, ic=ic, ...)
  class(inputs) <- "RuntimeInputs"
  validate_runtime_inputs(inputs)
}

#' Validate runtime inputs
#'
#' @param x An object of class `RuntimeInputs`.
#' @return Invisibly returns `x` if valid, otherwise throws an error.
#' @author Andrew Roberts
#' @export
validate_runtime_inputs <- function(x) {
  
  if(!inherits(x, "RuntimeInputs")) {
    stop("`x` does not inherit from `RuntimeInputs`.")
  }
  
  if(!is_named_list(x, check_unique=TRUE)) {
    stop("`RuntimeInputs` objects must be named lists.")
  }

  # Validate parameters.
  if(!is.null(x$param)) {
    if(!param_format_is_valid(x$param)) {
      stop("`param` element of RuntimeInputs object `x` is invalid.")
    }
  }
  
  # Validate initial conditions.
  if(!is.null(x$ic)) {
    if(!ic_format_is_valid(x$ic)) {
      stop("`ic` element of RuntimeInputs object `x` is invalid.")
    }
  }
  
  invisible(x)
}

#' Check if initial condition runtime input is in valid format
#' 
#' The \code{ic} element of \code{\link{RuntimeInputs}} is required to
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
#' The \code{ic} element of \code{\link{RuntimeInputs}} is required to
#' be a list.
#'
#' @param x An R object.
#' @returns logical, \code{TRUE} if \code{ic} is valid.
#' @author Andrew Roberts
#' @export
ic_format_is_valid <- function(ic) {
  is_named_list(ic, check_unique=TRUE)
}


#' Print method for \code{RunTimeInputs}
#' 
#' @seealso \code{\link{make_runtime_inputs}}
#' @author Andrew Roberts
#' @export
print.RuntimeInputs <- function(x, include_param_names=FALSE, include_ic_names=FALSE, ...) {
  cat("<RuntimeInputs>\n")
  
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
