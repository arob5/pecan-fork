# forward_model/ensemble_input_list.r


#' Construct ensemble model input in list format
#' 
#' Constructs an object of class \code{EnsembleInputList}, which is 
#' a light wrapper around a list of \code{ModelInput} objects. 
#' 
#' @details
#' The inputs list is accessed via \code{x$inputs}. The field \code{x$metadata}
#' can be used to store metadata related to the ensemble (input/run-specific 
#' metadata should instead be stored in the \code{metadata} field of the 
#' \code{ModelInput} objects). If the \code{inputs} list is named, these names
#' will be interpreted as run IDs associated with each input/run.
#' \code{EnsembleInputList} class also inherit from \code{EnsembleInput}.
#'
#' @param inputs A list of \code{ModelInput} objects, one per run.
#' @param metadata A named list, or empty list. 
#' @return An object of class \code{EnsembleInputList}, inheriting from 
#'  \code{EnsembleInput}.
#' @export
EnsembleInput.list <- function(inputs, metadata=list(), ...) {
  
  # Set default run IDs if not provided.
  if(is.null(names(inputs))) {
    names(inputs) <- paste0("run_", as.character(seq_along(inputs)))
  }
  
  obj <- structure(list(inputs=inputs, metadata=metadata),
                   class=c("EnsembleInputList", "EnsembleInput"))
  validate_ensemble_input_list(obj)
  
  return(obj)
}


is_ensemble_input_list <- function(x) {
  is_ensemble_input(x) && inherits(x, "EnsembleInputList")
}


validate_ensemble_input_list <- function(obj) {
  if(!is_ensemble_input_list(obj)) {
    stop("`obj` is not an `EnsembleInputList` object.")
  }
  
  if(!("inputs" %in% names(obj))) {
    stop("`EnsembleInputList$inputs` list is missing.")
  }
  
  if(!("metadata" %in% names(obj))) {
    stop("`EnsembleInputList$metadata` list is missing.")
  }
  
  if(!is_named_or_empty_list(obj$inputs, check_unique_names=TRUE)) {
    stop("EnsembleInputList$inputs must be a named list or empty list.")
  }
  
  if(!is_named_or_empty_list(obj$metadata, check_unique_names=TRUE)) {
    stop("EnsembleInputList$metadata must be a named list or empty list.")
  }
  
  if(!all(vapply(obj$inputs, is_model_input, logical(1)))) {
    stop("`EnsembleInputList$inputs` list contains element(s) not of class `ModelInput`.")
  }
  
  invisible(obj)
}


as_ensemble_input_list <- function(x, ...) {
  UseMethod("as_ensemble_input_list")
}


#' @export
as_ensemble_input_list.default <- function(x, ...) {
  stop("as_ensemble_input_list() is not implemented for objects of class ", 
       paste(class(x), collapse = "/"))
}


as_ensemble_input_list.EnsembleInputList <- function(x, ...) {
  x
}


run_ids.EnsembleInputList <- function(x, ...) {
  names(x$inputs)
}


#' Access input slot names
#'
#' Returns the names of slots (input fields) present in the \code{ModelInput}
#' objects making up the ensemble run.
#'
#' @param x A code{EnsembleInputList} object.
#' @param unique_only Logical; if \code{TRUE} (default), returns only the
#'   unique set of slot names across runs. If \code{FALSE}, returns
#'   per-run slot names (a list). Makes no difference for \code{ModelInput} objects.
#' @param ... Further arguments passed to methods.
#'
#' @return A character vector of slot names if \code{unique_only = TRUE},
#'   otherwise a list of character vectors (per run).
#' @export
slot_names.EnsembleInputList <- function(x, unique_only=TRUE, ...) {
  slot_names_per_run <- lapply(x$inputs, slot_names)
  if(unique_only) unique(unlist(slot_names_per_run, use.names=FALSE)) 
  else slot_names_per_run
}


metadata_names.EnsembleInputList <- function(x, unique_only=TRUE, ...) {
  metadata_names_per_run <- lapply(x$inputs, metadata_names)
  if(unique_only) unique(unlist(metadata_names_per_run, use.names=FALSE)) 
  else metadata_names_per_run
}


#' @export
print.EnsembleInputList <- function(x, ...) {
  cat("<EnsembleInputList>\n")
  cat(" Number of runs:", length(x), "\n")
  
  slot_nm <- slot_names(x)
  if(length(slot_nm) == 0L) {
    cat("  (no slots)\n")
  } else {
    cat(" slots:", paste(slot_nm, collapse = ", "), "\n")
  }
  
  invisible(x)
}
