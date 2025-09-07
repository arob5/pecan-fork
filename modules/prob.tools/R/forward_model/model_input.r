# forward_model/model_input.r

#' Model Input Class Constructor
#'
#' Creates a new \code{ModelInput} object, which is a container for
#' model inputs (called "slots") and optional metadata. Each slot is
#' a named element (e.g., \code{param}, \code{ic}, \code{driver})
#' storing an R object required by the model. Metadata can be used
#' for provenance, units, or other information.
#'
#' @param ... Named arguments representing the slots required by a model.
#'   For example, \code{param}, \code{ic}, \code{driver}.
#' @param metadata Optional named list of metadata to store alongside
#'   the slots.
#'
#' @return An object of class \code{ModelInput}.
#'
#' @examples
#' # Simple parameter set
#' mi1 <- ModelInput(param = c(a=1, b=2))
#'
#' # With additional initial conditions
#' mi2 <- ModelInput(param = c(a=2, b=5),
#'                   ic = list(x=0, y=0))
#'
#' # With metadata
#' mi3 <- ModelInput(param = c(a=3), metadata = list(units = "unitless"))
#'
#' slots(mi2)
#' metadata(mi3)
#' 
#' @seealso \code{\link{EnsembleInput}}
#' @author Andrew Roberts
#' @export
ModelInput <- function(..., metadata=list()) {
  slots <- list(...)

  x <- structure(list(slots=slots, metadata=metadata),
                 class = "ModelInput")
  validate_model_input(x)
  return(x)
}


#' Check if object inherits from \code{ModelInput}
#' 
#' @seealso \code{\link{ModelInput}}
#' @author Andrew Roberts
#' @export
is_model_input <- function(x) {
  inherits(x, "ModelInput")
}


check_model_input_type <- function(x) {
  if (!is_model_input(x)) {
    stop("`x` is not a ModelInput object.")
  }
}


#' Validate ModelInput
#'
#' Placeholder for model-specific validation logic.
#' By default, checks that \code{slots} is a named list.
#'
#' @param x A \code{ModelInput} object.
#' @return Invisibly returns \code{x}, or throws an error if invalid.
#' @export
validate_model_input <- function(x) {
  
  check_model_input_type(x)
  
  if(!("slots" %in% names(x))) {
    stop("`ModelInput$slots` is missing.")
  }
  
  if(!("metadata" %in% names(x))) {
    stop("`ModelInput$slots` is missing.")
  }

  if(!is_named_or_empty_list(x$slots, check_unique_names=TRUE)) {
    stop("`ModelInput$slots` must be a named list with unique names, or empty list.")
  }
  
  if(!is_named_or_empty_list(x$metadata, check_unique_names=TRUE)) {
    stop("`ModelInput$metadata` must be a named list with unique names, or empty list.")
  }
  
  invisible(x)
}


#' Slot Names Generic
#'
#' Returns the names of slots (input fields) present in a model input object.
#' Works for both single \code{ModelInput} objects and ensemble inputs.
#'
#' @param x A \code{ModelInput} or \code{EnsembleInput} object.
#' @param ... Further arguments passed to methods.
#'
#' @return A character vector of slot names if \code{unique_only = TRUE},
#'   otherwise a list of character vectors (per run).
#' @export
slot_names <- function(x, ...) {
  UseMethod("slot_names")
}


#' @export
slot_names.default <- function(x, ...) {
  stop("slot_names() is not implemented for objects of class ", 
       paste(class(x), collapse = "/"))
}


#' @export
slot_names.ModelInput <- function(x, ...) {
  names(x$slots)
}


#' Number of Slots Generic
#'
#' Returns the number of slots (input fields) present in a model input object.
#' Works for both single \code{ModelInput} objects and ensemble inputs.
#' 
#' @details
#' In the case of a \code{EnsembleInput} object, the individual \code{ModelInput}
#' objects may have different numbers of slots. In this case, the returned
#' value is the total number of unique slots, which corresponds to the number
#' of columns in the corresponding \code{EnsembleInputTable}.
#'
#' @param x A \code{ModelInput} or \code{EnsembleInput} object.
#' @param ... Further arguments passed to methods.
#'
#' @return Integer, number of slots.
#' @export
n_slots <- function(x, ...) {
  UseMethod("n_slots")
}


#' @export
n_slots.default <- function(x, ...) {
  stop("n_slots() is not implemented for objects of class ", 
       paste(class(x), collapse = "/"))
}


#' @export
n_slots.ModelInput <- function(x, ...) {
  length(x$slots)
}


#' Extract slots from ModelInput
#'
#' @param x A \code{ModelInput} object.
#' @return The \code{slots} list.
#' @export
slots <- function(x) {
  check_model_input_type(x)
  x$slots
}


#' Extract metadata from ModelInput
#'
#' @param x A \code{ModelInput} object.
#' @return The \code{metadata} list.
#' @export
metadata <- function(x) {
  check_model_input_type(x)
  x$metadata
}


#' Extract metadata names from ModelInput
#'
#' @param x A \code{ModelInput} object.
#' @return The names of the \code{metadata} list. Will be \code{NULL} if there 
#'  is no metadata.
#' @export
metadata_names <- function(x, ...) {
  UseMethod("metadata_names")
}


metadata_names.default <- function(x, ...) {
  stop("method_names() is not implemented for objects of class ", 
       paste(class(x), collapse = "/"))
}


#' Extract metadata names from ModelInput
#'
#' @param x A \code{ModelInput} object.
#' @return The names of the \code{metadata} list. Will be \code{character(0)} if there 
#'  is no metadata.
#' @export
metadata_names.ModelInput <- function(x, ...) {
  nm <- names(x$metadata)
  
  if(is.null(nm)) character(0)
  else nm
}


#' @export
print.ModelInput <- function(x, ...) {
  cat("<ModelInput>\n")
  
  # Print slot information.
  if(n_slots(x) == 0L) {
    cat("  (no slots)\n")
  } else {
    cat("  slots: ", paste(slot_names(x), collapse=", "), "\n")
  }
  
  # Print metadata information.
  if(length(x$metadata) > 0L) {
    cat("  metadata: ", paste(metadata_names(x), collapse=", "), "\n")
  }
  
  invisible(x)
}
