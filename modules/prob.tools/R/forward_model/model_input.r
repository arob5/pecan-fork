# forward_model/model_input.r

#' Model Input Class Constructor
#'
#' Creates a new \code{ModelInput} object, which is a container for
#' model inputs (called "slots") and optional metadata. Each slot is
#' a named element (e.g., \code{param}, \code{ic}, \code{driver})
#' storing an R object. Metadata can be used for provenance, units, 
#' or other information.
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
  x <- .new_model_input(slots, metadata)
  validate_model_input(x)
  
  return(x)
}


#' Internal constructor for ModelInput
#' 
#' Instantiates a \code{ModelInput} object. No validation is done in this
#' function. See \code{\link{ModelInput}} for the public interface.
#' 
#' @param slots Named list, each element representing an input value for a different
#'  input slot, with the names corresponding to the slot names.
#' @param metadata Named list (or empty list) of metadata to store alongside
#'   the slots.
#' 
#' @seealso \code{\link{ModelInput}}
#' @author Andrew Roberts
.new_model_input <- function(slots, metadata) {
  
  structure(list(slots=slots, metadata=metadata),
            class = "ModelInput")
}


#' Check if object inherits from \code{ModelInput}
#' 
#' @param x An object
#' @returns Logical, whether or not the object inherits from \code{ModelInput}.
#' 
#' @seealso \code{\link{ModelInput}}
#' @author Andrew Roberts
#' @export
is_model_input <- function(x) {
  inherits(x, "ModelInput")
}


#' Throw error if object is not \code{ModelInput}
#' 
#' @param x An object
#' @returns Invisibly returns \code{TRUE} if \code{x} is a \code{ModelInput}.
#'  Otherwise throws an error.
#' 
#' @seealso \code{\link{ModelInput}}
#' @author Andrew Roberts
#' @export
check_model_input_type <- function(x) {
  if(!is_model_input(x)) stop("`x` is not a ModelInput object.")
  
  invisible(TRUE)
}


#' Validate ModelInput
#'
#' Validates the general structure of a \code{ModelInput} object. Does not 
#' perform validation for the actual values stored in the object, which is
#' the job of model-specific validation.
#'
#' @details
#' Must have elements \code{slots} and \code{metadata}. Both of these must
#' be named lists, or empty lists. 
#'
#' @param x A \code{ModelInput} object.
#' @return Invisibly returns \code{TRUE} if validation tests are passed, 
#'  or throws an error if invalid.
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
  
  invisible(TRUE)
}


#' Slot Names Generic
#'
#' Returns the names of slots (input fields) present in a model input object.
#' Defined for both \code{ModelInput} and \code{EnsembleInput}.
#'
#' @param x A \code{ModelInput} or \code{EnsembleInput} object.
#' @param ... Further arguments passed to methods.
#'
#' @return Typically a character vector of slot names. See specific methods 
#'  for details.
#' @seealso \code{\link{slot_names.ModelInput}}, \code{\link{slot_names.EnsembleInput}}
#' @export
slot_names <- function(x, ...) {
  UseMethod("slot_names")
}


#' @export
slot_names.default <- function(x, ...) {
  raise_default_method_error(x, "slot_names")
}


#' Return slot names of a ModelInput
#'
#' Returns the names of slots (input fields) present in a model input object.
#'
#' @param x A \code{ModelInput}.
#' @param ... Not used.
#'
#' @return character vector of slot names.
#' @seealso \code{\link{slot_names.EnsembleInput}}
#' @export
slot_names.ModelInput <- function(x, ...) {
  names(x$slots)
}


#' Number of Slots Generic
#'
#' Returns the number of slots (input fields) present in a model input object.
#' Defined for both single \code{ModelInput} and \code{EnsembleInput}.
#'
#' @param x A \code{ModelInput} or \code{EnsembleInput} object.
#' @param ... Further arguments passed to methods.
#'
#' @return Integer, number of slots.
#' @seealso \code{\link{n_slots.ModelInput}}, \code{\link{n_slots.EnsembleInput}}
#' 
#' @author Andrew Roberts
#' @export
n_slots <- function(x, ...) {
  UseMethod("n_slots")
}


#' @export
n_slots.default <- function(x, ...) {
  raise_default_method_error(x, "n_slots")
}


#' Return Number of Slots in a ModelInput
#'
#' Returns the number of slots (input fields) present in a \code{ModelInput} object.
#' This is the length of the \code{slots} field (a list) stored in the object.
#'
#' @param x A \code{ModelInput} object.
#' @param ... Not used.
#'
#' @return Integer, number of slots. Throws error if \code{x} is not a
#'  \code{ModelInput}.
#' @seealso \code{\link{ModelInput}}, \code{\link{n_slots.EnsembleInput}}
#'
#' @author Andrew Roberts
#' @export
n_slots.ModelInput <- function(x, ...) {
  length(x$slots)
}


#' Extract slots from a ModelInput
#' 
#' Returns the \code{slots} field (a named list) of a \code{ModelInput} object.
#'
#' @param x A \code{ModelInput} object.
#' @return The \code{slots} list. Throws error if \code{x} is not a
#'  \code{ModelInput}.
#'
#' @author Andrew Roberts
#' @export
slots <- function(x) {
  check_model_input_type(x)
  x$slots
}


#' Extract metadata from a ModelInput
#' 
#' Returns the \code{metadata} field (a named list) of a \code{ModelInput} object.
#'
#' @param x A \code{ModelInput} object.
#' @return The \code{metadata} list. Throws error if \code{x} is not a
#'  \code{ModelInput}.
#'
#' @author Andrew Roberts
#' @export
metadata <- function(x) {
  check_model_input_type(x)
  x$metadata
}


#' Metadata Names Generic
#'
#' @param x A \code{ModelInput} object.
#' @return Used to extract the names of metadata elements associated with model
#'  input(s). See class-specific methods for specifics. If there is no metadata,
#'  should return \code{character(0)}.
#'  
#' @sealso \code{\link{metadata_names.ModelInput}}
#' 
#' @author Andrew Roberts
#' @export
metadata_names <- function(x, ...) {
  UseMethod("metadata_names")
}


#' @export
metadata_names.default <- function(x, ...) {
  raise_default_method_error(x, "metadata_names")
}


#' Extract metadata names from a ModelInput
#' 
#' Returns the character vector of metadata names for a \code{ModelInput}
#' object. These are the names of the \code{metadata} field.
#'
#' @param x A \code{ModelInput} object.
#' @return The names of the \code{metadata} list. Will be \code{character(0)} 
#'  if there is no metadata.
#'  
#' @author Andrew Roberts
#' @export
metadata_names.ModelInput <- function(x, ...) {
  nm <- names(x$metadata)
  
  if(is.null(nm)) character(0)
  else nm
}


#' Add Slot Generic
#'
#' Adds a new slot with name specified by \code{name}. The value for this slot
#' is set to \code{value}, which defaults to \code{NULL}.
#' Defined for both single \code{ModelInput} and \code{EnsembleInput}.
#'
#' @param x A \code{ModelInput} or \code{EnsembleInput} object.
#' @param name character(1), the name of the slot. Cannot conflict with existing
#'  slot names.
#' @param value An R object, the value to assign to the new slot. Defaults to 
#'  \code{NULL}.
#' @param ... Further arguments passed to methods.
#'
#' @returns \code{x}, with the new slot added.
#' @seealso \code{\link{add_slot.ModelInput}}
#' 
#' @author Andrew Roberts
#' @export
add_slot <- function(x, name, value=NULL, ...) {
  UseMethod("add_slot")
}


#' @export
add_slot.default <- function(x, name, value=NULL, ...) {
  raise_default_method_error(x, "add_slot")
}


#' Add New Slot to a ModelInput
#'
#' Adds a new slot to a \code{ModelInput} object with name specified by 
#' \code{name}. The value for this slot is set to \code{value}, which defaults 
#' to \code{NULL}. Defined for both single \code{ModelInput} and \code{EnsembleInput}.
#'
#' @param x A \code{ModelInput} object.
#' @param name character(1), the name of the slot. Cannot conflict with existing
#'  slot names.
#' @param value An R object, the value to assign to the new slot. Defaults to 
#'  \code{NULL}.
#' @param ... Further arguments passed to methods.
#'
#' @returns \code{x}, with the new slot added.
#' 
#' @author Andrew Roberts
#' @export
add_slot.ModelInput <- function(x, name, value=NULL, ...) {
  
  if(name %in% slot_names(x)) {
    stop("Slot `", name, "` already exists.")
  }
  
  x$slots[[name]] <- value
  validate_model_input(x)
  
  return(x)
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
