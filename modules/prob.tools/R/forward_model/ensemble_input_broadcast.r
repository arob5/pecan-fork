# forward_model/ensemble_input_broadcast.r

# The broadcast format for an `EnsembleInput` is typically the most 
# efficient data structure, relative to the list and table formats. An
# `EnsembleInputBroadcast` object is defined by a unique set of values per slot,
# in addition to a `(n_runs, n_slots)` matrix of indices describing how the 
# slot values map to particular runs. Instead of explicitly passing this matrix,
# an `EnsembleInputBroadcast` object can be constructed by passing a 
# "broadcast rule" function that produces this matrix.

# `EnsembleInputBroadcast` currently does not support run metadata or custom
# run IDs. It also currently requires all runs to have the same slots. All
# of these can eventually be accomodated. For the latter, `idx_mat` could be
# allowed to have NA values.


#' Construct ensemble model input in broadcast format from index matrix
#' 
#' Constructs an object of class \code{EnsembleInputBroadcast}, which consists
#' of a set of unique values for each slot, along with a matrix containing 
#' indices of these values that encodes how to construct the corresponding 
#' \code{EnsembleInputTable}.
#' 
#' @details
#' An \code{EnsembleInputBroadcast} is defined by two fields: \code{slots} and
#' \code{idx_mat}. The former is a named list of input fields/types ("slots").
#' The names defined the slot names. The values must themselves be lists 
#' containing a unique set of values for each slot. \code{idx_mat} is an
#' integer matrix of shape \code{(n_runs, n_slots)}. The \code{(i,j)} element 
#' contains the index of the list \code{slots[[j]]}, which is the value to use 
#' in the jth slot for the ith run. While this index matrix can be defined
#' by hand, it is typically more convenient and self-documenting to instead
#' implicitly define this matrix via a "broadcast rule". See the alternative
#' constructor \code{\link{EnsembleInput.function}} for more information.
#'
#' @param idx_mat An integer matrix of dimension \code{(n_runs, n_slots)}.
#'  The \code{(i,j)} element contains the index of the list \code{slots[[j]]},
#'  which is the value to use in the jth slot for the ith run.
#' @param slots A named list of slot value sets. The jth element of \code{slots}
#'  is itself a list, containing a unique set of values for that slot.
#'  
#' @return An object of class \code{EnsembleInputBroadcast}, inheriting from 
#'  \code{EnsembleInput}.
#' @seealso \code{\link{EnsembleInput.function}}
#'
#' @author Andrew Roberts
#' @export
EnsembleInput.matrix <- function(idx_mat, slots) {
  
  x <- .new_ensemble_input_broadcast(slots, idx_mat=idx_mat,
                                     broadcast_rule=NULL)
  validate_ensemble_input_broadcast(x)

  return(x)
}


#' Construct ensemble model input in broadcast format from broadcast rule
#' 
#' Constructs an object of class \code{EnsembleInputBroadcast}, which consists
#' of a set of unique values for each slot, along with a broadcast rule that
#' encodes how to construct the corresponding \code{EnsembleInputTable}.
#' 
#' @details
#' This is an alternative to \code{\link{EnsembleInput.matrix}}, which instead
#' takes a matrix of indices that describes how to construct the ensemble table
#' (see \code{\link{EnsembleInput.matrix}} for more detailed documentation on
#' the definition of an \code{EnsembleInputBroadcast}).
#' This constructor instead takes a broadcast rule, which is used to construct
#' \code{idx_mat}. Let \code{lens <- sapply(slots, length)} denote the length 
#' of each slot dimension. When called like \code{broadcast_rule(lens)}, the 
#' broadcast rule must return an integer matrix of shape \code{(n_runs, n_slots)}.
#' The \code{(i,j)} element contains the index of the list \code{slots[[j]]},
#' which is the value to use in the jth slot for the ith run. 
#' 
#' Most common input combination patterns can be reproduced by combining a few
#' simple rules. See the broadcast rule documentation 
#' (e.g., \code{\link{get_composite_rule}}) for common rules that have already
#' been implemented, and functions for combining them.
#'
#' @param broadcast_rule A broadcast rule function. See details for requirements.
#' @param slots A named list of slot value sets. The jth element of \code{slots}
#'  is itself a list, containing a unique set of values for that slot.
#'  
#' @return An object of class \code{EnsembleInputBroadcast}, inheriting from 
#'  \code{EnsembleInput}.
#' @seealso \code{\link{EnsembleInput.matrix}}, \code{\link{rule_cartesian}},
#'  \code{\link{rule_match}}, \code{\link{rule_broadcast}}, \code{\link{rule_recycle}} 
#'
#' @author Andrew Roberts
#' @export
EnsembleInput.function <- function(broadcast_rule, slots) {

  x <- .new_ensemble_input_broadcast(slots, idx_mat=NULL,
                                     broadcast_rule=broadcast_rule)
  validate_ensemble_input_broadcast(x)
  
  return(x)
}
  

#' Internal constructor for EnsembleInputBroadcast
#' 
#' Instantiates an \code{EnsembleInputBroadcast} object. Limited validation is 
#' done in this function. See \code{\link{EnsembleInput.matrix}} for the 
#' public interface and additional documentation.
#' 
#' @details
#' Exactly one of \code{idx_mat} or \code{broadcast_rule} must be non-\code{NULL}.
#' If the latter is provided, then \code{idx_mat} is constructed using the
#' broadcast rule.
#' 
#' @param slots A named list of slot value sets. The jth element of \code{slots}
#'  is itself a list, containing a unique set of values for that slot.
#' @param idx_mat An integer matrix of dimension \code{(n_runs, n_slots)}.
#'  The \code{(i,j)} element contains the index of the list \code{slots[[j]]},
#'  which is the value to use in the jth slot for the ith run.
#' @param broadcast_rule A broadcast rule function. See details for requirements.
#' 
#' @returns An object of class \code{EnsembleInputBroadcast}, inheriting from 
#'  \code{EnsembleInput}.
#' 
#' @seealso \code{\link{EnsembleInput.matrix}}, \code{\link{EnsembleInput.function}}
#' @author Andrew Roberts
.new_ensemble_input_broadcast <- function(slots, idx_mat, broadcast_rule) {

  if(!is_named_or_empty_list(slots)) {
    stop("`slots` must be named or empty list.")
  }

  if(!xor(is.null(idx_mat), is.null(broadcast_rule))) {
    stop("Exactly one of `idx_mat` and `broadcast_rule` must be NULL.")
  }
  
  # Construct the index matrix by applying the broadcast rule.
  if(is.null(idx_mat)) idx_mat <- .broadcast_slots(slots, broadcast_rule)
  
  
  structure(list(slots=slots, idx_mat=idx_mat, rule=broadcast_rule),
            class = c("EnsembleInputBroadcast", "EnsembleInput"))
}


#' Check if object inherits from \code{EnsembleInputBroadcast}
#' 
#' @param x An object
#' @returns Logical, whether or not the object inherits from \code{EnsembleInputBroadcast}.
#' 
#' @author Andrew Roberts
#' @export
is_ensemble_input_broadcast <- function(x) {
  is_ensemble_input(x) && inherits(x, "EnsembleInputBroadcast")
}


#' Throw error if object is not an \code{EnsembleInputBroadcast}
#' 
#' @param x An object
#' @returns Invisibly returns \code{TRUE} if \code{x} is an \code{EnsembleInputBroadcast}.
#'  Otherwise throws an error.
#' 
#' @seealso \code{\link{EnsembleInput}}
#' @author Andrew Roberts
#' @export
check_ensemble_input_broadcast_type <- function(x) {
  if (!is_ensemble_input_broadcast(x)) {
    stop("`x` is not an EnsembleInputBroadcast object.")
  }
  
  invisible(TRUE)
}


#' Validate an EnsembleInputBroadcast
#'
#' Validates the general structure of a \code{EnsembleInputBroadcast} object. 
#'
#' @details
#' Must contain \code{slots}, \code{idx_mat}, and \code{rule} fields.
#' \code{slots} must be a named list (or empty list). All elements of 
#' \code{slots} must themselves be lists. \code{idx_mat} must be an integer 
#' matrix with number of cols equal to \code{length(slots)}. The values in 
#' column \code{j} of \code{idx_mat} must be integers between 1 and
#' \code{length(slots[[j]])}. \code{rule} must be a function, or \code{NULL}.
#'
#' @param x An object.
#' @return Invisibly returns \code{TRUE} if validation tests are passed, 
#'  or throws an error if invalid.
#'  
#' @author Andrew Roberts
#' @export
validate_ensemble_input_broadcast <- function(x) {
  
  check_ensemble_input_broadcast_type(x)
  
  if(!("idx_mat" %in% names(x))) {
    stop("`EnsembleInputBroadcast$idx_mat` list is missing.")
  }
  
  if(!("slots" %in% names(x))) {
    stop("`EnsembleInputBroadcast$slots` list is missing.")
  }
  
  if(!("rule" %in% names(x))) {
    stop("`EnsembleInputBroadcast$rule` list is missing.")
  }
  
  if(!is_named_or_empty_list(x$slots, check_unique_names=TRUE)) {
    stop("EnsembleInputBroadcast$slots must be a named list or empty list.")
  }
  
  if(!is.function(x$rule) && !is.null(x$rule)) {
    stop("EnsembleInputBroadcast$rule must be a function or NULL.")
  }
  
  if(!is.matrix(x$idx_mat) || !is_integer_like(x$idx_mat)) {
    stop("EnsembleInputBroadcast$idx_mat must be an integer matrix.")
  }
  
  if(!all(vapply(x$slots, is.list, logical(1)))) {
    stop("EnsembleInputBroadcast$slots must be a list of lists.")
  }
  
  if(ncol(x$idx_mat) != length(x$slots)) {
    stop("EnsembleInputBroadcast dimension mismatch between `idx_mat` and `slots`.")
  }
  
  if(ncol(x$idx_mat) > 0L) {
    for(j in seq_len(ncol(x$idx_mat))) {
      max_idx <- length(x$slots[[j]])
      if(!all(x$idx_mat[[j]] >= 1L & x$idx_mat[[j]] <= max_idx)) {
        stop("`idx_mat` contains invalid entries in column ", j,
             " Entries must be between 1 and length(slots[[j]]) = ", max_idx)
      }
    }
  }
  
  invisible(TRUE)
}


#' Return character vector of run IDs
#'
#' See \code{\link{run_ids}}
#'
#' @param An \code{EnsembleInputBroadcast}
#' @param ... Not used
#' 
#' @return A character vector of run IDs of length \code{n_runs(x)}.
#' @seealso \code{\link{run_ids}}
#' 
#' @author Andrew Roberts
#' @export
run_ids.EnsembleInputBroadcast <- function(x, ...) {
  message("EnsembleInputBroadcast does not yet support custom run_ids. Autogenerating run_ids.")
  
  paste0("run_", seq_len(nrow(x$idx_mat)))
}


#' Get input slot names
#'
#' Returns the names of slots (input fields) present in the \code{ModelInput}
#' objects making up the ensemble run.
#'
#' @param x An \code{EnsembleInputBroadcast} object.
#' @param unique_only Logical; if \code{TRUE} (default), returns only the
#'   unique set of slot names across runs. If \code{FALSE}, returns
#'   per-run slot names (a list).
#' @param ... Not used.
#'
#' @return A character vector of slot names if \code{unique_only = TRUE},
#'   otherwise a list of character vectors (per run).
#' @seealso \code{\link{slot_names}}, \code{\link{slot_names.ModelInput}}
#'   
#' @author Andrew Roberts
#' @export
slot_names.EnsembleInputBroadcast <- function(x, ...) {
  names(x$slots)
}


#' Get metadata names
#'
#' Returns the names of metadata fields present in the \code{ModelInput}
#' objects making up the ensemble run.
#'
#' @param x An \code{EnsembleInputBroadcast} object.
#' @param unique_only Logical; if \code{TRUE} (default), returns only the
#'   unique set of metadata names across runs. If \code{FALSE}, returns
#'   per-run metadata names (a list).
#' @param ... Not used.
#'
#' @return A character vector of metadata names if \code{unique_only = TRUE},
#'   otherwise a list of character vectors (per run). Returns character(0)
#'   in the absence of metadata.
#' @seealso \code{\link{metadata_names}}, \code{\link{metadata_names.ModelInput}}
#'   
#' @author Andrew Roberts
#' @export
metadata_names.EnsembleInputBroadcast <- function(x, ...) {
  .NotYetImplemented()
}


#' Convert an EnsembleInput to broadcast format
#'
#' Converts an \code{EnsembleInput} object (e.g., in list or table format)
#' into an \code{EnsembleInputBroadcast} object. The result will still inherit
#' from \code{EnsembleInput}.
#'
#' @param An \code{EnsembleInput}
#' @param ... Additional arguments to be used by methods.
#' 
#' @returns An \code{EnsembleInputBroadcast} object.
#' 
#' @author Andrew Roberts
#' @export
as_ensemble_input_broadcast <- function(x, ...) {
  UseMethod("as_ensemble_input_broadcast")
}


#' @export
as_ensemble_input_broadcast.default <- function(x, ...) {
  raise_default_method_error(x, "as_ensemble_input_broadcast")
}


#' Identity function - input is already an \code{EnsembleInputBroadcast}.
#' @export
as_ensemble_input_broadcast.EnsembleInputBroadcast <- function(x, ...) {
  x
}


#' Compute index matrix from broadcast rule
#' 
#' Convenience function to help visualize the ensemble inputs. In particular,
#' returns a matrix of the same shape as \code{x$idx_mat}, where the integer
#' indices have been replaced by character labels of the form 
#' \code{<slot_name>_j} for the jth value of a slot.
#' 
#' @param x An \code{EnsembleInputBroadcast}.
#'  
#' @returns character matrix. The \code{(i,j)} entry contains the value
#'  \code{paste(slot_names(x)[[j]], x$idx_mat[i,j], sep="_")}. Note that this
#'  is just a label that provides the index of the value within the slot. This
#'  is NOT the slot value itself (which may be a structured object, not just
#'  a string).
#'  
#' @author Andrew Roberts
#' @export
get_labeled_idx_mat <- function(x) {
  check_ensemble_input_broadcast_type(x)
  visualize_slot_grid(x$idx_mat, slot_names(x))
}


#' Compute index matrix from broadcast rule
#' 
#' Helper function which constructs \code{idx_mat} by applying the 
#' broadcast rule function to the slot list.
#' 
#' @param broadcast_rule A broadcast rule function. 
#' @param slots A named list of slot value sets. The jth element of \code{slots}
#'  is itself a list, containing a unique set of values for that slot.
#'  
#' @returns integer matrix with \code{length(slots)} columns, provided the 
#'  broadcast rule is valid.
#' @author Andrew Roberts
.broadcast_slots <- function(slots, broadcast_rule) {
  lens <- vapply(slots, length, integer(1))
  broadcast_rule(lens)
}
