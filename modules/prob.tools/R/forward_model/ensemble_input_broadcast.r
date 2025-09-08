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


#' Construct ensemble model input in broadcast format
#' 
#' Constructs an object of class \code{EnsembleInputBroadcast}, which consists
#' of a set of unique values for each slot, along with a matrix containing 
#' indices of these values that encodes how to construct the corresponding 
#' \code{EnsembleInputTable}.
#'
#' @param idx_mat An integer matrix of dimension \code{(n_runs, n_slots)}.
#'  The \code{(i,j)} element contains the index of the list \code{slots[[j]]},
#'  which is the value to use in the jth slot for the ith run.
#' @param slots A named list of slot value sets. The jth element of \code{slots}
#'  is itself a list, containing a unique set of values for that slot.
#' @return An object of class \code{EnsembleInputBroadcast}, inheriting from 
#'  \code{EnsembleInput}.
#'  
#' @author Andrew Roberts
#' @export
EnsembleInput.matrix <- function(idx_mat, slots) {
  
  x <- structure(list(idx_mat=idx_mat, slots=slots, rule=NULL),
                 class = c("EnsembleInputBroadcast", "EnsembleInput"))
  
  validate_ensemble_input_broadcast(x)
  
  return(x)
}


#' Construct ensemble model input in broadcast format
#' 
#' Constructs an object of class \code{EnsembleInputBroadcast}, which consists
#' of a set of unique values for each slot, along with a broadcast rule that
#' encodes how to construct the corresponding \code{EnsembleInputTable}.
#' 
#' @details
#' This is an alternative to \code{\link{EnsembleInput.matrix}}, which instead
#' takes a matrix of indices that describes how to construct the ensemble table.
#' This constructor instead takes a broadcast rule, which is used to construct
#' this matrix. Let \code{lens <- sapply(slots, length)} denote the length 
#' of each slot dimension. When called like \code{broadcast_rule(lens)}, the 
#' broadcast rule must return an integer matrix of shape \code{(n_runs, n_slots)}.
#' The \code{(i,j)} element contains the index of the list \code{slots[[j]]},
#' which is the value to use in the jth slot for the ith run.
#'
#' @param broadcast_rule A broadcast rule function. See details for requirements.
#' @param slots A named list of slot value sets. The jth element of \code{slots}
#'  is itself a list, containing a unique set of values for that slot.
#'  
#' @return An object of class \code{EnsembleInputBroadcast}, inheriting from 
#'  \code{EnsembleInput}.
#'  
#' @author Andrew Roberts
#' @export
EnsembleInput.function <- function(broadcast_rule, slots) {

  if(!is_named_or_empty_list(slots)) {
    stop("`slots` must be named or empty list.")
  }
  
  lens <- vapply(slots, length, integer(1))
  x <- structure(list(idx_mat=broadcast_rule(lens), slots=slots, rule=broadcast_rule),
                 class = c("EnsembleInputBroadcast", "EnsembleInput"))
  
  validate_ensemble_input_broadcast(x)
  
  return(x)
}
  

is_ensemble_input_broadcast <- function(x) {
  is_ensemble_input(x) && inherits(x, "EnsembleInputBroadcast")
}


check_ensemble_input_broadcast_type <- function(x) {
  if (!is_ensemble_input_broadcast(x)) {
    stop("`x` is not an EnsembleInputBroadcast object.")
  }
}


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
  
  invisible(x)
}


run_ids.EnsembleInputBroadcast <- function(x, ...) {
  message("EnsembleInputBroadcast does not yet support custom run_ids. Autogenerating run_ids.")
  
  paste0("run_", seq_len(nrow(x$idx_mat)))
}


slot_names.EnsembleInputBroadcast <- function(x, ...) {
  names(x$slots)
}


metadata_names.EnsembleInputBroadcast <- function(x, ...) {
  .NotYetImplemented()
}


get_labeled_idx_mat <- function(x) {
  check_ensemble_input_broadcast_type(x)
  visualize_slot_grid(x$idx_mat, slot_names(x))
}


as_ensemble_input_broadcast <- function(x, ...) {
  UseMethod("as_ensemble_input_broadcast")
}


#' @export
as_ensemble_input_broadcast.default <- function(x, ...) {
  stop("as_ensemble_input_broadcast() is not implemented for objects of class ", 
       paste(class(x), collapse = "/"))
}


as_ensemble_input_broadcast.EnsembleInputBroadcast <- function(x, ...) {
  x
}


