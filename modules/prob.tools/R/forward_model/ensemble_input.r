# forward_model/ensemble_input.r

#' Base class for ensemble model input
#'
#' The base parent class for \code{EnsembleInput} objects. Subclasses include:
#' - \code{EnsembleInputList}
#' - \code{EnsembleInputTable}
#' - \code{EnsembleInputBroadcast}
#'
#' @details
#' An \code{EnsembleInput} is simply an ordered collection of \code{ModelInput}
#' objects, with run IDs defined for each run. Optionally, metadata related
#' to the ensemble as a whole can also be stored (it is recommended to store
#' run-dependent metadata in the \code{ModelInput} objects themselves). The
#' \code{EnsembleInput()} constructor is a generic that will dispatch to either
#' \code{EnsembleInput.EnsembleInputList()}, 
#' \code{EnsembleInput.EnsembleInputTable()}, or
#' \code{EnsembleInput.EnsembleInputBroadcast()} depending on the input type.
#' 
#'
#' @param x An object that can be used to define an \code{EnsembleInput}.
#' @return An \code{EnsembleInput} object.
#' 
#' @seealso \code{\link{EnsembleInput.EnsembleInputList()}},
#'  \code{\link{EnsembleInput.EnsembleInputTable()}},
#'  \code{\link{EnsembleInput.EnsembleInputBroadcast()}}
#' 
#' @author Andrew Roberts
#' @export
EnsembleInput <- function(x, ...) {
  UseMethod("EnsembleInput")
}


#' @export
EnsembleInput.default <- function(x, ...) {
  raise_default_method_error(x, "EnsembleInput")
}


#' Check if object inherits from \code{EnsembleInput}
#' 
#' @param x An object
#' @returns Logical, whether or not the object inherits from \code{EnsembleInput}.
#' 
#' @seealso \code{\link{EnsembleInput}}
#' @author Andrew Roberts
#' @export
is_ensemble_input <- function(x) {
  inherits(x, "EnsembleInput")
}


#' Throw error if object is not \code{EnsembleInput}
#' 
#' @param x An object
#' @returns Invisibly returns \code{TRUE} if \code{x} is an \code{EnsembleInput}.
#'  Otherwise throws an error.
#' 
#' @seealso \code{\link{EnsembleInput}}
#' @author Andrew Roberts
#' @export
check_ensemble_input_type <- function(x) {
  if (!is_ensemble_input(x)) stop("`x` is not an EnsembleInput object.")
  
  invisible(TRUE)
}


#' Return slot names of an EnsembleInput
#'
#' Returns the names of slots (input fields) present in the \code{ModelInput}
#' objects making up the ensemble run. The individual inputs may have different
#' slot names. By default, this method returns the union of all slot names
#' (i.e., the unique set of slot names over all \code{ModelInput}s). If 
#' \code{unique_only = FALSE} then returns a list of the slot names of each
#' individual \code{ModelInput} object.
#'
#' @param x An code{EnsembleInput} object.
#' @param unique_only Logical; if \code{TRUE} (default), returns only the
#'   unique set of slot names across runs. If \code{FALSE}, returns a list of 
#'   length \code{n_runs(x)} containing the slot names for each model input.
#' @param ... Not used.
#'
#' @return A character vector of slot names if \code{unique_only = TRUE},
#'   otherwise a list of character vectors (per run/input).
#' @seealso \code{\link{slot_names.ModelInput}}
#'   
#' @author Andrew Roberts
#' @export
slot_names.EnsembleInput <- function(x, unique_only=TRUE, ...) {
  slot_names_per_run <- lapply(as_ensemble_input_list(x)$inputs, slot_names)
  
  if(unique_only) unique(unlist(slot_names_per_run, use.names=FALSE)) 
  else slot_names_per_run
}


#' Run IDs Generic
#'
#' Returns a character vector of length equal to the number of runs, where each
#' value is the run ID for the respective run. Since each run is defined by 
#' a particular model input, then these run IDs can also be thought of as 
#' "model input IDs".
#'
#' @param x An \code{EnsembleInput} object.
#' @param ... Further arguments passed to methods.
#'
#' @return A character vector of run IDs of length \code{n_runs(x)}.
#' 
#' @author Andrew Roberts
#' @export
run_ids <- function(x, ...) {
  UseMethod("run_ids")
}


#' @export
run_ids.default <- function(x, ...) {
  raise_default_method_error(x, "run_ids")
}


#' Return number of slots in an EnsembleInput
#'
#' Returns the number of slots (input fields) present in an \code{EnsembleInput}
#' object. 
#' 
#' @details
#' As the model inputs comprising \code{EnsembleInput} can contain varying
#' numbers of slots, the number of slots of an \code{EnsembleInput} is defined
#' as the number of elements in the union of all model input slots (i.e., 
#' the total number of unique slots). This also corresponds to the number of
#' "slot columns" in an \code{\link{EnsembleInputTable}}.
#' However, note that \code{EnsembleInputTable} will always have more columns
#' than \code{n_slots(x)} due to the presence of the \code{run_id} column and
#' possibly additional metadata columns.
#'
#' @param x An \code{EnsembleInput} object.
#' @param ... Not used.
#'
#' @return Integer, number of slots.
#' 
#' @author Andrew Roberts
#' @export
n_slots.EnsembleInput <- function(x, ...) {
  length(slot_names(x))
}


#' Return total number of inputs (i.e., runs) in an EnsembleInput
#'
#' Returns the number of model inputs comprising an \code{EnsembleInput}
#' object. Each model input defines a "run"; hence, this is also the total
#' number of runs.
#'
#' @param x An \code{EnsembleInput} object.
#' @param ... Not used.
#'
#' @return Integer, number of runs. Throws error if \code{x} is not an 
#'  \code{EnsembleInput}.
#' 
#' @author Andrew Roberts
#' @export
n_runs <- function(x) {
  check_ensemble_input_type(x)
  length(run_ids(x))
}


#' Dimension of ensemble model input
#'
#' Returns a tuple containing the the number of runs and number of slots
#' contained within an \code{EnsembleInput}. This corresponds to the row and
#' column dimensions when the ensemble input is stored in a tabular format.
#' However, note that \code{EnsembleInputTable} will always have more columns
#' than \code{n_slots(x)} due to the presence of the \code{run_id} column and
#' possibly additional metadata columns.
#'
#' @param x An \code{EnsembleInput} object.
#' @param ... Not used
#' @return Named integer vector of length 2: c(n_runs = n_runs(x), n_slots = n_slots(x)).
#'
#' @author Andrew Roberts
#' @export
dim.EnsembleInput <- function(x, ...) {
  c(n_runs=n_runs(x), n_slots=n_slots(x))
}


#' Generic Getter for ModelInput for Specific Run 
#'
#' Returns the \code{ModelInput} object for run identified by the specified
#' \code{run_id}. 
#'
#' @param x An \code{EnsembleInput}
#' @param run_id character(1), the run ID.
#' @param ... Further arguments passed to methods.
#'
#' @return The \code{ModelInput} for the selected run. Throws error if 
#'  \code{run_id} is not found.
#' 
#' @author Andrew Roberts
#' @export
get_run_input <- function(x, run_id, ...) {
  UseMethod("get_run_input")
}


#' @export
get_run_input.default <- function(x, run_id, ...) {
  raise_default_method_error(x, "get_run_input")
}


#' Summarize an EnsembleInput
#'
#' Provide a unified summary of an \code{EnsembleInput} object, irrespective of
#' of the underlying data structure (list, table, broadcast). The convention 
#' us for \code{summarize()} to provide a data structure independent summary,
#' while \code{print()} may differ based on the particular sub-class.
#'
#' @returns Invisbly returns \code{x}. Prints a summary to standard output.
#' @author Andrew Roberts
#' @export
summary.EnsembleInput <- function(x, ...) {
  cat("<", class(x)[1], ">\n", sep="")
  cat(" Number of runs:", n_runs(x), "\n")
  cat(" Number of slots:", n_slots(x), "\n")
  
  slot_nm <- slot_names(x)
  if(length(slot_nm) == 0L) {
    cat("  (no slots)\n")
  } else {
    cat(" slots:", paste(slot_nm, collapse = ", "), "\n")
  }
  
  invisible(x)
}


#' Error when a requested run ID is not present
#' 
#' @author Andrew Roberts
raise_run_id_not_found_error <- function(run_id) {
  stop("Run ID `", run_id, "` not found in EnsembleInput.")
}




