# forward_model/ensemble_input.r

#' Base class for ensemble model input
#'
#' This is an abstract parent class that defines the interface
#' for ensemble model input objects. Subclasses include:
#' - \code{ensemble_input_list}
#' - \code{ensemble_input_table}
#' - \code{ensemble_input_broadcast}
#'
#' @param x An object to test.
#' @return Logical, whether `x` inherits from \code{EnsembleInput}.
#' @export
EnsembleInput <- function(x, ...) {
  UseMethod("EnsembleInput")
}


EnsembleInput.default <- function(x, ...) {
  stop("No EnsembleInput constructor is implemented for objects of class ", 
       paste(class(x), collapse = "/"))
}


is_ensemble_input <- function(x) {
  inherits(x, "EnsembleInput")
}


check_ensemble_input_type <- function(x) {
  if (!is_ensemble_input(x)) {
    stop("`x` is not an EnsembleInput object.")
  }
}


#' Access input slot names
#'
#' Returns the names of slots (input fields) present in the \code{ModelInput}
#' objects making up the ensemble run.
#'
#' @param x A code{EnsembleInput} object.
#' @param unique_only Logical; if \code{TRUE} (default), returns only the
#'   unique set of slot names across runs. If \code{FALSE}, returns
#'   per-run slot names (a list). Makes no difference for \code{ModelInput} objects.
#' @param ... Further arguments passed to methods.
#'
#' @return A character vector of slot names if \code{unique_only = TRUE},
#'   otherwise a list of character vectors (per run).
#' @export
slot_names.EnsembleInput <- function(x, unique_only=TRUE, ...) {
  slot_names_per_run <- lapply(as_ensemble_input_list(x)$inputs, slot_names)
  if(unique_only) unique(unlist(slot_names_per_run, use.names=FALSE)) 
  else slot_names_per_run
}


#' Run IDs Generic
#'
#' Returns a character vector of length equal to the number of runs, where each
#' value is the run ID for the respective run. If run IDs are not stored, then
#' the indices (converted to character) of the runs are returned.
#'
#' @param x A \code{EnsembleInput} object.
#' @param ... Further arguments passed to methods.
#'
#' @return A character vector of slot names if \code{unique_only = TRUE},
#'   otherwise a list of character vectors (per run).
#' @export
run_ids <- function(x, ...) {
  UseMethod("run_ids")
}


#' @export
run_ids.default <- function(x, ...) {
  stop("run_ids() is not implemented for objects of class ", 
       paste(class(x), collapse = "/"))
}


#' @export
n_slots.EnsembleInput <- function(x, ...) {
  length(slot_names(x, unique_only=TRUE))
}


n_runs <- function(x) {
  check_ensemble_input_type(x)
  length(run_ids(x))
}


#' Dimension of ensemble model input
#'
#' Returns the number of runs and number of slots.
#'
#' @param x An \code{EnsembleInput} object.
#' @param ... Additional arguments (ignored).
#' @return Integer vector of length 2: c(n_runs, n_slots)
#' @export
dim.EnsembleInput <- function(x, ...) {
  c(n_runs=n_runs(x), n_slots=n_slots(x))
}


#
# Old
#


#' #' @export
#' as_list.EnsembleInputTable <- function(x) {
#'   inputs <- lapply(seq_len(nrow(x$index)), function(i) {
#'     vals <- mapply(`[`, x$slots, x$index[i, ], SIMPLIFY = FALSE)
#'     structure(list(slots = vals), class = "model_input")
#'   })
#'   ensemble_input_list(inputs)
#' }
#' 
#' 
#' #' @export
#' as_list.EnsembleInputBroadcast <- function(x) {
#'   as_list(EnsembleInputTable(x$slots, x$index))
#' }




#' Head of ensemble model input
#'
#' Returns a tibble (or data.frame) showing the first `n` runs of
#' an ensemble model input with actual slot values.
#'
#' @param x An `ensemble_input` object.
#' @param n Number of runs to show. Default is 6.
#' @param as_tibble Logical; if TRUE and tibble is available, return a tibble.
#' @param ... Additional arguments (ignored).
#' @return A data.frame or tibble with one row per run and one column per slot.
#' @export
head.EnsembleInput <- function(x, n = 6L, as_tibble = TRUE, ...) {
  stopifnot(is_ensemble_input(x))
  
  # Ensure table representation
  tbl <- as_table(x)
  
  n <- min(n, nrow(tbl$index))
  idx <- tbl$index[seq_len(n), , drop = FALSE]
  
  df <- as.data.frame(
    lapply(seq_along(tbl$slots), function(k) {
      vapply(seq_len(nrow(idx)), function(i) {
        tbl$slots[[k]][[idx[i, k]]]
      }, FUN.VALUE = tbl$slots[[k]][[1]])
    }),
    stringsAsFactors = FALSE
  )
  
  names(df) <- names(tbl$slots)
  
  if (as_tibble && requireNamespace("tibble", quietly = TRUE)) {
    df <- tibble::as_tibble(df)
  }
  
  df
}


#' Tail of ensemble model input
#'
#' Returns a tibble (or data.frame) showing the last `n` runs of
#' an ensemble model input with actual slot values.
#'
#' @param x An `ensemble_input` object.
#' @param n Number of runs to show. Default is 6.
#' @param as_tibble Logical; if TRUE and tibble is available, return a tibble.
#' @param ... Additional arguments (ignored).
#' @return A data.frame or tibble with one row per run and one column per slot.
#' @export
tail.EnsembleInput <- function(x, n = 6L, as_tibble = TRUE, ...) {
  stopifnot(is_ensemble_input(x))
  
  tbl <- as_table(x)
  
  n <- min(n, nrow(tbl$index))
  idx <- tbl$index[(nrow(tbl$index)-n+1):nrow(tbl$index), , drop = FALSE]
  
  df <- as.data.frame(
    lapply(seq_along(tbl$slots), function(k) {
      vapply(seq_len(nrow(idx)), function(i) {
        tbl$slots[[k]][[idx[i, k]]]
      }, FUN.VALUE = tbl$slots[[k]][[1]])
    }),
    stringsAsFactors = FALSE
  )
  
  names(df) <- names(tbl$slots)
  
  if (as_tibble && requireNamespace("tibble", quietly=TRUE)) {
    df <- tibble::as_tibble(df)
  }
  
  df
}

