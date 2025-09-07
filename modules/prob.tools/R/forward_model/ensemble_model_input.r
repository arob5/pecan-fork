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


# ------------------------------------------------------------------------------
# Constructors for sub-classes
# ------------------------------------------------------------------------------


#' Construct a table-based ensemble model input
#'
#' @param slots A named list of unique slot values.
#' @param index An integer matrix of dimension (n_runs, n_slots),
#'   where each row specifies indices into `slots`.
#' @return An object of class \code{EnsembleInputTable}.
#' @export
EnsembleInputTable <- function(slots, index) {
  stopifnot(is.list(slots), is.matrix(index))
  structure(
    list(slots = slots, index = index),
    class = c("EnsembleInputTable", "EnsembleInput")
  )
}


#' Construct a broadcast-based ensemble model input
#'
#' @param slots A named list of slot value sets.
#' @param rule A rule function that defines broadcasting,
#'   returning an index matrix.
#' @return An object of class \code{EnsembleInputBroadcast}.
#' @export
EnsembleInputBroadcast <- function(slots, rule) {
  stopifnot(is.list(slots), is.function(rule))
  lens <- vapply(slots, length, integer(1))
  idx <- rule(lens)
  structure(
    list(slots=slots, rule=rule, index=idx),
    class = c("EnsembleInputBroadcast", "EnsembleInput")
  )
}


# ------------------------------------------------------------------------------
# Generics for conversion to EnsembleInput
# ------------------------------------------------------------------------------


#' Convert to ensemble model input
#'
#' Generic coercion to `EnsembleInput`.
#' Subclasses can be converted between one another.
#'
#' @param x An object to convert.
#' @param ... Additional arguments.
#' @export
as_ensemble_input <- function(x, ...) {
  UseMethod("as_ensemble_input")
}

# Default: throw error
#' @export
as_ensemble_input.default <- function(x, ...) {
  stop("Cannot convert object of class ", class(x)[1], " to ensemble_input.")
}

# Identity: already parent
#' @export
as_ensemble_input.EnsembleInput <- function(x, ...) {
  x
}


# ------------------------------------------------------------------------------
# Helpers for conversion between sub-classes
# ------------------------------------------------------------------------------


#' Convert to list representation
#'
#' @param x An ensemble model input.
#' @return An `EnsembleInputList`.
#' @export
as_list <- function(x) {
  UseMethod("as_list")
}


#' @export
as_list.EnsembleInputList <- function(x) x


#' @export
as_list.EnsembleInputTable <- function(x) {
  inputs <- lapply(seq_len(nrow(x$index)), function(i) {
    vals <- mapply(`[`, x$slots, x$index[i, ], SIMPLIFY = FALSE)
    structure(list(slots = vals), class = "model_input")
  })
  ensemble_input_list(inputs)
}


#' @export
as_list.EnsembleInputBroadcast <- function(x) {
  as_list(EnsembleInputTable(x$slots, x$index))
}


# ------------------------------------------------------------------------------


#' Convert to table representation
#'
#' @param x An ensemble model input.
#' @return An \code{EnsembleInputTable}.
#' @export
as_table <- function(x) {
  UseMethod("as_table")
}


#' @export
as_table.EnsembleInputTable <- function(x) x


#' @export
as_table.EnsembleInputList <- function(x) {
  # Collapse list into slots + index
  slot_names <- names(x$inputs[[1]]$slots)
  slots <- lapply(slot_names, function(nm) {
    unique(lapply(x$inputs, function(mi) mi$slots[[nm]]))
  })
  names(slots) <- slot_names
  
  index <- do.call(rbind, lapply(x$inputs, function(mi) {
    vapply(slot_names, function(nm) {
      match(mi$slots[[nm]], slots[[nm]])
    }, integer(1))
  }))
  
  EnsembleInputTable(slots, index)
}

#' @export
as_table.EnsembleInputBroadcast <- function(x) {
  EnsembleInputTable(x$slots, x$index)
}


# ------------------------------------------------------------------------------


#' Convert to broadcast representation
#'
#' @param x An ensemble model input.
#' @param rule Optional broadcasting rule to reconstruct the index.
#'   If omitted, defaults to \code{rule_cartesian}.
#' @return An `#' Convert to broadcast representation
#'
#' @param x An ensemble model input.
#' @param rule Optional broadcasting rule to reconstruct the index.
#'   If omitted, defaults to \code{rule_cartesian}.
#' @return An \code{EnsembleInputBroadcast}.
#' @export
as_broadcast <- function(x, rule=rule_cartesian) {
  UseMethod("as_broadcast")
}


#' @export
as_broadcast.EnsembleInputBroadcast <- function(x, rule=rule_cartesian) x


#' @export
as_broadcast.EnsembleInputTable <- function(x, rule=rule_cartesian) {
  EnsembleInputBroadcast(x$slots, rule)
}


#' @export
as_broadcast.EnsembleInputList <- function(x, rule=rule_cartesian) {
  as_broadcast(as_table(x), rule=rule)
}


# ------------------------------------------------------------------------------
# Accessing slot names
# ------------------------------------------------------------------------------


#' # Example: table-based ensemble
#' #' @export
#' slot_names.EnsembleInputTable <- function(x, unique_only = TRUE, ...) {
#'   if (unique_only) colnames(x$table) else as.list(colnames(x$table))
#' }
#' 
#' # Example: broadcast-based ensemble
#' #' @export
#' slot_names.EnsembleInputBroadcast <- function(x, unique_only = TRUE, ...) {
#'   slots <- names(x$slot_values)
#'   if (unique_only) slots else as.list(slots)
#' }


# ------------------------------------------------------------------------------
# Print methods
# ------------------------------------------------------------------------------


#' @export
print.EnsembleInputTable <- function(x, ...) {
  cat("<EnsembleInputTable>\n")
  cat(" Number of runs:", nrow(x$index), "\n")
  cat(" Slots:\n")
  for (nm in names(x$slots)) {
    cat("  -", nm, "(", length(x$slots[[nm]]), "unique)\n")
  }
  invisible(x)
}


#' @export
print.EnsembleInputBroadcast <- function(x, ...) {
  cat("<EnsembleInputBroadcast>\n")
  cat(" Number of runs:", nrow(x$index), "\n")
  cat(" Slots:\n")
  for (nm in names(x$slots)) {
    cat("  -", nm, "(", length(x$slots[[nm]]), "values)\n")
  }
  cat(" Broadcasting rule:", deparse(body(x$rule))[1], "\n")
  invisible(x)
}


# ------------------------------------------------------------------------------
# Summary, visualization, and utility methods
# ------------------------------------------------------------------------------


#' @export
summary.EnsembleInputList <- function(object, ...) {
  n_runs <- length(object$inputs)
  cat("<EnsembleInputList summary>\n")
  cat(" Number of runs:", n_runs, "\n")
  if (n_runs > 0) {
    slot_names <- names(object$inputs[[1]]$slots)
    cat(" Slots (", length(slot_names), "): ", paste(slot_names, collapse = ", "), "\n", sep = "")
    cat(" Example of first run:\n")
    print(object$inputs[[1]]$slots)
  }
  invisible(object)
}

#' @export
summary.EnsembleInputTable <- function(object, ...) {
  n_runs <- nrow(object$index)
  cat("<EnsembleInputList summary>\n")
  cat(" Number of runs:", n_runs, "\n")
  cat(" Slots:\n")
  for (nm in names(object$slots)) {
    uvals <- length(object$slots[[nm]])
    cat("  -", nm, ":", uvals, "unique values\n")
  }
  if (n_runs > 0) {
    cat(" Example of first run:\n")
    vals <- mapply(`[`, object$slots, object$index[1, ], SIMPLIFY = FALSE)
    print(vals)
  }
  invisible(object)
}

#' @export
summary.EnsembleInputBroadcast <- function(object, ...) {
  n_runs <- nrow(object$index)
  cat("<EnsembleInputBroadcast summary>\n")
  cat(" Number of runs:", n_runs, "\n")
  cat(" Slots:\n")
  for (nm in names(object$slots)) {
    nvals <- length(object$slots[[nm]])
    cat("  -", nm, ":", nvals, "values\n")
  }
  cat(" Broadcasting rule body:\n")
  cat("  ", paste(deparse(body(object$rule)), collapse = "\n   "), "\n")
  if (n_runs > 0) {
    cat(" Example of first run:\n")
    vals <- mapply(`[`, object$slots, object$index[1, ], SIMPLIFY = FALSE)
    print(vals)
  }
  invisible(object)
}


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


#' Dimension of ensemble model input
#'
#' Returns the number of runs and number of slots.
#'
#' @param x An \code{EnsembleInput} object.
#' @param ... Additional arguments (ignored).
#' @return Integer vector of length 2: c(n_runs, n_slots)
#' @export
dim.EnsembleInput <- function(x, ...) {
  tbl <- as_table(x)
  c(n_runs=nrow(tbl$index), n_slots=length(tbl$slots))
}


#' Number of runs in ensemble model input
#'
#' @param x An \code{EnsembleInput} object.
#' @param ... Additional arguments (ignored).
#' @return Integer: number of runs
#' @export
length.EnsembleInput <- function(x, ...) {
  tbl <- as_table(x)
  nrow(tbl$index)
}



#
# TEMP
#

#' Coerce EnsembleInput to table representation
#'
#' Converts any \code{EnsembleInput} into a tibble-like data.frame
#' where each row corresponds to a run and each column corresponds to a slot.
#' If not all runs have the same slots, missing values are filled with \code{NA}.
#'
#' @param x An \code{EnsembleInput} (list, table, or broadcast).
#' @param simplify Logical; if TRUE (default), atomic or vector slots are simplified.
#'        If FALSE, all slots are stored as list-columns.
#'
#' @return A data.frame with one row per run and one column per slot.
#' @examples
#' mi1 <- model_input(param = 1, ic = list(x=0))
#' mi2 <- model_input(param = 2, driver = data.frame(time=1:3, val=1:3))
#' emb <- EnsembleInputList(list(mi1, mi2))
#'
#' coerce_to_table(emb)
#'
#' @export
coerce_to_table <- function(x, simplify = TRUE) {
  if (!inherits(x, "EnsembleInput")) {
    stop("Input must be an EnsembleInput.")
  }
  
  # Union of all slot names
  all_slots <- slot_names(x, unique_only = TRUE)
  
  # Materialize each run as a row
  rows <- lapply(as.list(x), function(mi) {
    row <- setNames(vector("list", length(all_slots)), all_slots)
    for (nm in names(mi)) {
      row[[nm]] <- mi[[nm]]
    }
    row
  })
  
  # Bind into data.frame with list-columns
  df <- do.call(rbind, lapply(rows, function(row) {
    as.data.frame(row, stringsAsFactors = FALSE)
  }))
  
  # Ensure list-columns for non-scalar objects
  for (nm in all_slots) {
    col <- lapply(rows, `[[`, nm)
    if (!simplify || any(vapply(col, function(x) length(x) != 1 || is.list(x), logical(1)))) {
      df[[nm]] <- I(col)
    } else {
      df[[nm]] <- unlist(col)
    }
  }
  
  df
}


#' Coerce to EnsembleInputTable
#'
#' Converts an \code{EnsembleInput} (list, table, or broadcast)
#' into a rectangular table representation.
#'
#' @param x An \code{EnsembleInput}.
#' @param simplify Logical; passed to \code{coerce_to_table}.
#'
#' @return An \code{EnsembleInputTable} object.
#' @export
as.EnsembleInputTable <- function(x, simplify = TRUE) {
  if (inherits(x, "EnsembleInputTable")) {
    return(x)
  }
  if (inherits(x, "EnsembleInput")) {
    df <- coerce_to_table(x, simplify = simplify)
    return(EnsembleInputTable(df))
  }
  stop("Unsupported input type: must be an EnsembleInput.")
}




