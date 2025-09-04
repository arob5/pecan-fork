# forward_model/ensemble_inputs.r


#' Create an \code{EnsembleInput} Object
#'
#' Constructs an \code{EnsembleInput} object, a tidy tibble-based S3 class 
#' representing a collection of ensemble model inputs. Each row corresponds to 
#' a single model run, with associated settings and (optionally) runtime 
#' inputs and metadata.
#' 
#' @details
#' At minimum, a run is defined by a unique combination of \code{run_id} and
#' \code{settings}. If only these columns are provided, this data structure 
#' can be thought of as an alternative representation of a 
#' \code{\link{MultiSettings}} object. \code{EnsembleInput} extends this
#' functionality with the addition of the \code{\link{runtime_input}} column, 
#' which provides the ability to overwrite default inputs (e.g., parameters,
#' initial conditions). Therefore, \code{runtime_input} takes precedence when
#' there are overlapping inputs specified in both \code{runtime_input} and
#' \code{settings}. Columns beyond these three are treated as metadata, and
#' do not undergo any special validation.
#' 
#' @param run_id Character vector of run IDs (length n).
#' @param settings List of \code{\link{Settings}} objects (length n).
#' @param runtime_input Optional list of \code{\link{RuntimeInput}} objects (length n).
#' @param metadata Optional named list of vectors or lists, each element of length n. 
#'  Metadata columns are added to the tibble.
#'
#' @return An object of class \code{EnsembleInput}, inheriting from \code{tbl_df}.
#' @seealso \code{\link{prep_model_ensemble_run}}, \code{\link{run_model_ensemble}},
#'          \code{\link{run_model_ensemble_and_read_output}}
#'
#' @author Andrew Roberts
#' @export
EnsembleInput <- function(run_id, settings, runtime_input=NULL, metadata=NULL) {
  x <- .new_ensemble_input(run_id, settings, runtime_input, metadata)
  validate_ensemble_input(x)
  
  return(x)
}


#' Validate an \code{EnsembleInput} Object
#'
#' Performs structural checks and type validation on an \code{EnsembleInput} object. 
#' Checks for expected columns, column types, and that all elements of the \code{settings}
#' and (optional) \code{runtime_input} columns are of the expected classes.
#'
#' @param x Object to validate (should inherit from \code{EnsembleInput}).
#'
#' @return Invisible input \code{x}, or throws an error if validation fails.
#' @seealso \code{\link{EnsembleInput}}, \code{\link{.new_ensemble_input}}
#' 
#' @author Andrew Roberts
#' @export
validate_ensemble_input <- function(x) {
  
  if(!inherits(x, "EnsembleInput")) {
    stop("`x` is not an object of class `EnsembleInput`.")
  }
  
  if(!inherits(x, "tbl_df")) {
    stop("Objects of class `EnsembleInput` must inherit from `tbl_df`.")
  }
  
  if(!("run_id" %in% names(x))) {
    stop("`EnsembleInput` missing `run_id` column.")
  }
  
  if(!("settings" %in% names(x))) {
    stop("`EnsembleInput` missing `settings` column.")
  }
  
  if(!is.character(x$run_id)) {
    stop("ensemble_input$run_id must be of class character.")
  }
  
  if(!is.list(x$settings)) {
    stop("ensemble_input$settings must be a list column of `Settings` objects.")
  }
  
  if(!all(vapply(x$settings, PEcAn.settings::is.Settings, logical(1)))) {
    stop("ensemble_input$settings must be a list column of `Settings` objects.")
  }
  
  if("runtime_input" %in% names(x)) {
    if(!is.list(x$runtime_input)) {
      stop("ensemble_input$runtime_input must be a list column of `RuntimeInput` objects.")
    }
    
    if(!all(vapply(x$runtime_input, is_runtime_input, logical(1)))) {
      stop("ensemble_input$runtime_input must be a list column of `RuntimeInput` objects.")
    }
  }
  
  if(any(duplicated(x$run_id))) {
    stop("`ensemble_input$run_id` cannot contain duplicates.")
  }

  invisible(x)
}


#' Create a New \code{EnsembleInput} S3 Object
#'
#' Internal constructor for the \code{EnsembleInput} S3 class. This function minimally validates 
#' inputs and assembles a tibble with columns for \code{run_id}, \code{settings}, 
#' (optionally) \code{runtime_input}, and any provided metadata.
#'
#' @param run_id Character vector of run IDs (length n)
#' @param settings List of \code{Settings} objects (length n).
#' @param runtime_input Optional list of \code{RuntimeInput} objects (length n).
#' @param metadata Optional named list. Each entry must be a vector or list of length n.
#'
#' @return A tibble of class \code{EnsembleInput} 
#'         (not fully validated—use with \code{validate_ensemble_input}).
#' @seealso \code{\link{EnsembleInput}}, \code{\link{validate_ensemble_input}}
#' 
#' @author Andrew Roberts
#' @keywords internal
.new_ensemble_input <- function(run_id, settings, runtime_input=NULL, metadata=NULL) {
  
  n <- length(run_id)
  if(length(settings) != n) {
    stop("Length of `settings` must match length of `run_id`.")
  }
  
  if(!is.null(runtime_input) && length(runtime_input) != n) {
    stop("Length of `runtime_input` must match length of `run_id`.")
  }
  
  tbl <- tibble::tibble(run_id = run_id,
                        settings = settings,
                        runtime_input = runtime_input)
  
  # If provided, metadata stored as additional optional columns.
  if (!is.null(metadata)) {
    if(!all(lengths(metadata) == nrow(tbl))) {
      stop("Elements of `metadata` must be lists of length equal to length of `settings`.")
    }
    
    tbl <- dplyr::bind_cols(tbl, tibble::as_tibble(metadata))
  }
  
  class(tbl) <- c("EnsembleInput", class(tbl))
  return(tbl)
}


#' Check if object inherits from \code{EnsembleInput}
#' 
#' @seealso \code{\link{EnsembleInput}}
#' @author Andrew Roberts
#' @export
is_ensemble_input <- function(x) {
  inherits(x, "EnsembleInput")
}


#' Print an \code{EnsembleInput} Object
#'
#' Custom print method for objects of class \code{EnsembleInput}. 
#' Displays a summary of the object, including the number of runs, the primary 
#' columns (\code{run_id}, \code{settings}, and optionally \code{runtime_input}), 
#' and any metadata columns.
#'
#' @param x An object of class \code{EnsembleInput}.
#' @param ... Additional arguments (currently ignored).
#' @return Invisibly returns \code{x}.
#' 
#' @author Andrew Roberts
#' @export
print.EnsembleInput <- function(x, ...) {
  cat("<EnsembleInput: tibble with", nrow(x), "runs>\n")
  
  # Primary columns.
  cols <- c("run_id", "settings")
  if("runtime_input" %in% names(x)) cols <- c(cols, "runtime_input")
  
  # Metadata columns.
  metadata_cols <- setdiff(names(x), cols)
  if(length(metadata_cols) > 0L) {
    cat("Metadata columns:\n")
    cat("  ", paste(metadata_cols, collapse=", "))
    cat("\n")
  }
  
  # Print first few rows with structure info
  print(as_tibble(x)[, cols], n=min(5, nrow(x)))

  invisible(x)
}


#' Subset an \code{EnsembleInput} Object
#'
#' Subset rows and/or columns of an \code{EnsembleInput} object, preserving the 
#' class if essential columns (\code{run_id}, \code{settings}) are retained.
#'
#' @param x An object of class \code{EnsembleInput}.
#' @param i Row indices to subset.
#' @param j Column indices to subset.
#' @param drop Logical; whether to drop dimensions. Default is \code{FALSE}.
#' @return The subsetted object, retaining class \code{EnsembleInput} 
#'  (and validated) if essential columns are present, otherwise defaults to a tibble/data.frame.
#'  
#' @author Andrew Roberts
#' @export
`[.EnsembleInput` <- function(x, i, j, drop=FALSE) {
  new <- NextMethod("[") # standard tibble/data.frame subsetting
  
  # Only keep ensemble_spec class if all required columns present
  if (all(c("run_id", "settings") %in% names(new))) {
    class(new) <- c("EnsembleInput", "tbl_df", "tbl", "data.frame")
    validate_ensemble_input(new)
  }
  
  return(new)
}


#' Coerce to \code{EnsembleInput}
#'
#' Converts a tibble or data frame to an \code{EnsembleInput} object after 
#' validating its structure.
#'
#' @param x A data.frame or tibble to coerce.
#' @return An object of class \code{EnsembleInput}, typically a tibble.
#'
#' @author Andrew Roberts
#' @export
as.EnsembleInput <- function(x) {
  if (!is.data.frame(x)) stop("`x` must be a data.frame or tibble")
  class(x) <- c("EnsembleInput", "tbl_df", "tbl", "data.frame")
  validate_ensemble_input(x)
}


#' Extract Ensemble Runs by Run ID
#'
#' Extracts the subset of an \code{EnsembleInput} object corresponding to a vector of run IDs.
#'
#' @param x An \code{EnsembleInput} object.
#' @param ids Character vector of run IDs to extract.
#' @return A subsetted \code{EnsembleInput} object containing only the specified run IDs.
#'
#' @author Andrew Roberts
#' @export
filter_run_id <- function(x, ids) {

  if(!is_ensemble_input(x)) {
    stop("`x` must be an `EnsembleInput` object.")
  }
  
  x[x$run_id %in% ids, ]
}


#' Return the output path associated with a particular run ID
#'
#' The output path is constructed from the selected \code{settings} object and 
#' @code{run_id} as @code{file.path(settings$modeloutdir,run_id)}.
#'
#' @param x An \code{EnsembleInput} object.
#' @param run_id character(1), the run ID to select.
#' @return character(1), the path to which model output is saved for the selected run.
#'
#' @author Andrew Roberts
#' @export
output_path <- function(x, run_id) {
  
  if(!is_scalar(run_id)) {
    stop("`output_path()` requires `run_id` to be a scalar.")
  }
  
  x <- filter_run_id(x, run_id)
  
  if(nrow(x) == 0L) {
    stop("`run_id` does not match run IDs in `EnsembleInput` object.")
  }

  file.path(x$settings[[1]]$modeloutdir, run_id)
}





