# forward_model/forward_model_generators.r


#' @export
is_forward_model_run <- function(x) {
  inherits(x, "forward_model_run")
}


#' Forward model factory with multiple vectorized free slots
#'
#' This factory generates a closure that accepts multiple free slots as lists of values.
#' Each combination of free slot values is automatically broadcast over fixed slots
#' using the provided broadcasting rule.
#'
#' @param slots Named list of slot value sets. Use `NULL` for free slots.
#' @param rule Broadcasting rule function (produces index matrix).
#' @param model_fn Function to run a single model input.
#' @param ensemble_runner Function that executes the ensemble.
#' @param return_ensemble Logical; if TRUE, return a `forward_model_run` object.
#'
#' @return A closure whose arguments are the free slots. Returns either the raw results
#'   or a `forward_model_run` object containing the ensemble and results.
#' @export
forward_model_factory <- function(slots, rule, model_fn, ensemble_runner, return_ensemble = FALSE) {
  free_slots  <- names(slots)[vapply(slots, is.null, logical(1))]
  fixed_slots <- slots[!vapply(slots, is.null, logical(1))]
  
  force(slots); force(rule); force(model_fn); force(ensemble_runner); force(return_ensemble)
  
  function(...) {
    args <- list(...)
    
    # Check that all free slots provided
    if (!setequal(names(args), free_slots)) {
      stop("Must provide exactly the free slots: ", paste(free_slots, collapse = ", "))
    }
    
    # Convert all values to lists if they are not already
    for (nm in free_slots) {
      val <- args[[nm]]
      if (!is.list(val)) args[[nm]] <- as.list(val)
    }
    
    # Merge fixed slots with user-provided free slots
    full_slots <- c(fixed_slots, args)
    
    # Construct broadcasted ensemble using the provided rule
    emb <- ensemble_model_input_broadcast(full_slots, rule)
    
    # Run ensemble
    results <- ensemble_runner(emb, model_fn)
    
    if (return_ensemble) {
      structure(
        list(ensemble = emb, results = results),
        class = c("forward_model_run", "ensemble_model_input_broadcast", "ensemble_model_input")
      )
    } else {
      results
    }
  }
}


# ============================================================
# Partial evaluation for forward models
# ============================================================

#' Partially fix slots in a forward model
#'
#' Returns a new forward model closure where some slots are fixed.
#' Remaining slots become the new free slots of the returned function.
#'
#' @param fwd A forward model closure created by `forward_model_factory`.
#' @param fixed_slots Named list of slots to fix. Must be subset of original free slots.
#'
#' @return A new forward model closure with fewer free slots.
#' @export
partial_forward_model <- function(fwd, fixed_slots) {
  stopifnot(is.function(fwd), is.list(fixed_slots))
  
  function(...) {
    args <- list(...)
    # Merge fixed_slots into current args
    all_args <- c(fixed_slots, args)
    
    # Call the original forward model
    do.call(fwd, all_args)
  }
}


# ============================================================
# Peek a slice of an ensemble
# ============================================================

#' Peek a slice of an ensemble model input
#'
#' Returns the slot values for specific runs or conditions without running the model.
#'
#' @param emb An ensemble_model_input object (list, table, or broadcast).
#' @param run_indices Optional integer vector of run indices to peek.
#' @param slot_filters Optional named list of slot values to filter by.
#' @param as_tibble Logical; if TRUE and tibble is available, return a tibble.
#'
#' @return A data.frame or tibble with one row per matching run and one column per slot.
#' @export
peek_ensemble_slice <- function(emb, run_indices = NULL, slot_filters = NULL, as_tibble = TRUE) {
  tbl <- as_table(emb)
  idx <- tbl$index
  
  # Filter by run_indices if provided
  if (!is.null(run_indices)) {
    idx <- idx[run_indices, , drop = FALSE]
  }
  
  # Filter by slot_filters if provided
  if (!is.null(slot_filters)) {
    for (nm in names(slot_filters)) {
      if (!nm %in% names(tbl$slots)) stop("Unknown slot: ", nm)
      values <- slot_filters[[nm]]
      mask <- idx[[nm]] %in% match(values, tbl$slots[[nm]])
      idx <- idx[mask, , drop = FALSE]
    }
  }
  
  # Materialize the slot values for the selected runs
  df <- as.data.frame(
    lapply(names(tbl$slots), function(nm) tbl$slots[[nm]][idx[[nm]]]),
    stringsAsFactors = FALSE
  )
  names(df) <- names(tbl$slots)
  
  if (as_tibble && requireNamespace("tibble", quietly = TRUE)) {
    df <- tibble::as_tibble(df)
  }
  
  df
}


# ============================================================
# Peek a slice of an ensemble
# ============================================================

#' Peek a slice of an ensemble model input
#'
#' Returns the slot values for specific runs or conditions without running the model.
#'
#' @param emb An ensemble_model_input object (list, table, or broadcast).
#' @param run_indices Optional integer vector of run indices to peek.
#' @param slot_filters Optional named list of slot values to filter by.
#' @param as_tibble Logical; if TRUE and tibble is available, return a tibble.
#'
#' @return A data.frame or tibble with one row per matching run and one column per slot.
#' @export
peek_ensemble_slice <- function(emb, run_indices = NULL, slot_filters = NULL, as_tibble = TRUE) {
  tbl <- as_table(emb)
  idx <- tbl$index
  
  # Filter by run_indices if provided
  if (!is.null(run_indices)) {
    idx <- idx[run_indices, , drop = FALSE]
  }
  
  # Filter by slot_filters if provided
  if (!is.null(slot_filters)) {
    for (nm in names(slot_filters)) {
      if (!nm %in% names(tbl$slots)) stop("Unknown slot: ", nm)
      values <- slot_filters[[nm]]
      mask <- idx[[nm]] %in% match(values, tbl$slots[[nm]])
      idx <- idx[mask, , drop = FALSE]
    }
  }
  
  # Materialize the slot values for the selected runs
  df <- as.data.frame(
    lapply(names(tbl$slots), function(nm) tbl$slots[[nm]][idx[[nm]]]),
    stringsAsFactors = FALSE
  )
  names(df) <- names(tbl$slots)
  
  if (as_tibble && requireNamespace("tibble", quietly = TRUE)) {
    df <- tibble::as_tibble(df)
  }
  
  df
}


#' Slice a forward model to selected runs
#'
#' Returns a new forward model closure that only runs a subset of the original ensemble.
#'
#' @param fwd A forward model closure created by `forward_model_factory`.
#' @param run_indices Optional vector of run indices to keep.
#' @param slot_filters Optional named list of slot values to filter runs.
#'
#' @return A new forward model closure with the same interface as the original, but only executing the selected runs.
#' @export
slice_forward_model <- function(fwd, run_indices = NULL, slot_filters = NULL) {
  stopifnot(is.function(fwd))
  
  function(...) {
    # Generate full ensemble using the original closure with the provided args
    full <- fwd(...)
    
    # Determine which runs to keep
    if (!is_forward_model_run(full)) {
      stop("The forward model must be created with return_ensemble = TRUE to slice.")
    }
    
    emb <- full$ensemble
    
    # Use peek_ensemble_slice to get indices matching the filters
    slice_df <- peek_ensemble_slice(emb, run_indices = run_indices, slot_filters = slot_filters)
    
    # Map filtered rows back to indices in the original broadcast
    idx_to_keep <- sapply(seq_len(nrow(slice_df)), function(i) {
      match(
        paste0(as.list(slice_df[i, ]), collapse = "_"),
        paste0(lapply(get_broadcast_run(emb, seq_len(nrow(emb$index))), function(mi) {
          paste0(unlist(mi$slots), collapse = "_")
        }), collapse = "_")
      )
    })
    
    # Run only the selected indices
    results <- full$results[idx_to_keep]
    
    # Return sliced forward_model_run object
    structure(
      list(ensemble = peek_ensemble_slice(emb, run_indices = idx_to_keep, as_tibble = FALSE),
           results = results),
      class = c("forward_model_run_slice", "forward_model_run", "ensemble_model_input_broadcast", "ensemble_model_input")
    )
  }
}







#' @export
print.forward_model_run <- function(x, ...) {
  cat("<forward_model_run>\n")
  cat("Ensemble:\n")
  NextMethod()  # uses the ensemble_model_input print
  cat("Number of results:", length(x$results), "\n")
  invisible(x)
}

#' @export
summary.forward_model_run <- function(object, ...) {
  cat("<forward_model_run summary>\n")
  cat("Ensemble:\n")
  NextMethod()  # ensemble_model_input summary
  cat("Number of results:", length(object$results), "\n")
  invisible(object)
}































.make_fwd_model <- function(settings, input_template, obs_op=NULL, 
                            run_id_prefix="run", ensemble_prefix="ens",
                            include_uuid=TRUE, ...) {
  
}


make_fwd_model <- function(settings, output_vars=NULL, obs_op=NULL,
                           run_id_prefix="run", include_uuid=TRUE, ...) {
  
  run_counter <- 1L
  
  # Default to identity
  if(is.null(obs_op)) obs_op <- function(x) x
  
  function(input) {
    run_id <- paste0(run_id_prefix, run_counter)
    id_counter <<- id_counter + 1L
    if(include_uuid) run_id <- paste(run_id, uuid::UUIDgenerate(), sep="_")
      
    out <- run_model_and_read_output(settings, runtime_input=input, run_id=run_id, 
                                     append_run=FALSE, stop_on_error=TRUE, 
                                     output_vars=output_vars, ...)
    obs_op(out)
  }
  
}


make_fwd_ensemble_model <- function(settings, output_vars = NULL, obs_op = NULL, append_run = TRUE, ...) {
  id_counter <- 1L
  
  if (is.null(obs_op)) obs_op <- function(x) x  # identity
  
  function(input_matrix) {
    # input_matrix: rows are individual inputs (matrix or tibble)
    n <- if (is.matrix(input_matrix)) nrow(input_matrix) else length(input_matrix)
    
    # Generate unique run IDs for ensemble members
    run_ids <- paste0("run", seq(id_counter, length.out = n))
    id_counter <<- id_counter + n
    
    # Construct runtime_input list
    # If using matrix: each row must be converted to a RuntimeInput; customize this per your protocol
    runtime_inputs <- lapply(seq_len(n), function(i) {
      # Assumes you have a function `make_runtime_input`
      make_runtime_input(param = as.numeric(input_matrix[i, ]))
      # Optionally, deal with IC etc. if needed
    })
    
    # EnsembleInput tibble
    ensemble_input <- tibble::tibble(
      run_id = run_ids,
      settings = rep(list(settings), n),
      runtime_input = runtime_inputs
    )
    
    out <- run_model_ensemble_and_read_output(
      ensemble_input, variables = output_vars, append_run = append_run, ...
    )
    
    obs_op(out)
  }
}


make_fwd_ensemble_model_matrix <- function(settings, output_vars = NULL, obs_op = NULL, append_run = TRUE, ...) {
  id_counter <- 1L
  if (is.null(obs_op)) obs_op <- function(x) x
  
  function(input_matrix) {
    n <- if (is.matrix(input_matrix)) nrow(input_matrix) else length(input_matrix)
    run_ids <- paste0("run", seq(id_counter, length.out = n))
    id_counter <<- id_counter + n
    
    runtime_inputs <- lapply(seq_len(n), function(i) {
      param_vec <- if (is.matrix(input_matrix)) input_matrix[i, ] else input_matrix[[i]]
      make_runtime_input(param = param_vec)
    })
    
    ensemble_input <- tibble::tibble(
      run_id = run_ids,
      settings = rep(list(settings), n),
      runtime_input = runtime_inputs
    )
    
    out <- run_model_ensemble_and_read_output(
      ensemble_input, variables = output_vars, append_run = append_run, ...
    )
    
    obs_op(out)  # often you want to extract just what you need (e.g. a filtered set); customize here
  }
}











