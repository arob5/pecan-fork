# forward_model/forward_model_generators.r

# TODOs:
# - Force slots to align across runs.
# - Write ModelInput.flatten_to_numeric() and ModelInput.unflatten_from_numeric() methods
# - Write EnsembleInput.flatten_to_numeric() EnsembleInput.unflatten_from_numeric() methods
# - Figure out how to handle run IDs

# Forward model will accept matrix (one row per flattened input). The inputs
# will then be vectorized. e.g., if we have slots:
# l <- list(a=list(1,2,3), b=list(1:3, 4:6) and both slots are specified, then
# each individual input will have length 4 (e.g, input one here is c(1,1:3)).
# If a one row matrix is provided (one input) each individual model input will
# be updated using the passed value.
#
# What if more than one row is provided? We will interpret this as vectorizing
# over the inputs. i.e., not messing the original ens_input template - that is
# fixed. The template will be repeated once per row.
#
# The tricky part here: how do we specify how the passed input will be broadcast?
# Start simple: require the broadcast representation and require the entire
# slot to be selected as "free". e.g., for l <- list(a=list(1,2,3), b=list(1:3, 4:6),
# if `slot_names = "b"` then the forward model will expect users to pass values
# like list(1:3, 4:6) [though they will be passed in the form matrix(c(1:3, 4:6))].
# The passed value will be inserted into this slot, which can then be broadcast.
# It is up to the user to define slots such that each slot is either fixed
# or free. Focus on ensemble input now, but should have dispatch single 
# forward model if "default" is a ModelInput (this will be more straightforward).

# Eventually, "Slot" should be turned into a class, which could provide validation
# that all values of that slot are of the same type.

# verbose printing should:
# - Print fixed and free slots
# - Dimension by slot of each free slot
# - Print dimension of expected input
# - Number of total runs in ensemble
# - Forward model method that will be used (.function, .Settings, etc.)
# - Information on how the input will be broadcast

# Start simple by not vectorizing; just pass one value, which then gets 
# broadcast out according to the template.
gen_array_fwd_model <- function(obj, default, slot_names, verbose=TRUE) {

  # Freeze arguments.
  force(obj); force(default); force(slot_names)
  .validate_array_fwd_model(default, slot_names)

  # Flattened list containing all slots.
  free_slots <- do.call(c, default$slots[slot_names]) 
  free_slot_dims <- lapply(free_slots, get_array_like_dim)
  
  if(verbose) print_fwd_model_description(obj, default, slot_names)

  function(input_mat, ...) {
    ens_input <- update_ens_input_free_slots(default, input_mat, free_slot_dims,
                                             slot_name_map)
    run_model_ensemble(obj, ens_input, ...)
  }
}


update_ens_input_free_slots <- function(ens_input, input_mat, free_slot_names, 
                                        free_slot_dims) {
  
  # Flat list containing values from all slots.
  vals <- .flat_to_batched_array_list(input_mat, free_slot_dims)
  
  # Allocate values to each slot.
  idx_start <- 1L
  for(i in seq_along(free_slot_names)) {
    nm <- free_slot_names[[i]]
    idx_end <- idx_start + length(ens_input$slots[[nm]]) - 1L
    ens_input$slots[[nm]] <- vals[idx_start:idx_end]
    idx_start <- idx_end + 1L
  }
  
  return(ens_input)
}


.validate_array_fwd_model <- function(default, slot_names) {
  check_ensemble_input_broadcast_type(default)
  
  if(anyDuplicated(slot_names) > 0L) {
    stop("`slot_names` contains duplicates.")
  }
  
  invalid_slot_names <- setdiff(slot_names, slot_names(default))
  if(length(invalid_slot_names) > 0L) {
    stop("Invalid slot names: ", paste(invalid_slot_names, ", "))
  }
  
  free_slots <- default$slots[slot_names]
  for(l in free_slots) .check_input_is_array_list(l)

  invisible(TRUE)
}
















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











