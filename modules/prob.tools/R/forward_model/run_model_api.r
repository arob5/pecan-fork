# forward_model/run_model_api.r

# Implements standardized protocols for running PEcAn models, ensuring a 
# consistent interface for specifying outputs and returning outputs in a standard
# format. The lowest-level building block consists of an API for prepping a 
# model run (i.e., writing configs). Higher level protocols define interfaces
# for running models (prepping then running) and running models and reading 
# their outputs (prepping, running, and reading). APIs are provided for both
# single runs and ensemble model runs. Ensemble model runs are defined as 
# a set of model runs of a model, with each run using (potentially) different inputs.
#
# APIs for single model run:
#   prep_model_run(), run_model(), run_model_and_read_output()
# 
# APIs for ensemble model run:
#   prep_model_ensemble_run(), run_model_ensemble(), run_model_ensemble_and_read_output()


#' @title Prepare a model run (write config files)
#' 
#' @description
#' Writes config files to disk for a single model run. At minimum, a model 
#' run is defined by a \code{run_id} and \code{settings} object. Optionally,
#' \code{runtime_input}, a \code{\link{RuntimeInput}} object can be provided
#' to overwrite default settings (e.g., to manually pass in parameters and
#' initial conditions). Therefore, when \code{settings} and \code{runtime_input}
#' specify overlapping inputs, the \code{runtime_input} values
#' take precedence. 
#' 
#' @details
#' This interface is a light wrapper around the model-specific write config
#' function, which does the bulk of the work. The model is specified within
#' the \code{settings} object, which is used to load the model package and 
#' use the correct write config function. 
#' 
#' @param settings A PEcAn  settings list object.
#' @param runtime_input A \code{RuntimeInput} object containing parameters, 
#' initial conditions, etc. that will override any existing defaults. Or
#' \code{NULL} (default).
#' @param run_id Unique identifier for the model run. If \code{NULL} (default),
#'  one will be randomly generated.
#' @param constraint_vars Character vector of variables to extract from the model output.
#' @param append_run Logical; if \code{FALSE}, overwrites existing 
#'  \code{runs.txt} file (default). If `TRUE`, appends run ID to the existing file.
#'
#' @returns Invisibly returns the run ID.
#' @seealso \code{\link{make_runtime_input}}
#' 
#' @examples
#' \dontrun{
#'   runtime_input <- make_runtime_input(param = c(param1=0, param2=10))
#'   run_id <- prep_model_run(runtime_input, settings, run_id="test_run")
#' }
#' 
#' @author Andrew Roberts
#' @export
prep_model_run <- function(settings, runtime_input=NULL, run_id=NULL, append_run=FALSE, ...) {
  
  # Ensure runtime inputs are in proper format.
  if(!is.null(runtime_input)) validate_runtime_input(runtime_input)
  
  # If run ID is not provided, randomly generate one.
  if(is.null(run_id) || is.na(run_id)) run_id <- uuid::UUIDgenerate()
  
  # Load model package.
  PEcAn.utils::load.modelpkg(settings$model$type)
  
  # Write model config to file.
  model_write_config <- paste0("write.config.", settings$model$type)
  do.call(model_write_config, args=list(defaults=settings$pfts,
                                        trait.values=runtime_input$param,
                                        IC=runtime_input$ic,
                                        settings=settings,
                                        run.id=run_id))
  
  # Either append to or overwrite existing "runs.txt" file.
  cat(as.character(run_id),
      file=file.path(settings$rundir, "runs.txt"),
      sep="\n",
      append=append_run)
  
  return(invisible(run_id))
}


#' Prepare an Ensemble of Model Runs
#'
#' Calls \code{prep_model_run()} for each ensemble member in \code{ensemble_input}, 
#' which must be a tibble of run specifications. Each run corresponds to a row;
#' a config file will be written for each, and the run IDs will be appended 
#' in a single \code{runs.txt} file.
#'
#' @param ensemble_input An \code{EnsembleInput} object (tibble) with columns 
#'  \code{run_id}, \code{settings}, and optionally \code{runtime_input}.
#' @param ... Additional arguments passed to each \code{prep_model_run()}.
#'
#' @return Invisibly returns a list of run IDs for the ensemble.
#' @seealso \code{\link{prep_model_run}}, \code{\link{run_model_ensemble}}
#' @author Andrew Roberts
#' @export
prep_model_ensemble_run <- function(ensemble_input, ...) {
  
  validate_ensemble_input(ensemble_input)
  
  # Determine whether runtime_input will be used.
  if ("runtime_input" %in% names(ensemble_input)) {
    arg_list <- ensemble_input[c("run_id", "settings", "runtime_input")]
  } else {
    arg_list <- tibble::tibble(
      run_id = ensemble_input$run_id,
      settings = ensemble_input$settings,
      runtime_input = rep(list(NULL), nrow(ensemble_input))
    )
  }
  
  # Loop over rows of `ensemble_input`, calling `prep_model_run()` for each row.
  run_ids <- purrr::pmap(
    ensemble_input[c("run_id", "settings", "runtime_input")],
    function(run_id, settings, runtime_input, ...) {
      prep_model_run(run_id=run_id, settings=settings, 
                     runtime_input=runtime_input, append_run=TRUE, ...)
    }
  )
  
  return(invisible(run_ids))
}


#' Execute a single model run
#'
#' Prepares configuration and executes a single model run using PEcAn's 
#' standardized protocol.
#'
#' @param settings A PEcAn \code{settings} object.
#' @param runtime_input A \code{RuntimeInput} object, or \code{NULL} (default).
#' @param run_id Character: Run ID (optional).
#' @param append_run Logical: Append run ID to \code{runs.txt} file (otherwise overwrites file).
#' @param stop_on_error Logical: Stop if model runs encounter an error (default: \code{TRUE}).
#' @param ... Additional arguments passed to \code{prep_model_run()}.
#'
#' @return Invisibly returns the run ID.
#' @seealso \code{\link{run_model_ensemble}}, \code{\link{prep_model_run}}
#' @author Andrew Roberts
#' @export
run_model <- function(settings, runtime_input=NULL, run_id=NULL, append_run=FALSE, 
                      stop_on_error=TRUE, ...) {

  run_id <- prep_model_run(runtime_input=runtime_input, 
                           settings=settings, 
                           run_id=run_id, 
                           append_run=append_run, ...)
  
  PEcAn.workflow::start_model_runs(settings, stop_on_error=stop_on_error, write=FALSE)
  
  return(invisible(run_id))
}


#' Execute an ensemble model run
#'
#' Runs an ensemble of models, preparing and starting model runs for each member 
#' of the \code{ensemble_input}.
#'
#' @param ensemble_input An \code{EnsembleInput} object (see \code{prep_model_ensemble_run()}).
#' @param stop_on_error Logical: Stop if any model run encounters an error (default: \code{TRUE}).
#' @param ... Additional arguments passed to \code{prep_model_ensemble_run()}.
#'
#' @return Invisibly returns a list of run IDs.
#' @seealso \code{\link{run_model}}, \code{\link{prep_model_ensemble_run}}
#' @author Andrew Roberts
#' @export
run_model_ensemble <- function(ensemble_input, stop_on_error=TRUE, ...) {
  
  run_ids <- prep_model_ensemble_run(ensemble_input, ...)
  PEcAn.workflow::start_model_runs(ensemble_input$settings[[1]], 
                                   stop_on_error=stop_on_error, write=FALSE)

  return(invisible(run_ids))
}


#' Run and Read Output from a Model
#'
#' Prepares, runs, and loads the output from a single model run.
#'
#' @param settings A PEcAn \code{settings} object.
#' @param runtime_input A \code{RuntimeInput} object, or \code{NULL} (default).
#' @param run_id Character: Run ID (optional).
#' @param append_run Logical: Append run ID to \code{runs.txt}.
#' @param stop_on_error Logical: Stop if model runs encounter an error.
#' @param output_vars Character vector of output variable names to read.
#' @param ... Additional arguments to \code{PEcAn.utils::read.output()}.
#'
#' @return Returns model output as read by \code{PEcAn.utils::read.output}.
#' @seealso \code{\link{run_model}}, \code{\link{run_model_ensemble_and_read_output}}
#' @author Andrew Roberts
#' @export
run_model_and_read_output <- function(settings, runtime_input=NULL, run_id=NULL, 
                                      append_run=FALSE, stop_on_error=TRUE, 
                                      output_vars=NULL, ...) {
  
  run_id <- run_model(settings=settings,
                      runtime_input=runtime_input,
                      run_id=run_id,
                      append_run=append_run,
                      stop_on_error=stop_on_error)
  
  run_output_path <- file.path(settings$modeloutdir, run_id)
  model_output <- PEcAn.utils::read.output(run_id, outdir=run_output_path, 
                                           variables=output_vars, ...)
  return(model_output)
}


#' Run Ensemble Model and Read Output
#'
#' Prepares an ensemble model run, executes the runs, and reads output for each run.
#'
#' @param ensemble_input An \code{EnsembleInput} object as in \code{run_model_ensemble()}.
#' @param stop_on_error Logical: Throws error if any runs fail (default: \code{TRUE}).
#' @param variables Character vector of output variable names to read. Passed to \code{PEcAn.utils::read.output()}.
#' @param ... Additional arguments passed to \code{PEcAn.utils::read.output()}.
#'
#' @return Returns a named list of model outputs (one per run ID). The list names
#'  are set to the run IDs, and the elements are of the form returned by
#'  \code{PEcAn.utils::read.output()}.
#' @seealso \code{\link{run_model_ensemble}}, \code{\link{run_model_and_read_output}}
#' @author Andrew Roberts
#' @export
run_model_ensemble_and_read_output <- function(ensemble_input, stop_on_error=TRUE,
                                               variables=NULL, ...) {

  # Execute ensemble runs.
  run_ids <- run_model_ensemble(ensemble_input, stop_on_error=stop_on_error)
  
  # Read output into list.
  run_output_path <- file.path(settings$modeloutdir, run_id)
  model_ensemble_output <- lapply(run_ids, function(run_id) PEcAn.utils::read.output(run_id, 
                                                                                     outdir=run_output_path, 
                                                                                     variables=variables, ...))
  names(model_ensemble_output) <- run_ids
  
  return(model_ensemble_output)
}
