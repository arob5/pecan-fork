# forward_model/run_model_api.r
#
# Depends: PEcAn.settings, PEcAn.utils

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
#   run_model(), prep_pecan_model_run(), start_pecan_model_run()    
# 
# APIs for ensemble model run:
#   run_model_ensemble(), prep_pecan_ensemble_run(), start_pecan_ensemble_run()
#
# `run_model.Settings()` and `run_model_ensemble.Settings()` are the PEcAn methods
# for the run model and run model ensemble generics.


#' Run a PEcAn model and return outputs
#'
#' A standardized API for a single PEcAn model run, which entails writing a
#' model config, starting the model run, and reading outputs from file.
#' 
#' @details
#' At present, this API is defined so that PEcAn's standard model run workflow
#' can be executed without modification. A PEcAn model execution requires 
#' the inputs \code{trait.values}, \code{run.id}, and \code{settings}. 
#' PEcAn \code{Settings} objects contain settings beyond those necessary to run the
#' model. Thus, to be more specific, the \code{settings} input is typically used
#' to specify required driver (meteorology) and initial condition (IC) inputs
#' via the fields \code{settings$run$inputs$met, settings$run$inputs$poolinitcond}.
#' Beyond model inputs, the settings object defines the model to be run via
#' \code{settings$model}, as well as the filepaths:
#' \itemize{
#'  \item \code{settings$outdir}: the highest level output directory for the run.
#'  \item \code{settings$modeloutdir}: directory to which model outputs will be written.
#'  \item \code{settings$rundir}: directory to which config files will be saved.
#' }
#'     
#' The PEcAn workflow allows users to manually pass in model parameters via
#' the \code{trait.values} argument. Some models allow runtime specification
#' of other model inputs (e.g., initial conditions).
#' 
#' To respect the PEcAn workflow structure, the run model API is structured as follows:  
#' \itemize{
#'  \item \code{settings$outdir}: \code{settings}
#'  \item \code{settings$modeloutdir}: directory to which model outputs will be written.
#'  \item \code{settings$rundir}: directory to which config files will be saved.
#' }
#'
#' @param settings A PEcAn \code{Settings} object.
#' @param model_input A \code{ModelInput} object.
#' @param run_id character(1), a unique string identifier for the run. Will be 
#'  auto-generated if not provided.
#' @param overwrite_runs_file logical(1), if \code{TRUE}, overwrites any existing
#'  \code{runs.txt} file in the run directory. Otherwise, appends to this file.
#'  If appending, note that any runs executed from this directory using
#'  \code{PEcAn.workflow::start_model_runs()} will run both the newly appended
#'  and old runs. Even in this case, this method will still only return the 
#'  results associated with \code{run_id}, the current run. 
#' @param ... Additional arguments passed to \code{PEcAn.utils::read.output()}.
#'
#' @returns The model outputs corresponding to the model run with ID \code{run_id}.
#'  The outputs are formatted as returned by \code{PEcAn.utils::read.output()}.
#'  An attribute \code{run_id} is attached to the output object.
#'  
#' @author Andrew Roberts
#' @export
run_model.Settings <- function(settings, model_input, run_id=NULL, 
                               overwrite_runs_file=FALSE, ...) {
  
  # Write configs, start model run, write outputs to file.
  run_id <- start_pecan_model_run(settings=settings,
                                  model_input=model_input,
                                  run_id=run_id,
                                  overwrite_runs_file=overwrite_runs_file)
  
  # Read outputs from file.
  run_output_path <- file.path(settings$modeloutdir, run_id)
  model_output <- PEcAn.utils::read.output(run_id, outdir=run_output_path, 
                                           variables=output_vars, ...)
  attr(model_output, "run_id") <- run_id
  
  return(model_output)
}


run_model_ensemble.Settings <- function(settings, model_input, run_id=NULL, 
                                        overwrite_runs_file=FALSE, ...) {
  
  # Read output into list.
  model_ens_output <- lapply(run_ids, 
                                  function(run_id) PEcAn.utils::read.output(run_id, 
                                                                            outdir=output_path(ensemble_input, run_id), 
                                                                            variables=variables, ...))
  names(model_ens_output) <- run_ids
  
  return(model_ens_output)
}


#' Write Configuration Files to Disk
#' 
#' Writes inputs for a PEcAn model to file, and writes the \code{run_id} to the
#' \code{runs.txt} file. Does not actually run the model - prepares the model
#' for execution using \code{PEcAn.workflow::start_model_runs()}.
#'
#'  
#' @returns Invisibly returns the \code{run_id}. Loads the PEcAn model package
#'  and uses the write config function from this package to write configuration
#'  files to disk.
prep_pecan_model_run <- function(settings, model_input, run_id=NULL, 
                                 overwrite_runs_file=FALSE, ...) {

  # If run ID is not provided, randomly generate one.
  if(is.null(run_id) || is.na(run_id)) run_id <- uuid::UUIDgenerate()
  
  # Overwrite defaults in `settings` with values specified in `model_input`.
  settings <- overwrite_default_settings(settings, model_input)
  
  # Load model package.
  PEcAn.utils::load.modelpkg(settings$model$type)
  
  # Create run and output directories.
  dir.create(file.path(settings$rundir, run_id), recursive=TRUE)
  dir.create(file.path(settings$modeloutdir, run_id), recursive=TRUE)
  
  # Write model config to file.
  model_write_config <- paste0("write.config.", settings$model$type)
  
  write_config_args <- list(settings=settings, defaults=settings$pfts, run.id=run_id)
  write_config_args <- c(write_config_args, pecan_runtime_slots(model_input))
  do.call(model_write_config, args=write_config_args)
  
  # Either append to or overwrite existing "runs.txt" file.
  cat(as.character(run_id),
      file=file.path(settings$rundir, "runs.txt"),
      sep="\n",
      append=!overwrite_runs_file)
  
  return(invisible(run_id))
}










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
#' use the correct write config function. At present, the convention for all
#' of the model API function is to pass \code{...} arguments only to the 
#' underlying core PEcAn functions (e.g., \code{PEcAn.utils::read.output}),
#' when relevant. All arguments for the API functions are stated explicitly by name.
#' 
#' @param settings A PEcAn  settings list object.
#' @param runtime_input A \code{RuntimeInput} object containing parameters, 
#' initial conditions, etc. that will override any existing defaults. Or
#' \code{NULL} (default).
#' @param run_id Unique identifier for the model run. If \code{NULL} (default),
#'  one will be randomly generated.
#' @param append_run Logical; if \code{FALSE}, overwrites existing 
#'  \code{runs.txt} file (default). If `TRUE`, appends run ID to the existing file.
#' @param ... Additonal arguments currently not used.
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
prep_model_run <- function(settings, runtime_input=NULL, run_id=NULL, append_run=TRUE, ...) {
  
  # Ensure runtime inputs are in proper format.
  if(!is.null(runtime_input)) validate_runtime_input(runtime_input)
  
  # If run ID is not provided, randomly generate one.
  if(is.null(run_id) || is.na(run_id)) run_id <- uuid::UUIDgenerate()
  
  # Load model package.
  PEcAn.utils::load.modelpkg(settings$model$type)
  
  # Create run and output directories.
  dir.create(file.path(settings$rundir, run_id), recursive=TRUE)
  dir.create(file.path(settings$modeloutdir, run_id), recursive=TRUE)
  
  # Write model config to file.
  model_write_config <- paste0("write.config.", settings$model$type)
  do.call(model_write_config, args=list(defaults=settings$pfts,
                                        trait.values=list(runtime_input$param), # TODO: temporary: SIPNET requires this to be a list
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
#' @param append_run logical, whether to append to or overwrite an existing 
#'  \code{runs.txt} file. The runs within the current ensemble batch will always
#'  be appended; this argument only controls whether existing runs before this
#'  batch will be overwritten. Default is to append.
#' @param ... Additional arguments currently not used.
#'
#' @return Invisibly returns a list of run IDs for the ensemble.
#' @seealso \code{\link{prep_model_run}}, \code{\link{run_model_ensemble}}
#' @author Andrew Roberts
#' @export
prep_model_ensemble_run <- function(ensemble_input, append_run=TRUE, ...) {
  
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
  
  # Append to or replace existing "runs.txt" file. Runs within the current
  # ensemble batch are always appended.
  if(append_run) {
    arg_list <- arg_list %>% mutate(append_run=TRUE)
  } else {
    arg_list <- arg_list %>% mutate(append_run = row_number() != 1L)
  }
  
  # Loop over rows of `ensemble_input`, calling `prep_model_run()` for each row.
  run_ids <- purrr::pmap(
    arg_list[c("run_id", "settings", "runtime_input", "append_run")],
    function(run_id, settings, runtime_input, append_run, ...) {
      prep_model_run(run_id=run_id, settings=settings, 
                     runtime_input=runtime_input, append_run=append_run)
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
#' @param ... Additional arguments currently not used.
#'
#' @return Invisibly returns the run ID.
#' @seealso \code{\link{run_model_ensemble}}, \code{\link{prep_model_run}}
#' @author Andrew Roberts
#' @export
run_model <- function(settings, runtime_input=NULL, run_id=NULL, append_run=TRUE, 
                      stop_on_error=TRUE, ...) {

  run_id <- prep_model_run(runtime_input=runtime_input, 
                           settings=settings, 
                           run_id=run_id, 
                           append_run=append_run)
  
  PEcAn.workflow::start_model_runs(settings, stop.on.error=stop_on_error, 
                                   write=FALSE)
  
  return(invisible(run_id))
}


#' Execute an ensemble model run
#'
#' Runs an ensemble of models, preparing and starting model runs for each member 
#' of the \code{ensemble_input}.
#'
#' @param ensemble_input An \code{EnsembleInput} object (see \code{prep_model_ensemble_run()}).
#' @param stop_on_error Logical: Stop if any model run encounters an error (default: \code{TRUE}).
#' @param ... Additional arguments currently not used.
#'
#' @return Invisibly returns a list of run IDs.
#' @seealso \code{\link{run_model}}, \code{\link{prep_model_ensemble_run}}
#' @author Andrew Roberts
#' @export
run_model_ensemble <- function(ensemble_input, append_run=TRUE, stop_on_error=TRUE, ...) {
  
  run_ids <- prep_model_ensemble_run(ensemble_input, append_run=append_run)
  PEcAn.workflow::start_model_runs(ensemble_input$settings[[1]], 
                                   stop.on.error=stop_on_error, write=FALSE)

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
#'         An attribute named \code{run_id} is set to the character run ID
#'         identifying the model run.
#' @seealso \code{\link{run_model}}, \code{\link{run_model_ensemble_and_read_output}}
#' @author Andrew Roberts
#' @export
run_model_and_read_output <- function(settings, runtime_input=NULL, run_id=NULL, 
                                      append_run=TRUE, stop_on_error=TRUE, 
                                      output_vars=NULL, ...) {
  
  run_id <- run_model(settings=settings,
                      runtime_input=runtime_input,
                      run_id=run_id,
                      append_run=append_run,
                      stop_on_error=stop_on_error)
  
  run_output_path <- file.path(settings$modeloutdir, run_id)
  model_output <- PEcAn.utils::read.output(run_id, outdir=run_output_path, 
                                           variables=output_vars, ...)
  attr(model_output, "run_id") <- run_id
  
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
                                               variables=NULL, append_run=TRUE, ...) {

  # Execute ensemble runs.
  run_ids <- run_model_ensemble(ensemble_input, append_run=append_run, 
                                stop_on_error=stop_on_error)
  
  # Read output into list.
  model_ensemble_output <- lapply(run_ids, 
                                  function(run_id) PEcAn.utils::read.output(run_id, 
                                                                            outdir=output_path(ensemble_input, run_id), 
                                                                            variables=variables, ...))
  names(model_ensemble_output) <- run_ids
  
  return(model_ensemble_output)
}
