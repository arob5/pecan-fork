# forward_model/run_model_api.r

run_model <- function(obj, model_input, ...) {
  UseMethod("run_model")
}


run_model_ensemble <- function(obj, ens_input, ...) {
  UseMethod("run_model_ensemble")
}


run_model.function <- function(model_func, model_input, ...) {
  model_func(model_input)
}


run_model_ensemble.function <- function(model_func, ens_input, ...) {
  
  apply_over_ensemble(ens_input, ens_input)
  
}


