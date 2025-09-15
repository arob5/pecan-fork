# forward_model/model_input.r

InputSlot <- function(x) {
  structure(list(value=x), class="InputSlot")
}


MetadataSlot <- function(x) {
  structure(list(value=x), class="MetadataSlot")
}


#' Check if object inherits from \code{InputSlot}
#' 
#' @param x An object
#' @returns Logical, whether or not the object inherits from \code{InputSlot}.
#' 
#' @author Andrew Roberts
#' @export
is_input_slot <- function(x) {
  inherits(x, "InputSlot")
}


as_input_slot <- function(x) {
  if(is_input_slot(x)) x
  else InputSlot(x)
}


as_metadata_slot <- function(x) {
  if(is_metadata_slot(x)) x
  else MetadataSlot(x)
}


#' Check if object inherits from \code{MetadataSlot}
#' 
#' @param x An object
#' @returns Logical, whether or not the object inherits from \code{MetadataSlot}.
#' 
#' @author Andrew Roberts
#' @export
is_metadata_slot <- function(x) {
  inherits(x, "MetadataSlot")
}


print.InputSlot <- function(x, ...) {
  cat("InputSlot:\n")
  print(x$value, ...)
  invisible(x)
}

print.MetadataSlot <- function(x, ...) {
  cat("MetadataSlot:\n")
  print(x$value, ...)
  invisible(x)
}


is_model_input_leaf <- function(x) {
  is_input_slot(x) ||
    is_metadata_slot(x) ||
    !is.list(x)
}


# Empty list is not treated as a leaf by default.
ModelInput <- function(x, untagged_is_slot=TRUE) {
  assertthat::assert_that(is.list(x), msg="ModelInput constructor expects a list.")
  validated_x <- validate_and_wrap(x, path=character(), untagged_is_slot)
  
  structure(validated_x, class="ModelInput")
}


#' Check if object inherits from \code{ModelInput}
#' 
#' @param x An object
#' @returns Logical, whether or not the object inherits from \code{ModelInput}.
#' 
#' @author Andrew Roberts
#' @export
is_model_input <- function(x) {
  inherits(x, "ModelInput")
}


#' Throw error if object is not \code{ModelInput}
#' 
#' @param x An object
#' @returns Invisibly returns \code{TRUE} if \code{x} is an \code{ModelInput}.
#'  Otherwise throws an error.
#' 
#' @seealso \code{\link{ModelInput}}
#' @author Andrew Roberts
#' @export
check_model_input_type <- function(x) {
  if (!is_model_input(x)) stop("`x` is not an ModelInput object.")
  
  invisible(TRUE)
}


# recursive validator/wrapper
validate_and_wrap <- function(x, path, untagged_is_slot) {

  # A leaf 
  if(is_model_input_leaf(x)) {
    if(is_input_slot(x) || is_metadata_slot(x)) {
      return(x)
    } else if(!is.list(x)) {
      if(untagged_is_slot) return(InputSlot(x))
      else return(MetadataSlot(x))
    }
  }
  
  # Branch node (list)
  assertthat::assert_that(is_named_list(x, check_unique_names=TRUE),
                          msg = sprintf("All list elements must have unique names at level '%s'.",
                                       .node_path_to_key(path)))


  # Recurse on sub-tree
  out <- list()
  for(nm in names(x)) {
    out[[nm]] <- validate_and_wrap(x[[nm]], c(path, nm), untagged_is_slot)
  }
  
  return(out)
}


# Traverses left-to-right, depth first.
# f must have signature f(node, path, ...)
traverse_leaves <- function(x, f) {
  check_model_input_type(x)
  out <- list()
  
  recurse <- function(node, path=character()) {
    if(is_model_input_leaf(node)) {
      key <- .node_path_to_key(path)
      out[[key]] <<- f(node, path)
    } else if(is.list(node)) {
      for(nm in names(node)) {
        recurse(node[[nm]], c(path, nm))
      }
    } else {
      .raise_input_node_type_error(path=path)
    }
  }
  
  recurse(x, path=character())
  out
}


flatten_model_input <- function(x) {
  
  check_model_input_type(x)
  traverse_leaves(x, function(x, ...) x)
  
}


unflatten_model_input <- function(slots, metadata=NULL) {
  
  stopifnot(is_named_list(slots))
  stopifnot(is.null(metadata) || is_named_list(metadata))

  tree <- list()
  
  # Insert slots
  for(nm in names(slots)) {
    path <- .node_key_to_path(nm)
    val <- as_input_slot(slots[[nm]])
    tree <- .assign_path(tree, path, val)
  }
  
  # Insert metadata
  if(!is.null(metadata)) {
    for(nm in names(metadata)) {
      path <- .node_key_to_path(nm)
      val <- as_metadata_slot(metadata[[nm]])
      tree <- .assign_path(tree, path, val)
    }
  }
  
  # ensure deterministic ordering: reorder list according to slot order
  slot_order <- names(slots)
  meta_order <- if(!is.null(metadata)) names(metadata) else character()
  reorder_paths <- c(slot_order, meta_order)
  
  ModelInput(tree)
}


flatten_input_slots <- function(x, return_raw_values=TRUE) {
  
  check_model_input_type(x)
  
  return_input_slot <- function(x, ...) {
    if(is_input_slot(x)) {
       if(return_raw_values) x$value else x
    } else {
      NULL
    }
  }
  
  traverse_leaves(x, return_input_slot)
}


flatten_metadata_slots <- function(x, return_raw_values=TRUE) {
  
  check_model_input_type(x)
  
  return_metadata_slot <- function(x, ...) {
    if(is_metadata_slot(x)) {
      if(return_raw_values) x$value else x
    } else {
      NULL
    }
  }
  
  out <- traverse_leaves(x, return_metadata_slot)
  Filter(Negate(is.null), out)
}

.node_path_to_key <- function(node_path) {
  paste(node_path, collapse="/")
}

# key should not begin with "/"
.node_key_to_path <- function(node_key) {
  strsplit(node_key, split="/", fixed=TRUE)[[1]]
}

leaf_names <- function(x) {
  check_model_input_type(x)
  names(flatten_model_input(x))
}


slot_names <- function(x) {
  check_model_input_type(x)
  names(flatten_input_slots(x))
}


metadata_names <- function(x) {
  check_model_input_type(x)
  names(flatten_metadata_slots(x))
}

n_leaves <- function(x) {
  length(flatten_model_input(x))  
}


n_slots <- function(x) {
  length(flatten_input_slots(x))
}


n_metadata <- function(x) {
  length(flatten_metadata_slots(x))
}


tree_depth <- function(x) {
  check_model_input_type(x)
  
  recurse <- function(node, depth=1L) {
    if(is_model_input_leaf(node)) return(depth)
    if(length(node) == 0L) return(depth) # Empty list
    max(vapply(node, recurse, integer(1), depth=depth+1L))
  }
  
  recurse(x)
}


print.ModelInput <- function(x) {
  
  input_summary <- sprintf("ModelInput(n_slots = %s, n_metadata = %s, depth = %s)\n",
                           as.character(n_slots(x)), as.character(n_metadata(x)), 
                           as.character(tree_depth(x)))
  
  cat(input_summary)
}


summary.ModelInput <- function(x, ...) {
  
  check_model_input_type(x)

  slot_nm <- slot_names(x)
  meta_nm <- metadata_names(x)
  num_slots <- length(slot_nm)
  num_meta <- length(meta_nm)
  
  cat(sprintf("ModelInput with %d slots, %d metadata, depth %d\n",
              num_slots, num_meta, tree_depth(x)))
  
  if(num_slots > 0L) {
    cat("Slots:    ", paste(slot_nm, collapse = ", "), "\n")
  }
  
  if(num_meta > 0L) {
    cat("Metadata: ", paste(meta_nm, collapse = ", "), "\n")
  }
  
  invisible(x)
}


print_tree <- function(x, prefix="", include_leaf_class=TRUE) {
  check_model_input_type(x)

  recurse <- function(node, name=NULL, prefix="", is_last=TRUE) {
    connector <- if (is_last) "└── " else "├── "
    new_prefix <- if (is_last) paste0(prefix, "    ") else paste0(prefix, "│   ")
    
    # Determine label
    if(is_model_input_leaf(node)) {
      if(is_input_slot(node)) label <- paste0(name, " [InputSlot")
      else if(is_metadata_slot(node)) label <- paste0(name, " [MetadataSlot")
      
      if(include_leaf_class) label <- paste(label, class(node$value)[1], sep=", ")
      label <- paste0(label, "]")
    } else {
      label <- name
    }
    
    # Print this node (skip root if NULL name)
    if(!is.null(name)) {
      cat(prefix, connector, label, "\n", sep="")
    }
    
    # Recurse if list
    if(is.list(node) && !is_model_input_leaf(node)) {
      nms <- names(node)
      for(i in seq_along(nms)) {
        recurse(node[[i]], nms[i], new_prefix, i == length(nms))
      }
    }
  }
  
  recurse(x, name=NULL, prefix=prefix, is_last=TRUE)
  invisible(x)
}


# helper: assign value into a nested list by path
.assign_path <- function(tree, path, value) {
  
  if(length(path) == 1L) { # At terminal node in recursion
    if(!is.null(tree[[path]])) {
      stop(sprintf("Conflict: multiple values for key '%s'", 
                   .node_path_to_key(path)))
    }
    tree[[path]] <- value
  } else {
    nm <- path[1]
    rest_of_path <- path[-1]
    
    if(is.null(tree[[nm]])) tree[[nm]] <- list()
    if(!is.list(tree[[nm]])) { # Leaf already exists here
      stop(sprintf("Conflict: path '%s' tries to overwrite a non-list value",
                   .node_path_to_key(path)))
    }
    tree[[nm]] <- .assign_path(tree[[nm]], rest_of_path, value)
  }
  
  return(tree)
}


.raise_input_node_type_error <- function(path="", key=NULL) {
  if(is.null(key)) key <- .node_path_to_key(path)
  
  stop("Invalid node type at level: ", key,
       ". ModelInput nodes must be InputSlots, MetadataSlots, or lists.")
}








