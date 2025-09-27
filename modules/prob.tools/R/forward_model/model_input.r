# forward_model/model_input.r

#' Tag for an input slot in ModelInput
#'
#' A light wrapper around an object that identifies that the object should
#' be treated as a leaf in the \code{ModelInput} tree, and designates the leaf 
#' as a component of the model input.
#' 
#' @author Andrew Roberts
#' @export
InputSlot <- function(x) {
  structure(list(value=x), class="InputSlot")
}


#' Tag for a metadata slot in ModelInput
#'
#' A light wrapper around an object that identifies that the object should
#' be treated as a leaf in the \code{ModelInput} tree, and designates the leaf 
#' as metadata.
#' 
#' @author Andrew Roberts
#' @export
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


#' Convert object to an \code{InputSlot}
#' @export
as_input_slot <- function(x) {
  if(is_input_slot(x)) x
  else InputSlot(x)
}


#' Convert object to a \code{MetadataSlot}
#' @export
as_metadata_slot <- function(x) {
  if(is_metadata_slot(x)) x
  else MetadataSlot(x)
}


#' Print method for \code{InputSlot}
#' @export
print.InputSlot <- function(x, ...) {
  cat("InputSlot:\n")
  print(x$value, ...)
  invisible(x)
}


#' Print method for \code{MedadataSlot}
#' @export
print.MetadataSlot <- function(x, ...) {
  cat("MetadataSlot:\n")
  print(x$value, ...)
  invisible(x)
}


#' Check if object has class identical to "list"
#'
#' This returns true only for "pure" lists. It returns false for objects
#' like data.frames that inherit from list.
#'
#' @author Andrew Roberts
#' @export
is_pure_list <- function(x) {
  is.list(x) && identical(class(x), "list")
}


#' Defines a leaf in a \code{ModelInput} object
#' 
#' A leaf is any object that is either:
#' 1. Not a pure list
#' 2. Is directly tagged as an \code{InputSlot} or \code{MetadataSlot}.
#' 
#' @details
#' A \code{ModelInput} object itself is not considered a leaf. By excluding
#' only pure lists, this means that objects like data.frames are treated
#' as leaves. Objects of any class can be treated as a leaf by wrapping them
#' as an \code{InputSlot} or \code{MetadataSlot}.
#' 
#' @author Andrew Roberts 
#' @export
is_model_input_leaf <- function(x) {
  
  # Ensure the whole tree is not treated as leaf.
  if(is_model_input(x)) return(FALSE)
  
  !is_pure_list(x) ||
  is_input_slot(x) ||
  is_metadata_slot(x)
}


#' Defines a branch in a \code{ModelInput} object
#' 
#' A branch is any object that is not a leaf.
#' 
#' @author Andrew Roberts 
#' @export
is_model_input_branch <- function(x) {
  !is_model_input_leaf(x)
}
 

#' Test whether object is explicitly tagged as a ModelInput leaf
#'
#' @returns logical(1), \code{TRUE} if \code{x} is an \code{InputSlot} or \code{MetadataSlot}.
#' @export
is_tagged_leaf <- function(x) {
  is_input_slot(x) || is_metadata_slot(x)
}


#' ModelInput Constructor
#' 
#' Wraps a pure R nested list as a \code{ModelInput} object.
#' 
#' @details
#' The nested list must have names for all elements. The names at any branch
#' of the list must be unique.
#' 
#' 
ModelInput <- function(x, untagged_is_slot=TRUE) {
  assertthat::assert_that(is_pure_list(x), msg="ModelInput constructor expects a list.")
  x <- .validate_and_wrap(x, path=character(), untagged_is_slot)
  
  structure(list(.data=x), class="ModelInput")
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
  if (!is_model_input(x)) stop("Object is not a ModelInput.")
  
  invisible(TRUE)
}


#' Validate a Nested List and Identify Leaves
#'
#' Recursive validator to determine whether a nested list can be wrapped as a
#' \code{ModelInput}. Any leaves that are not already tagged as an \code{InputSlot}
#' or \code{MetadataSlot} will be wrapped. If \code{untagged_is_slot = TRUE} 
#' they will be wrapped as input slots; else metadata slots.
#' 
#' @author Andrew Roberts
#' @export
.validate_and_wrap <- function(x, path, untagged_is_slot) {

  if(is_model_input_leaf(x)) {
    if(is_input_slot(x) || is_metadata_slot(x)) return(x)
    
    # Object is not a list so is treated as a leaf. Assigned either as an
    # input slot or metadata slot.
    if(untagged_is_slot) return(InputSlot(x))
    else return(MetadataSlot(x))
  }
  
  # Ensure branch has unique names
  if(!is_pure_list(x) || !has_unique_names(x)) {
    stop(sprintf("All list elements must have unique names at level '%s'.",
                 .node_path_to_key(path)))
  }

  # Recurse on sub-tree
  out <- list()
  for(nm in names(x)) {
    out[[nm]] <- .validate_and_wrap(x[[nm]], c(path, nm), untagged_is_slot)
  }
  
  return(out)
}


#' Traverse Tree and Apply Function to Leaves
#'
#' Recursive tree traversal from left-to-right, depth first. The function
#' \code{f} is applied to each node.
#' 
#' @param x A ModelInput object
#' @param f A function with signature \code{f(node, path, ...)}.
#' @param ... Additional arguments passed to \code{f}.
#'
#' @returns list, of length equal to the number of leaves in the tree. The
#'  names of the list are set to the key paths of the form \code{a/b/c}. The values
#'  of the list are the return value of \code{f} applied to each leaf.
#'  
#' @author Andrew Roberts
#' @export
traverse_leaves <- function(x, f, ...) {
  check_model_input_type(x)
  out <- list()
  
  recurse <- function(node, path=character()) {
    if(is_model_input_leaf(node)) {
      key <- .node_path_to_key(path)
      out[[key]] <<- f(node, path, ...)
    } else if(is_model_input_branch(node)) {
      for(nm in names(node)) {
        recurse(node[[nm]], c(path, nm))
      }
    } else {
      .raise_input_node_type_error(path=path)
    }
  }
  
  recurse(x$.data, path=character())
  out
}


#' Return flat list of leaves in ModelInput tree
#'
#' @param x a ModelInput object
#'
#' @returns Flat (non-nested) list of leaves in a \code{ModelInput} tree,
#' with names set to the respective key paths. This function retains the
#' \code{InputSlot} and \code{MetadataSlot} labels on the leaf objects.
#' See the \code{\link{input_slots}} and \code{\link{metadata_slots}} methods
#' to return the raw leaf values.
#'
#' @author Andrew Roberts
#' @export
flatten_model_input <- function(x) {
  
  check_model_input_type(x)
  traverse_leaves(x, function(x, ...) x)
}


#' Construct ModelInput tree from flat list
#'
#' @details
#' Almost acts as an inverse to the methods \code{\link{input_slots}} and
#' \code{\link{metadata_slots}}. Feeding the returned values of these methods
#' to this function is guaranteed to construct a tree with the same leaves,
#' where the input slot leaves are in the same order, and the metadata leaves
#' are in the same order. However, the overall ordering of the leaves may be
#' different, as this function provides no way to specify how the input slot
#' and metadata leaves should be interleaved when inserting them into the tree.
#'
#' @param slots a flat list of the form returned by \code{\link{input_slots}}.
#' @param metadata a flat list of the form returned by \code{\link{metadata_slots}}.
#'
#' @returns A \code{ModelInput} object, with leaves set to the elements of
#'  \code{slots} and \code{metadata}.
#'
#' @author Andrew Roberts
#' @export
unflatten_model_input <- function(slots, metadata=NULL) {

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

  ModelInput(tree)
}


#' Extract input slots from a ModelInput
#' 
#' Returns the input slots (a named list) within a \code{ModelInput} object.
#' This is an alias for \code{flatten_input_slots(x, return_raw_values=TRUE)}.
#'
#' @param x A \code{ModelInput} object.
#' @returns List of values from the input slot leaves. Names are set to key paths.
#'
#' @author Andrew Roberts
#' @export
input_slots <- function(x) {
  flatten_input_slots(x, return_raw_values=TRUE)
}


#' Extract metadata slots from a ModelInput
#' 
#' Returns the metadata slots (a named list) within a \code{ModelInput} object.
#' This is an alias for \code{flatten_metadata_slots(x, return_raw_values=TRUE)}.
#'
#' @param x A \code{ModelInput} object.
#' @returns List of values from the metadata slot leaves. Names are set to key paths.
#'
#' @author Andrew Roberts
#' @export
metadata_slots <- function(x) {
  flatten_metadata_slots(x, return_raw_values=TRUE)
}


#' Extract input slots from a ModelInput
#' 
#' Returns the input slots (a named list) within a \code{ModelInput} object.
#' Optionally returns the raw node values, or the \code{InputSlot}-wrapped
#' objects.
#'
#' @param x A \code{ModelInput} object.
#' @returns List of input slot leaves. If \code{return_raw_values=TRUE} these 
#'  will be the raw values (i.e., the \code{InputSlot} class is dropped).
#'  Otherwise, the \code{InputSlot} class is maintained.
#'
#' @author Andrew Roberts
#' @export
flatten_input_slots <- function(x, return_raw_values=TRUE) {
  
  check_model_input_type(x)
  
  return_input_slot <- function(x, ...) {
    if(is_input_slot(x)) {
       if(return_raw_values) x$value else x
    } else {
      NULL
    }
  }
  
  out <- traverse_leaves(x, return_input_slot)
  Filter(Negate(is.null), out)
}


#' Extract metadata slots from a ModelInput
#' 
#' Returns the metadata slots (a named list) within a \code{ModelInput} object.
#' Optionally returns the raw node values, or the \code{MetadataSlot}-wrapped
#' objects.
#'
#' @param x A \code{ModelInput} object.
#' @returns List of metadata slot leaves. If \code{return_raw_values=TRUE} these 
#'  will be the raw values (i.e., the \code{MetadataSlot} class is dropped).
#'  Otherwise, the \code{MetadataSlot} class is maintained.
#'
#' @author Andrew Roberts
#' @export
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


#' Extract ModelInput nodes using bracket indexing
#' 
#' @details
#' Following R convention, \code{`[[`} will throw an error if the key path
#' does not exist, while \code{`$`} will instead return \code{NULL}.
#' 
#' @param x A ModelInput object
#' @param i character, either a string key path of the form \code{a/b/c} or
#'  a vector path of the form \code{c("a", "b", "c")}. The special keyword
#'  \code{.data} will extract the internal list (i.e., strips the \code{ModelInput})
#'  class).
#' 
#' @returns If a node at the key path exists (is non-NULL), returns either a 
#' sub-tree or the leaf value. In the former case, the sub-tree retains the
#' \code{ModelInput} class. In the latter case, the actual value of the node
#' is returned (i.e., the \code{InputSlot}/\code{MetadataSlot} class attribute is
#' stripped). Throws error if no node exists at the key path.
#' 
#' @author Andrew Roberts
#' @export
`[[.ModelInput` <- function(x, i, ...) {
  
  # Allow access to internal data.
  if(identical(i, ".data")) return(unclass(x)[[i]])
  
  node <- .resolve_model_input_path(x, i, error_if_missing=TRUE)

  if(is_model_input_leaf(node)) {
    return(node$value)
  } else {
    structure(list(.data=node), class=class(x))
  }
}


#' Extract ModelInput nodes using \code{`$`} indexing
#'
#' Identical to \code{`[[.ModelInput`}, except that \code{NULL} is returned
#' when no node is found at the key path.
#' 
#' @author Andrew Roberts
#' @export
`$.ModelInput` <- function(x, name) {
  
  # Allow access to internal data.
  if(name == ".data") return(unclass(x)$.data)
  
  # Same as x[[name]], but with NULL for non-existent keys
  node <- .resolve_model_input_path(x, name, error_if_missing=FALSE)
  if(is.null(node)) return(NULL)
  
  if(is_model_input_leaf(node)) {
    return(node$value)
  } else {
    structure(list(.data=node), class=class(x))
  }
}


#' Extract node from a ModelInput by its key path
#'
#' Given a key path (in string or vector form), extract the value at that
#' path. The object is extracted as is, without consideration of its class (i.e.,
#' the \code{ModelInput} class will be stripped, even if the object is a valid
#' branch/sub-tree). If no node is found at the key path, either an error is 
#' thrown or \code{NULL} is returned, depending on the argument \code{error_if_missing}.
#' 
#' @param x A \code{ModelInput} object.
#' @param path character, either a string key path of the form \code{a/b/c} or
#'  a vector path of the form \code{c("a", "b", "c")}.
#' @param error_if_missing logical(1), if \code{TRUE} throws an error if no node
#'  exists at the path; otherwise returns \code{NULL} in this case.
#'  
#' @returns The node at the key path. May return \code{NULL}
#'  if \code{error_if_missing = TRUE} and no node exists at the path.
#'  
#' @author Andrew Roberts  
.resolve_model_input_path <- function(x, path, error_if_missing=TRUE) {
  check_model_input_type(x)
  tree <- x$.data
  
  # Convert to node path.
  if(is.character(path) &&  length(path) == 1L) {
    path <- .node_key_to_path(path)
  }
  
  for(node_name in path) {
    if(is_model_input_branch(tree) && !is.null(tree[[node_name]])) {
      tree <- tree[[node_name]]
    } else {
      if(error_if_missing) stop(sprintf("Path %s does not exist", .node_path_to_key(path)))
      else return(NULL)
    }
  }
  
  return(tree)
}


#' Convert vector key path to string
#'
#' Given a node path of the form \code{c("a", "b", "c")}, converts to 
#' the string \code{"a/b/c"}.
#'
#' @author Andrew Roberts
.node_path_to_key <- function(node_path) {
  paste(node_path, collapse="/")
}


#' Convert string key path to vector
#'
#' Given a node key of the form \code{"a/b/c"}, converts to  the vector 
#' \code{c("a", "b", "c")}. Key should not begin with "/".
#'
#' @author Andrew Roberts
.node_key_to_path <- function(node_key) {
  strsplit(node_key, split="/", fixed=TRUE)[[1]]
}


#' Return vector of leaf keys
#'
#' @param x A \code{ModelInput} object.
#'
#' @returns character, vector of leaf keys, using \code{a/b/c} convention.
#'  Ordered by the flattening convention in \code{\link{traverse_leaves}}.
#' 
#' @author Andrew Roberts
#' @export
leaf_names <- function(x) {
  check_model_input_type(x)
  names(flatten_model_input(x))
}


#' Return vector of input slot keys
#'
#' Returns the names of the input slot keys present in a model input object.
#' Defined for both \code{ModelInput} and \code{EnsembleInput}.
#'
#' @param x A \code{ModelInput} or \code{EnsembleInput} object.
#' @param ... Further arguments passed to methods.
#'
#' @return character vector of input slot names.
#' @seealso \code{\link{input_names.ModelInput}}, \code{\link{input_names.EnsembleInput}}
#'
#' @author Andrew Roberts
#' @export
input_names <- function(x, ...) {
  UseMethod("input_names")
}


#' @export
input_names.default <- function(x, ...) {
  raise_default_method_error(x, "input_names")
}


#' Extract input slot names from a \code{ModelInput}.
#' @export
input_names.ModelInput <- function(x) {
  nm <- names(flatten_input_slots(x))
  
  if(is.null(nm)) character(0)
  else nm
}


#' Return vector of metadata slot keys
#'
#' Returns the names of the metadata slot keys present in a model input object.
#' Defined for both \code{ModelInput} and \code{EnsembleInput}.
#'
#' @param x A \code{ModelInput} or \code{EnsembleInput} object.
#' @param ... Further arguments passed to methods.
#'
#' @return character vector of metadata slot names.
#' @seealso \code{\link{metadata_names.ModelInput}}, \code{\link{metadata_names.EnsembleInput}}
#'
#' @author Andrew Roberts
#' @export
metadata_names <- function(x, ...) {
  UseMethod("metadata_names")
}


#' @export
metadata_names.default <- function(x, ...) {
  raise_default_method_error(x, "metadata_names")
}


#' Extract input slot names from a \code{ModelInput}.
#' @export
metadata_names.ModelInput <- function(x) {
  nm <- names(flatten_metadata_slots(x))
  if(is.null(nm)) character(0)
  else nm
}


#' Number of leaves in a ModelInput tree
#' @export
n_leaves <- function(x) {
  length(flatten_model_input(x))  
}


#' Number of Input Slots Generic
#'
#' Returns the number of input slots present in a model input object.
#' Defined for both single \code{ModelInput} and \code{EnsembleInput}.
#'
#' @param x A \code{ModelInput} or \code{EnsembleInput} object.
#' @param ... Further arguments passed to methods.
#'
#' @return Integer, number of slots.
#' @seealso \code{\link{n_inputs.ModelInput}}, \code{\link{n_inputs.EnsembleInput}}
#' 
#' @author Andrew Roberts
#' @export
n_inputs <- function(x, ...) {
  UseMethod("n_inputs")
}


#' @export
n_inputs.default <- function(x, ...) {
  raise_default_method_error(x, "n_inputs")
}


#' Number of input slots in a ModelInput
#' @export
n_inputs.ModelInput <- function(x) {
  length(flatten_input_slots(x))
}


#' Number of metadata leaves in a ModelInput tree
#' @export
n_metadata <- function(x) {
  length(flatten_metadata_slots(x))
}


#' Depth of a ModelInput tree
#'
#' The root of the tree is defined to have depth zero. A flat list has depth
#' one, and so on.
#' 
#' @param x A \code{ModelInput} object.
#' 
#' @author Andrew Roberts
#' @export
tree_depth <- function(x) {
  check_model_input_type(x)
  
  recurse <- function(node, depth) {
    if(is_model_input_leaf(node)) return(depth)
    if(length(node) == 0L) return(depth) # Empty list
    max(vapply(node, recurse, integer(1), depth=depth+1L))
  }
  
  recurse(x$.data, depth=0L)
}


#' Print method for a \code{ModelInput}
#' @export
print.ModelInput <- function(x) {
  
  input_summary <- sprintf("ModelInput(n_slots = %s, n_metadata = %s, depth = %s)\n",
                           as.character(n_inputs(x)), as.character(n_metadata(x)), 
                           as.character(tree_depth(x)))
  
  cat(input_summary)
}


#' Summary method for a \code{ModelInput}
#' @export
summary.ModelInput <- function(x, ...) {
  
  check_model_input_type(x)

  input_nm <- slot_names(x)
  meta_nm <- metadata_names(x)
  num_slots <- length(input_nm)
  num_meta <- length(meta_nm)
  
  cat(sprintf("ModelInput with %d slots, %d metadata, depth %d\n",
              num_slots, num_meta, tree_depth(x)))
  
  if(num_slots > 0L) {
    cat("Slots:    ", paste(input_nm, collapse = ", "), "\n")
  }
  
  if(num_meta > 0L) {
    cat("Metadata: ", paste(meta_nm, collapse = ", "), "\n")
  }
  
  invisible(x)
}


#' Visualize ModelInput tree structure 
#'
#' Uses a directory-like visualization to print the tree structure.
#'
#' @param x A \code{ModelInput} object.
#' @param prefix character(1), string prefix to print at beginning of each line.
#' @param include_leaf_class logical(1), if \code{TRUE} prints the class of the 
#'  value of each leaf, in addition to its input slot or metadata slot label.
#'
#' @export
print_tree <- function(x, prefix="", include_leaf_class=TRUE) {
  check_model_input_type(x)

  recurse <- function(node, name=NULL, prefix="", is_last=TRUE) {
    connector <- if (is_last) "└── " else "├── "
    new_prefix <- if (is_last) paste0(prefix, "    ") else paste0(prefix, "│   ")
    
    # Leaf label
    if(is_tagged_leaf(node)) {
      if(is_input_slot(node)) label <- paste0(name, " [InputSlot")
      else if(is_metadata_slot(node)) label <- paste0(name, " [MetadataSlot")
      else stop("No tag defined for leaf with class: ", paste(class(node), collapse=", "))
      
      if(include_leaf_class) label <- paste(label, class(node$value)[1], sep=", ")
      label <- paste0(label, "]")
    } else {
      label <- name
    }
    
    # Print this node (skip root if NULL name)
    if(!is.null(name)) {
      cat(prefix, connector, label, "\n", sep="")
    }
    
    # Recurse on sub-tree
    if(is_model_input_branch(node)) {
      nms <- names(node)
      for(i in seq_along(nms)) {
        recurse(node[[i]], nms[i], new_prefix, i == length(nms))
      }
    }
  }
  
  recurse(x$.data, name=NULL, prefix=prefix, is_last=TRUE)
  invisible(x)
}


#' Assign value in nested list by key path
#' 
#' Helper for \code{ModelInput} constructor.
#' 
#' @param tree named list, potentially nested.
#' @param path character, vector key path to a node in the tree.
#' @param value R object to assign as the node value at the specified path.
#' 
#' @returns The updated nested list with the value assigned at the specified
#'  path. Throws error if a value is already found at that path.
#'
#' @author Andrew Roberts
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
       ". ModelInput nodes must be InputSlots, MetadataSlots, or pure lists.")
}








