# prob.tools/tests/testthat/test-model_input.r
#
# Depends: testthat

test_that("Key path conversion works correctly", {
  expect_equal(.parse_key_path("a"), "a")
  expect_equal(.parse_key_path("a/b/c", FALSE), c("a", "b", "c"))
  expect_equal(.parse_key_path("a/b/c/", FALSE), c("a", "b", "c"))
  expect_equal(.parse_key_path(c("a", "b", "c"), FALSE), c("a", "b", "c"))
  expect_equal(.parse_key_path(c("a", "b", "c"), TRUE), "a/b/c")
  
  expect_error(.parse_key_path(NULL, TRUE))
})


test_that("empty branches are handled correctly", {
  
  x <- ModelInput(list())
  y <- ModelInput(list(a=list(b=list())))
  
  expect_true(model_input_is_empty(x))
  expect_equal(x$.data, list())
  expect_equal(tree_depth(x), 0L)
  expect_equal(n_leaves(x), 0L)
  expect_equal(n_inputs(x), 0L)
  expect_equal(n_metadata(x), 0L)
  expect_equal(leaf_keys(x), character(0))
  expect_equal(input_keys(x), character(0))
  expect_equal(metadata_keys(x), character(0))
  expect_equal(leaf_names(x), character(0))
  expect_equal(input_names(x), character(0))
  expect_equal(metadata_names(x), character(0))
  
  expect_false(model_input_is_empty(y))
  expect_equal(y$.data, list(a=list(b=list())))
  expect_equal(tree_depth(y), 2L)
  expect_equal(n_leaves(y), 0L)
  expect_equal(n_inputs(y), 0L)
  expect_equal(n_metadata(y), 0L)
  expect_equal(leaf_keys(y), character(0))
  expect_equal(input_keys(y), character(0))
  expect_equal(metadata_keys(y), character(0))
  expect_equal(leaf_names(y), character(0))
  expect_equal(input_names(y), character(0))
  expect_equal(metadata_names(y), character(0))
})


test_that("leaf apply function works correctly", {
  l <- list(a=1, b=list(c=list(d=2, e=3, f=list(g=4))), h=list(i=5))
  l_flat <- list(a=1, `b/c/d`=2, `b/c/e`=3, `b/c/f/g`=4, `h/i`=5)
  keys <- names(l_flat); names(keys) <- keys
  l_wrapped <- .validate_and_wrap_model_input(l, untagged_is_input=TRUE)
  x <- ModelInput(l)

  expect_equal(apply_to_leaves(x, function(x, ...) x), l_wrapped)
  expect_equal(apply_to_leaves(x, function(x, ...) x),
               .apply_to_leaves(x$.data, function(x, ...) x))
  expect_equal(apply_to_leaves(x, function(x, ...) x$value), l)
  expect_equal(apply_to_leaves(x, function(x, ...) x$value, flatten=TRUE), l_flat)
  expect_equal(apply_to_leaves(x, function(x, path, ...) .node_path_to_key(path), flatten=TRUE),
               as.list(keys))
  expect_error(.apply_to_leaves(x, function(x, ...) x))
  
  # Test handling of NULL values.
  extract_even <- function(x, ...) if(x$value %% 2 == 0) x$value else NULL
  evens_flat <- as.list(l_flat[unlist(l_flat) %% 2 == 0])
  evens_tree <- list(b=list(c=list(d=2, f=list(g=4))))
  evens_flat_with_nulls <- lapply(l_flat, function(x) if(x %% 2 == 0) x else NULL)
  evens_tree_with_nulls <- list(a=NULL, b=list(c=list(d=2, e=NULL, f=list(g=4))), h=list(i=NULL))
  
  expect_equal(apply_to_leaves(x, extract_even), evens_tree_with_nulls)
  expect_equal(apply_to_leaves(x, extract_even, flatten=TRUE), evens_flat_with_nulls)
  expect_equal(apply_to_leaves(x, extract_even, drop_null=TRUE), evens_tree)
  expect_equal(apply_to_leaves(x, extract_even, flatten=TRUE, drop_null=TRUE), evens_flat)
  
  expect_equal(apply_to_leaves(x, function(x, ...) NULL, drop_null=TRUE, flatten=TRUE), list())
  expect_equal(apply_to_leaves(x, function(x, ...) NULL, drop_null=TRUE, flatten=FALSE), list())
  
  # Behavior when there are empty branches
  y <- ModelInput(list())
  l_empty <- list(a=list(b=list()))
  z <- ModelInput(l_empty)
  
  id <- function(x, ...) x
  expect_equal(apply_to_leaves(y, id, flatten=FALSE, drop_null=FALSE), list())
  expect_equal(apply_to_leaves(y, id, flatten=TRUE, drop_null=FALSE), list())
  expect_equal(apply_to_leaves(y, id, flatten=FALSE, drop_null=TRUE), list())
  expect_equal(apply_to_leaves(y, id, flatten=TRUE, drop_null=TRUE), list())
  
  expect_equal(apply_to_leaves(z, id, flatten=FALSE, drop_null=FALSE), l_empty)
  expect_equal(apply_to_leaves(z, id, flatten=TRUE, drop_null=FALSE), list())
  expect_equal(apply_to_leaves(z, id, flatten=FALSE, drop_null=TRUE), list())
  expect_equal(apply_to_leaves(z, id, flatten=TRUE, drop_null=TRUE), list())
})


test_that("model input leaves are correctly flattened", {
  l <- list(a=1, b=list(c=list(d=2, e=3, f=list(g=4))), h=list(i=5))
  l_flat <- list(a=1, `b/c/d`=2, `b/c/e`=3, `b/c/f/g`=4, `h/i`=5)
  l_flat_wrapped <- lapply(l_flat, InputSlot)
  x <- ModelInput(l)
  
  expect_equal(flatten_model_input(x), l_flat_wrapped)
  expect_equal(flatten_model_input(x, TRUE), l_flat)
  expect_equal(as_list(x, TRUE, TRUE), l_flat)
  expect_equal(as_list(x, FALSE, TRUE), l_flat_wrapped)
  expect_equal(as_list(x, TRUE, FALSE), l)
  expect_equal(as_list(x, FALSE, FALSE), x$.data)
  
  expect_equal(flatten_model_input(ModelInput(list())), list())
  expect_equal(flatten_model_input(ModelInput(list(a=list(b=list())))), list())
})


test_that("model input and metadata slots are correctly flattened", {
  
  l <- list(a=1, b=list(c=list(d=MetadataSlot(2), e=3, f=InputSlot(list(g=4)))), h=list(i=MetadataSlot(5)))
  l_input_slots <- list(a=1, `b/c/e`=3, `b/c/f`=list(g=4))
  l_metadata_slots <- list(`b/c/d`=2, `h/i`=5)
  x <- ModelInput(l)
  
  expect_equal(input_slots(x), l_input_slots)
  expect_equal(metadata_slots(x), l_metadata_slots)
  expect_equal(input_slots(ModelInput(list(a=1), untagged_is_input=FALSE)), list())
  expect_equal(metadata_slots(ModelInput(list(a=1))), list())
  
  expect_equal(metadata_slots(ModelInput(list())), list())
  expect_equal(metadata_slots(ModelInput(list(a=list(b=list())))), list())
})


test_that("model input flat to tree conversion works properly", {
  
  l <- list(a=1, b=list(c=list(d=MetadataSlot(2), e=3, f=InputSlot(list(g=4)))), h=list(i=MetadataSlot(5)))
  x <- ModelInput(l)
  tree1 <- unflatten_model_input(input_slots(x), metadata_slots(x))
  tree2 <- unflatten_model_input(input_slots(x))
  tree3 <- unflatten_model_input(metadata=metadata_slots(x))
  
  expect_true(is_model_input(tree1))
  expect_equal(input_slots(tree1), input_slots(x))
  expect_equal(metadata_slots(tree1), metadata_slots(x))
  
  expect_true(is_model_input(tree2))
  expect_equal(input_slots(tree2), input_slots(x))
  expect_equal(metadata_slots(tree2), list())
  
  expect_true(is_model_input(tree3))
  expect_equal(input_slots(tree3), list())
  expect_equal(metadata_slots(tree3), metadata_slots(x))
  
  expect_equal(unflatten_model_input(), ModelInput(list()))
})


test_that(".get_tree_node_at_path() correctly extracts element from nested list", {
  
  a_element <- 1
  e_element <- 3
  d_branch <- list(e=e_element, f=4)
  l <- list(a=a_element, b=list(c=2, d=d_branch))

  expect_equal(.get_tree_node_at_path(l, "a"), a_element)
  expect_equal(.get_tree_node_at_path(l, "a/"), a_element)
  expect_equal(.get_tree_node_at_path(l, "b/d/e"), e_element)
  expect_equal(.get_tree_node_at_path(l, c("b", "d", "e")), e_element)
  expect_equal(.get_tree_node_at_path(l, "b/d/"), d_branch)
  expect_equal(.get_tree_node_at_path(l, c("b","d")), d_branch)
  
  expect_null(.get_tree_node_at_path(l, "a/b", error_if_missing=FALSE))
  expect_null(.get_tree_node_at_path(l, c("a","b"), error_if_missing=FALSE))
  
  expect_error(.get_tree_node_at_path(ModelInput(l), "a"))
  expect_error(.get_tree_node_at_path(l, "a/b"))
  expect_error(.get_tree_node_at_path(l, c("a", "b")))
  expect_error(.get_tree_node_at_path(l, "not/a/path"))
  expect_error(.get_tree_node_at_path(l, c("not", "a", "path")))
})


test_that("Bracket indexing works correctly", {
  
  l <- list(a=MetadataSlot(1), b=list(c=2, d=list(e=3, f=4)))
  l_wrapped <- .validate_and_wrap_model_input(l, untagged_is_input=TRUE)
  x <- ModelInput(l)
  a_element <- 1
  e_element <- 3
  b_subtree <- ModelInput(l$b)
  
  expect_equal(x[[".data"]], l_wrapped)
  expect_equal(x[["a"]], a_element)
  expect_equal(x[["b/d/e"]], e_element)
  expect_equal(x[[c("b", "d", "e")]], e_element)
  expect_equal(x[["b"]], b_subtree)
  
  expect_null(x[["not/a/path"]])
  expect_null(x[[c("not", "a", "path")]])
})


test_that("Dollar sign indexing works correctly", {
  
  l <- list(a=MetadataSlot(1), b=list(c=2, d=list(e=3, f=4)))
  l_wrapped <- .validate_and_wrap_model_input(l, untagged_is_input=TRUE)
  x <- ModelInput(l)
  a_element <- 1
  e_element <- 3
  b_subtree <- ModelInput(l$b)
  
  expect_equal(x$.data, l_wrapped)
  expect_equal(x$a, a_element)
  expect_equal(x$`b/d/e`, e_element)
  expect_equal(x$b, b_subtree)
  
  expect_null(x$`not/a/path`)
})


test_that("Assignment of ModelInput value works properly", {
  l <- list(a=MetadataSlot(1), b=list(c=2, d=list(e=3, f=4)))
  x <- ModelInput(l)
  
  x_add_g <- x; x_add_g$g <- 5
  expect_equal(x_add_g, ModelInput(c(l, list(g=5))))
  
  x_mod_e <- x; x_mod_e[["b/d/e"]] <- 100
  l_mod_e <- l; l_mod_e$b$d$e <- 100
  expect_equal(x_mod_e, ModelInput(l_mod_e))
  
  x_mod_b <- set_model_input_value(x, "b", 0, untagged_is_input=FALSE)
  l_mod_b <- l; l_mod_b$b <- 0
  expect_equal(x_mod_b, ModelInput(l_mod_b, untagged_is_input=FALSE))
  
  x_mod_a <- set_model_input_value(x, "a/x/y", 0)
  
  x_mod_a <- x; x_mod_a$`a/x/y/` <- list(z=10)
  l_mod_a <- l; l_mod_a[["a"]] <- list(x=list(y=list(z=10)))
  expect_equal(x_mod_a, ModelInput(l_mod_a))
  
  y <- ModelInput(list())
  y[["a/b/c"]] <- list()
  expect_equal(y$.data, list(a=list(b=list(c=list()))))
  
  y[["a/b/c"]] <- 2
  expect_equal(y$.data, list(a=list(b=list(c=InputSlot(2)))))
  
  y$a <- list()
  expect_equal(y$.data, list(a=list()))
})


test_that("leaf_keys extracts leaf keys", {
  l <- list(a=MetadataSlot(1), b=list(c=2, d=list(e=3, f=4)))
  keys <- c("a", "b/c", "b/d/e", "b/d/f")
  x <- ModelInput(l)
  
  expect_equal(leaf_keys(x), keys)
  expect_error(leaf_keys(list(a=1, b=2)))
})


test_that("input_keys extracts input slot keys", {
  l <- list(a=MetadataSlot(1), b=list(c=2, d=list(e=3, f=MetadataSlot(4))), g=10)
  keys <- c("b/c", "b/d/e", "g")
  x <- ModelInput(l)
  x_all_metadata <- ModelInput(list(a=1, b=list(c=2, d=3)), untagged_is_input=FALSE)
  
  expect_equal(input_keys(x), keys)
  expect_equal(input_keys(x_all_metadata), character(0))
})


test_that("metadata_keys extracts metadata slot keys", {
  l <- list(a=MetadataSlot(1), b=list(c=2, d=list(e=3, f=MetadataSlot(4))), g=10)
  keys <- c("a", "b/d/f")
  x <- ModelInput(l)
  x_no_metadata <- ModelInput(list(a=1, b=list(c=2, d=3)), untagged_is_input=TRUE)
  
  expect_equal(metadata_keys(x), keys)
  expect_equal(metadata_keys(x_no_metadata), character(0))
})


test_that("leaf count methods work correctly", {
  l <- list(a=MetadataSlot(1), b=list(c=2, d=list(e=3, f=MetadataSlot(4))), g=10)
  x <- ModelInput(l)
  x_no_metadata <- ModelInput(list(a=1, b=list(c=2, d=3)), untagged_is_input=TRUE)
  x_no_inputs <- ModelInput(list(a=1, b=list(c=2, d=3)), untagged_is_input=FALSE)
  
  expect_equal(n_leaves(x), 5L)
  expect_equal(n_inputs(x), 3L)
  expect_equal(n_metadata(x), 2L)
  
  expect_equal(n_leaves(x_no_metadata), 3L)
  expect_equal(n_inputs(x_no_metadata), 3L)
  expect_equal(n_metadata(x_no_metadata), 0L)
  
  expect_equal(n_leaves(x_no_inputs), 3L)
  expect_equal(n_inputs(x_no_inputs), 0L)
  expect_equal(n_metadata(x_no_inputs), 3L)
})


test_that("leaf depth computed correctly", {
  
  depth_one_tree <- ModelInput(list(a=InputSlot(list(a=list(b=2)))))
  
  expect_equal(tree_depth(ModelInput(list(a=1))), 1L)
  expect_equal(tree_depth(ModelInput(list(a=1, b=list(c=2, d=list(e=3))))), 3L)
  expect_equal(tree_depth(depth_one_tree), 1L)
})


test_that("Assignment in nested list works correctly", {
  
  expect_equal(.assign_tree_node_at_path(list(), "a", 1), list(a=1))
  expect_equal(.assign_tree_node_at_path(list(), "a/b/c", 1), list(a=list(b=list(c=1))))
  
  # Assigning values in R nested list.
  l <- list(a=list(b=list(c=1)), d=2)
  l_add_e <- l; l_add_e$a$b$e <- 3
  l_mod_c <- l; l_mod_c$a$b$c <- 3
  l_add_branch <- l; l_add_branch$d <- list()
  
  expect_equal(.assign_tree_node_at_path(l, c("a", "b", "e"), 3), l_add_e)
  expect_equal(.assign_tree_node_at_path(l, "a/b/e/", 3), l_add_e)
  expect_equal(.assign_tree_node_at_path(l, c("a", "b", "c"), 3, allow_overwrite=TRUE), l_mod_c)
  expect_equal(.assign_tree_node_at_path(l, "d", list(), allow_overwrite=TRUE), l_add_branch)
  expect_error(.assign_tree_node_at_path(l, c("a", "b", "c"), 3))
  expect_error(.assign_tree_node_at_path(l, c("d"), list()))
})









  