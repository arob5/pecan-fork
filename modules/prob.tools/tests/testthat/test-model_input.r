# prob.tools/tests/testthat/test-model_input.r
#
# Depends: testthat

test_that(".resolve_model_input_path() correctly extracts node", {
  
  l <- list(a=MetadataSlot(1), b=list(c=2, d=list(e=3, f=4)))
  x <- ModelInput(l)
  a_element <- MetadataSlot(1)
  e_element <- InputSlot(3)
  d_branch <- list(e=InputSlot(3), f=InputSlot(4))
  
  expect_equal(.resolve_model_input_path(x, "a"), MetadataSlot(1))
  expect_equal(.resolve_model_input_path(x, "a/"), MetadataSlot(1))
  expect_equal(.resolve_model_input_path(x, "b/d/e"), e_element)
  expect_equal(.resolve_model_input_path(x, c("b", "d", "e")), e_element)
  expect_equal(.resolve_model_input_path(x, "b/d/"), d_branch)
  expect_equal(.resolve_model_input_path(x, c("b","d")), d_branch)
  
  expect_null(.resolve_model_input_path(x, "a/b", error_if_missing=FALSE))
  expect_null(.resolve_model_input_path(x, c("a","b"), error_if_missing=FALSE))
  
  expect_error(.resolve_model_input_path(x, "a/b"))
  expect_error(.resolve_model_input_path(x, c("a", "b")))
  expect_error(.resolve_model_input_path(x, "not/a/path"))
  expect_error(.resolve_model_input_path(x, c("not", "a", "path")))
})


test_that("Bracket indexing works correctly", {
  
  l <- list(a=MetadataSlot(1), b=list(c=2, d=list(e=3, f=4)))
  l_wrapped <- .validate_and_wrap(l, untagged_is_slot=TRUE)
  x <- ModelInput(l)
  a_element <- 1
  e_element <- 3
  b_subtree <- ModelInput(l$b)
  
  expect_equal(x[[".data"]], l_wrapped)
  expect_equal(x[["a"]], a_element)
  expect_equal(x[["b/d/e"]], e_element)
  expect_equal(x[[c("b", "d", "e")]], e_element)
  expect_equal(x[["b"]], b_subtree)
  
  expect_error(x[["not/a/path"]])
  expect_error(x[[c("not", "a", "path")]])
})


test_that("Dollar sign indexing works correctly", {
  
  l <- list(a=MetadataSlot(1), b=list(c=2, d=list(e=3, f=4)))
  l_wrapped <- .validate_and_wrap(l, untagged_is_slot=TRUE)
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


test_that("leaf_names extracts leaf keys", {
  l <- list(a=MetadataSlot(1), b=list(c=2, d=list(e=3, f=4)))
  keys <- c("a", "b/c", "b/d/e", "b/d/f")
  x <- ModelInput(l)
  
  expect_equal(leaf_names(x), keys)
  expect_error(leaf_names(list(a=1, b=2)))
})


test_that("input_names extracts input slot keys", {
  l <- list(a=MetadataSlot(1), b=list(c=2, d=list(e=3, f=MetadataSlot(4))), g=10)
  keys <- c("b/c", "b/d/e", "g")
  x <- ModelInput(l)
  x_all_metadata <- ModelInput(list(a=1, b=list(c=2, d=3)), untagged_is_slot=FALSE)
  
  expect_equal(input_names(x), keys)
  expect_equal(input_names(x_all_metadata), character(0))
})


test_that("metadata_names extracts metadata slot keys", {
  l <- list(a=MetadataSlot(1), b=list(c=2, d=list(e=3, f=MetadataSlot(4))), g=10)
  keys <- c("a", "b/d/f")
  x <- ModelInput(l)
  x_no_metadata <- ModelInput(list(a=1, b=list(c=2, d=3)), untagged_is_slot=TRUE)
  
  expect_equal(metadata_names(x), keys)
  expect_equal(metadata_names(x_no_metadata), character(0))
})


test_that("leaf count methods work correctly", {
  l <- list(a=MetadataSlot(1), b=list(c=2, d=list(e=3, f=MetadataSlot(4))), g=10)
  x <- ModelInput(l)
  x_no_metadata <- ModelInput(list(a=1, b=list(c=2, d=3)), untagged_is_slot=TRUE)
  x_no_inputs <- ModelInput(list(a=1, b=list(c=2, d=3)), untagged_is_slot=FALSE)
  
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



  