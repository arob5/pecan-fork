# prob.tools/tests/testthat/test-batched_array.r
#
# Depends: testthat

test_that("only vectors and arrays are array like", {
  expect_false(is_array_like(list()))
  expect_false(is_array_like(data.frame()))
  expect_false(is_array_like(NULL))
  expect_true(is_array_like(array()))
  expect_true(is_array_like(matrix()))
  expect_true(is_array_like(numeric()))
})


test_that("vectors/1d arrays treated as singleton array batch", {
  expect_equal(.wrap_vector_as_batch_array(1), matrix(1))
  expect_equal(.wrap_vector_as_batch_array(1:4), matrix(1:4, nrow=1L))
  expect_equal(.wrap_vector_as_batch_array(array(1, dim=1)), matrix(1))
  expect_equal(.wrap_vector_as_batch_array(matrix(1:4, nrow=1L)), matrix(1:4, nrow=1L))
  expect_equal(.wrap_vector_as_batch_array(list()), list())
  expect_equal(.wrap_vector_as_batch_array(NULL), NULL)
})


test_that("flat vectors wrapped as one row matrices", {
  expect_equal(.wrap_vector_as_flat_array(1), matrix(1))
  expect_equal(.wrap_vector_as_flat_array(1:4), matrix(1:4, nrow=1L))
  expect_equal(.wrap_vector_as_flat_array(array(1, dim=1)), matrix(1))
  expect_equal(.wrap_vector_as_flat_array(matrix(1:4, nrow=1L)), matrix(1:4, nrow=1L))
  expect_equal(.wrap_vector_as_flat_array(list()), list())
  expect_equal(.wrap_vector_as_flat_array(NULL), NULL)
})


test_that("length one batch axis is prepended", {
  expect_equal(.add_batch_axis_to_array(1), matrix(1))
  expect_equal(.add_batch_axis_to_array(array(1, dim=1L)), matrix(1, nrow=1L))
  expect_equal(.add_batch_axis_to_array(matrix(1:3, ncol=1L)), array(1:3, dim=c(1,3,1)))
  expect_equal(.add_batch_axis_to_array(array(1:3, dim=c(1,3,1))), array(1:3, dim=c(1,1,3,1)))
  expect_error(.add_batch_axis_to_array(NULL))
})


test_that("Batch array validated correctly", {
  expect_equal(.wrap_and_check_batch_array(1), matrix(1))
  expect_equal(.wrap_and_check_batch_array(matrix(1)), matrix(1))
  expect_equal(.wrap_and_check_batch_array(array(dim=1L)), matrix())
  expect_equal(.wrap_and_check_batch_array(array(1:3, dim=c(1,1,3))), array(1:3, dim=c(1,1,3)))
  expect_equal(.wrap_and_check_batch_array(1:3), matrix(1:3, nrow=1L))
  expect_error(.wrap_and_check_batch_array(list()))
  expect_error(.wrap_and_check_batch_array(NULL))
})


test_that("Flat array validated correctly", {
  expect_equal(.wrap_and_check_flat_array(1, 1), matrix(1))
  expect_equal(.wrap_and_check_flat_array(1, c(1,1)), matrix(1))
  expect_equal(.wrap_and_check_flat_array(1, list(c(1,1))), matrix(1))
  expect_equal(.wrap_and_check_flat_array(1:12, list(c(3,2), 4, c(2,1))), matrix(1:12, nrow=1L))
  expect_equal(.wrap_and_check_flat_array(1:12, 12), matrix(1:12, nrow=1L))
  expect_equal(.wrap_and_check_flat_array(matrix(1:36, nrow=3, ncol=12), list(c(2,4), 4)), 
               matrix(1:36, nrow=3, ncol=12))
  expect_error(.wrap_and_check_flat_array(list(), 1))
  expect_error(.wrap_and_check_flat_array(NULL, 1))
  expect_error(.wrap_and_check_flat_array(1:12, 13))
  expect_error(.wrap_and_check_flat_array(matrix(1:36, nrow=3, ncol=12), list(c(2,4), 3)))
})


test_that("Batch array list is a list of batch arrays with equal batch size", {
  
  expect_equal(.wrap_and_check_batch_array_list(list()), list()) # TODO: is this the behavior we want?
  expect_equal(.wrap_and_check_batch_array_list(list(1)), list(matrix(1)))
  
  l <- list(1, 2:4, array(5:10, dim=c(1,1,3,2)))
  l_wrapped <- list(matrix(1), matrix(2:4, nrow=1L), array(5:10, dim=c(1,1,3,2)))
  expect_equal(.wrap_and_check_batch_array_list(l), l_wrapped)
  
  l_diff_batch_size <- list(1, 2:4, array(5:10, dim=c(2,1,3)))
  expect_error(.wrap_and_check_batch_array_list(l_diff_batch_size))
})


test_that(".batch_array_to_flat", {
  expect_equal(.batch_array_to_flat(1), matrix(1))
  expect_equal(.batch_array_to_flat(array(dim=1L)), matrix())
  expect_equal(.batch_array_to_flat(matrix(1:3, nrow=1L)), matrix(1:3, nrow=1L))
  
  arr1 <- array(1:12, dim=c(3,2,3))
  expect_equal(.batch_array_to_flat(arr1), array(1:12, dim=c(3,6)))
  
  arr2 <- array(1:12, dim=c(1,1,12))
  expect_equal(.batch_array_to_flat(arr2), matrix(1:12, nrow=1L))
})


test_that(".flat_to_batch_array", {
  expect_equal(.flat_to_batch_array(1, matrix(1)), matrix(1))
  expect_equal(.flat_to_batch_array(1:5, c(5,1)), array(1:5, dim=c(1,5,1)))
  expect_equal(.flat_to_batch_array(matrix(1:10, nrow=2L), c(5,1)), array(1:10, dim=c(2,5,1)))
  expect_error(.flat_to_batch_array(NULL))
  expect_error(.flat_to_batch_array(list()))
})


test_that(".array_list_to_flat", {
  expect_equal(.array_list_to_flat(list()), NULL) # TODO: is this the behavior we want?
  expect_equal(.array_list_to_flat(list(1)), matrix(1))
  
  arr <- array(4:12, dim=c(3,2,2))
  l <- list(1, 2:3, arr)
  l_batch <- list(matrix(1:3, nrow=3), arr)
  l_flat <- matrix(c(1, 2:3, as.vector(arr)), nrow=1L)
  l_batch_flat <- rbind(c(1, arr[1,,]), c(2, arr[2,,]), c(3, arr[3,,]))
  
  expect_equal(.array_list_to_flat(l), l_flat)
  expect_equal(.array_list_to_flat(l_batch, arrays_are_batch=TRUE), l_batch_flat)
  expect_error(.array_list_to_flat(l, arrays_are_batch=TRUE))
  
  expect_error(.array_list_to_flat(1))
})


test_that(".flat_to_batch_array_list", {
  expect_equal(.flat_to_batch_array_list(1, 1), list(matrix(1)))
  expect_equal(.flat_to_batch_array_list(1:3, list(c(2,1), 1)), 
               list(array(1:2, dim=c(1,2,1)), matrix(3)))
  
  flat <- matrix(1:30, nrow=2L)
  l1 <- list(array(1:24, dim=c(2,3,4)), array(25:30, dim=c(2,1,3)))
  l2 <- list(array(1:30, dim=c(2,5,3)))
  
  expect_equal(.flat_to_batch_array_list(flat, list(c(3,4), c(1,3))), l1)
  expect_equal(.flat_to_batch_array_list(flat, c(5,3)), l2)
  expect_error(.flat_to_batch_array_list(flat, c(5,2)))
  
  expect_error(.flat_to_batch_array_list(1, list(1,1)))
  expect_error(.flat_to_batch_array_list(1:3, list(c(2,1))))
})


