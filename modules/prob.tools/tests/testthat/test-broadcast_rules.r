# prob.tools/tests/testthat/test-broadcast_rules.r
#
# Depends: testthat

test_that(".standardize_list_rule produces valid broadcast rule setup", {
  
  axes <- c("a", "b", "c")
  expect_equal(.standardize_list_rule(axes, list(recycle = c("a", "b")), FALSE),
               list(axis_names=axes, list_rule=list(rule_recycle=c("a","b"), rule_identity="c")))
  expect_equal(.standardize_list_rule(axes, list(recycle = c("a", "b")), TRUE),
               list(axis_names=c("a","b"), list_rule=list(rule_recycle=c("a","b"))))
  
  expect_error(.standardize_list_rule("a", list(), FALSE))
  expect_error(.standardize_list_rule("a", list("a"), FALSE))
  expect_error(.standardize_list_rule("a", list(identity = "b"), FALSE))
  expect_error(.standardize_list_rule("a", list(identity = NULL), FALSE))
  expect_error(.standardize_list_rule(c("a", "b"), list(identity="a", match=c("a", "b")), FALSE))
})


test_that("get_broadcast_rule produces valid broadcast rule function", {
  axes <- c("a", "b", "c")
  list_rule <- list(recycle=c("a","c"))
  rule <- get_broadcast_rule(axes, list_rule, drop_absent_axes=FALSE)
  
  lens <- c(a=2, b=3, c=4)
  mat_correct <- cbind(rep(1:2, 6), rep(1:3,c(4,4,4)), rep(1:4, 3))
  colnames(mat_correct) <- names(lens)
  
  expect_equal(rule(lens), mat_correct)
  expect_error(rule(c(2,3,4)))
})

