# prob.tools/tests/testthat/test-pecan-model_input.r
#
# Depends: testthat, PEcAn.settings

test_that("Correctly overwrite pecan settings", {

  l_pecan <- list(a=1, b=list(c=list(d=2, e=3)), f=list(g=4))
  l_other <- list(h=5, i=list(j=6))
  l <- list(pecan=l_pecan, other=l_other)
  model_input <- ModelInput(l)
  
  expect_equal(update_pecan_settings(list(), model_input), ModelInput(l_pecan)$.data)
  
  test <- update_pecan_settings(list(), model_input)
  
})
