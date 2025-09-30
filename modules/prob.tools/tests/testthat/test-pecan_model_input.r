# prob.tools/tests/testthat/test-pecan-model_input.r
#
# Depends: testthat, PEcAn.settings

test_that("pecan model input constructor works as expected", {
  l_other <- list(h=5, i=list(j=6))
  l <- c(l_other, list(pecan=l_pecan))
  model_input <- ModelInput(l)
  
  expect_equal(model_input, PecanModelInput(l_other, l_pecan))
  
  l_other <- list(pecan=1, h=5, i=list(j=6))
  expect_error(PecanModelInput(l_other, l_pecan))
})


test_that("Correctly overwrite pecan settings", {

  l_pecan <- list(a=1, b=list(c=list(d=2, e=3)), f=list(g=4))
  l_other <- list(h=5, i=list(j=6))
  x <- PecanModelInput(l_other, l_pecan)
  
  expect_equal(update_pecan_settings(list(), model_input), ModelInput(l_pecan)$.data)
  
  settings <- list(x=1, y=list(z=2), a=3, b=list(c=list(e=100)))
  settings_updated <- list(x=1, y=list(z=2), a=1, b=list(c=list(e=2, d=3)), f=list(g=4))
  expect_equal(update_pecan_settings(settings, x), settings_updated)
})





