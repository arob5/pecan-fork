# prob.tools/tests/testthat/test-pecan-model_input.r
#
# Depends: testthat, PEcAn.settings

test_that("pecan model input constructor works as expected", {
  
  l_config <- list(a=1, b=list(c=2))
  l_settings <- list(d=3, e=list(f=4))
  l <- setNames(list(l_config, l_settings), 
                c(CONFIG_BRANCH_KEY, SETTINGS_BRANCH_KEY))
  mi <- ModelInput(l)
  
  expect_equal(PecanModelInput(l_config, l_settings), mi)
  expect_equal(PecanModelInput(l_config), 
               ModelInput(list(config=l_config, settings=list())))
  expect_equal(PecanModelInput(settings_tree=l_settings), 
               ModelInput(list(config=list(), settings=l_settings)))
  expect_equal(PecanModelInput(), ModelInput(list(config=list(), settings=list())))
  expect_equal(PecanModelInput(base_tree=list(a=1, b=list(c=2))), 
               ModelInput(c(list(a=1, b=list(c=2)), list(config=list()), list(settings=list()))))
})


test_that("Correctly overwrite pecan settings", {

  l_config <- list(a=1, b=list(c=2))
  l_settings <- list(d=3, e=list(f=4), g=list())
  l <- setNames(list(l_config, l_settings), 
                c(CONFIG_BRANCH_KEY, SETTINGS_BRANCH_KEY))
  mi <- ModelInput(l)
  
  defaults <- list(a=0, b=list(d=0, c=0), e=list(z=0, f=0), g=0)
  
  updated <- update_pecan_settings(defaults, mi)
  # TODO: finish this test.
})





