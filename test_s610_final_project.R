context("Check Approximate Bayesian Computation function")
source("s610_final_project.R")


###test output range
test_that("qc qh must be in [0,1]", { # since the initial q_c q_h is from Uniform (0,1)
  expect_true(all(generate_abc_smc_sample(data_A77) >=0 && generate_abc_smc_sample(data_A77) <=1))
})

test_that("qc qh must be in [0,1]", { # since the initial q_c q_h is from Uniform (0,1)
  expect_true(all(generate_abc_smc_sample(data_A80) >=0 && generate_abc_smc_sample(data_A80) <=1))
})

test_that("qc qh must be in [0,1]", { # since the initial q_c q_h is from Uniform (0,1)
  expect_true(all(generate_abc_smc_sample(data_ASE) >=0 && generate_abc_smc_sample(data_ASE) <=1))
})

test_that("qc qh must be in [0,1]", { # since the initial q_c q_h is from Uniform (0,1)
  expect_true(all(generate_abc_smc_sample(data_BSE) >=0 && generate_abc_smc_sample(data_BSE) <=1))
})


###test output length
test_that("llr output has 2,000 numeric outputs", { #1000 for qc, 1000 for qh
  expect_equal(length(generate_abc_smc_sample(data_A77)), 2000)
})

test_that("llr output has 2,000 numeric outputs", { #1000 for qc, 1000 for qh
  expect_equal(length(generate_abc_smc_sample(data_A80)), 2000)
})

test_that("llr output has 2,000 numeric outputs", { #1000 for qc, 1000 for qh
  expect_equal(length(generate_abc_smc_sample(data_ASE)), 2000)
})

test_that("llr output has 2,000 numeric outputs", { #1000 for qc, 1000 for qh
  expect_equal(length(generate_abc_smc_sample(data_BSE)), 2000)
})
