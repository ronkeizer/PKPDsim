library(PKPDsim)
library(testit)
Sys.setenv("R_TESTS" = "")

parameters <- list(KA = 0.5, CL = 5, V = 50)
covs <- list(WT = new_covariate(50))
reg_abs <- new_regimen(amt = 100, n = 1, interval = 12, type = "bolus")
reg_perkg <- new_regimen(amt = 2, n = 1, interval = 12, type = "bolus", per_unit = "WT")
mod1 <- new_ode_model(code = "
  CLi = CL * pow(WT/70, 0.75)
  dAdt[1] = -KA * A[1]
  dAdt[2] = KA*A[1] - (CLi/V)*A[2]
", dose = list(cmt = 1, bioav = 1), covariates = covs, parameters = parameters, cpp_show_code = T)

mod2 <- new_ode_model(code = "
  CLi = CL * pow(WT/70, 0.75)
  dAdt[1] = -KA * A[1]
  dAdt[2] = KA*A[1] - (CLi/V)*A[2]
", dose = list(cmt = 1, bioav = "WT"), covariates = covs, parameters = parameters)

test <- sim_ode(mod1, parameters = parameters,
                regimen = reg_perkg, return_design = TRUE,
                covariates = covs50, only_obs=TRUE)$y

y1 <- sim_ode(mod1, parameters = parameters, regimen = reg_perkg, covariates = covs50, only_obs=TRUE)$y
y2 <- sim_ode(mod2, parameters = parameters, regimen = reg_abs, covariates = covs50, only_obs=TRUE)$y
y3 <- sim_ode(mod3, parameters = parameters, regimen = reg_perkg, covariates = covs50, only_obs=TRUE)$y

assert("per kg dosing == absolute dosing", all(y1 == y2))
assert("per kg dosing == dosing using bioavailability", all(y1 == y2))
