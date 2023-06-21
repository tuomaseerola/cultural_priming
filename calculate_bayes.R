calculate_bayes <- function(data = NULL) {
  set.seed(150)
  library(brm)
  library(brms)
  library(rstan)
  library(tidybayes)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  RTData_sample <- data
  RTData_sample$CongruenceN <- 1
  RTData_sample$CongruenceN[RTData_sample$Congruence == 'Congruent'] <- -1
  
  # This is just main effects
  # b1 = brms::brm(RT ~ CongruenceN + (1|Id),
  #                 data = RTData_sample,
  #                 family = 'shifted_lognormal',
  #                 iter = 4000, chains = 4, thin = 1,
  #                 prior = prior(student_t(1, 0.01, 0.02), class = b, coef = CongruenceN),
  #                 #prior = prior(student_t(1, 0.005, 0.01), class = b, coef = CongruenceN),
  #                 seed = 1)
  # b1
  
  b1 = brms::brm(
    RT ~ CongruenceN + (1 + CongruenceN |
                          Id) + (1 | RandTransp) + (1 | Word),
    data = RTData_sample,
    family = 'shifted_lognormal',
    iter = 4000, # was 4000
    chains = 4,
    thin = 1,
    prior = prior(student_t(1, 0.00, 0.71), class = b, coef = CongruenceN),
    #prior = prior(student_t(1, 0.005, 0.01), class = b, coef = CongruenceN),
    seed = 1,
    sample_prior=TRUE # added in June 2023
#    save_pars = save_pars(all = TRUE) # added in in June 2023
  )
  
  return <- b1
}