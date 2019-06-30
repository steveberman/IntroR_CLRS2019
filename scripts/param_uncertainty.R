library(ggplot2)
library(dplyr)

#--------------------------
# gamma_params will take a mean and CV and 
# return appropriate parameters for a gamma
# distribution
gamma_params <- function(Mean, CV){
  
  if (missing(Mean)) stop("No mean was specified")
  if (missing(CV)) stop("No CV was specified")
  
  if (!is.numeric(Mean)) stop("Mean must be numeric")
  if (!is.numeric(CV)) stop("CV must be numeric")
  
  if (CV <= 0) stop("CV is strictly positive")
  
  beta = 1 / (Mean * CV^2)
  alpha = 1 / CV^2
  
  list(alpha = alpha, beta = beta)
  
}

#-------------------------
# We think that the frequency is 1e3, but we're not 
# sure. We'll assume a standard deviation of 20% of the 
# mean
freq_mean <- 1e3
freq_cv <- 0.2
freq_parms <- gamma_params(freq_mean, freq_cv)

#-----------------------------------
# This will generate 5 thousand simulations
# of our frequency parameter. Note tha the sample
# mean is pretty close to our assumed mean.
set.seed(1234)
sim_freq <- 5e3
tbl_freq <- data.frame(
    sim = seq_len(sim_freq)
  , sim_lambda = rgamma(sim_freq, shape = freq_parms$alpha, scale = freq_parms$beta)
)
tbl_freq$frequency <- rpois(sim_freq, tbl_freq$sim_lambda)
summary(tbl_freq$frequency)

#--------------------------------------
# Now we'd like to model severity for each
# frequency
sev_mean <- 10e3
sev_cv <- 0.9
sev_params <- gamma_params(sev_mean, sev_cv)
severity <- lapply(tbl_freq$frequency, rgamma, shape = sev_params$alpha, scale = sev_params$beta)

#-----------------------------------
# We'd like a data frame for each random
# frequency. The data frame will We can do this within a function