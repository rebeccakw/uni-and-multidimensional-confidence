generate_fixed_parameters <- function(bimodal_type, diff_noise) {
  #============================
  #Rebecca West 2022
  #Last Update: Feb 2023 
  #This function randomly generates parameters for the unscaled evidence strength model class.
  #Parameters include: boundaries, sigmas, attention weights (bimodal models).
  #Parameters are sampled from a normal distribution with a defined mean (referred to with mu) 
  #and standard deviation (referred to as sd). All parameters also have a lower and upper bound. 
  
  #Function continues to resample parameters from the define distribution until all parameters meet 
  #the requirements of that model

#============= Sample Sigma Parameters ============
sigma_bounds = c(0.3, 3)

if (diff_noise == FALSE) {
  #sigma parameters
  sigma_mu = c(1.37, 1.03, 0.69, 0.34)
  sigma_sd = rep(1, 4)
  repeat {
    # sampling sigma first
    sigmas <- (rnorm(length(sigma_mu), 0, 1)*sigma_sd)+sigma_mu
    
    #if sampling sigma - need them to be ordered consecutively
    if (all(sort(sigmas, decreasing = TRUE) == sigmas) & #order check
        all(sigmas > sigma_bounds[1]) & #lower bound check
        all(sigmas < sigma_bounds[2])) { #upper bound check
      break
    }
  }
}

if (diff_noise == TRUE) {
  #sigma parameters
  vis_sigma_mu = c(1.37, 1.03, 0.69, 0.34)
  aud_sigma_mu = c(1.37, 1.03, 0.69, 0.34)
  sigma_sd = rep(1, 4)
  repeat {
    # sampling sigma first
    vis_sigmas <- (rnorm(length(vis_sigma_mu), 0, 1)*sigma_sd)+vis_sigma_mu
    aud_sigmas <- (rnorm(length(aud_sigma_mu), 0, 1)*sigma_sd)+aud_sigma_mu
    sigmas = c(vis_sigmas, aud_sigmas)
    
    #if sampling sigma - need them to be ordered consecutively
    if (all(sort(vis_sigmas,  decreasing = TRUE) == vis_sigmas) & #order check for visual
        all(sort(aud_sigmas,  decreasing = TRUE) == aud_sigmas) & #order check for auditory
        all(sigmas > sigma_bounds[1]) & #lower bound check
        all(sigmas < sigma_bounds[2])) { #upper bound check
      break
    }
  }
}

#============= Sample Boundary Parameters ============
#boundary parameters
boundaries_mu = c(0.11, 0.23, 0.54, 1.32, 1.70, 2.34, 2.71)
boundaries_sd = rep(1, 7)
boundaries_bounds = c(0, 8) #limits on boundary position

#set a timer because sometimes this will run on an infinite loop
start_time = Sys.time()
time_out = FALSE

repeat {    
  #if sigmas are consecutive - try to find some boundary parameters
  #sample some randomly and see if they meet conditions 
  bounds <- (rnorm(length(boundaries_mu), 0, 1)*boundaries_sd)+boundaries_mu
  
  run_time = Sys.time()
  if (all(bounds > boundaries_bounds[1]) & #lower bound check
      all(bounds < boundaries_bounds[2]) & #upper bound check
      all(sort(bounds) == bounds)) {
    break
  }
  
  if (difftime(run_time, start_time, units = "mins") > 30) {
    time_out = TRUE
    break
  }
}

#============= Sample Weight Parameters ============
if (bimodal_type == 'biased') {
    weight_vis_mu = 0.5
    #sample weight
    repeat {  
    weight_vis <- rnorm(1, weight_vis_mu, 0.2)
    if (weight_vis > 0 & weight_vis < 1) {
      break
    }
    }
      weight_aud <- 1-weight_vis
    }
#============= Return Parameters ============
if (bimodal_type != 'biased') {
  generating_parameters = c(bounds, sigmas)
} else if (bimodal_type == 'biased') {
  generating_parameters = c(bounds, sigmas, weight_vis)
} 
if (time_out == TRUE) {
  generating_parametrs = rep(NA, length(generating_parameters))
}
return(generating_parameters)
}