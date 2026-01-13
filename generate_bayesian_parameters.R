generate_bayesian_parameters <- function(modality = 'visual', fit_prior = FALSE, 
                                         bimodal_type = NA, diff_noise = FALSE, distributional_noise = FALSE) {
  #============================
  #Rebecca West
  #Updated: March 2023
  #This function randomly generates parameters for the bayesian model class.
  #Parameters include: boundaries, sigmas, attention weights (bimodal models).
  #Parameters are sampled from a normal distribution with a defined mean (defined in argument mus) 
  #and standard deviation (defined in argument sigmas). 
  #All parameters also have a lower and upper bound. 
  
  #Function continues to resample parameters from the define distribution until all parameters meet 
  #the requirements of that model
  library(reshape)
  
  #number of samples for the distributional noise parameter   
  samples = 100
  #================================
  # Set Distribution of Parameters To Be Sampled
  #================================
  #boundary parameters
  boundaries_mu = c(0.25,	0.2,	0.1,	-0.609,	-1.008,	-1.556,	-1.732)	
  boundaries_sd = rep(1.5, 7)
  boundaries_bounds = c(0, 15)
  
  if (modality == 'visual') {
    cat_one_sigma_mu = 0.3427148
    cat_two_sigma_mu = 1.370859
  } else if (modality == 'auditory') {
    cat_one_sigma_mu = 0.3429777
    cat_two_sigma_mu = 1.371911
  } else if (modality == 'bimodal') {
    cat_one_vis_sigma_mu = 0.3427148
    cat_two_vis_sigma_mu = 1.370859
    cat_one_aud_sigma_mu = 0.3429777
    cat_two_aud_sigma_mu = 1.371911
  }
  cat_sd = 0.5
  cat_bounds = c(0, 3)
  
  #function to convert from d space to measurement space for each boundary 
  source(paste0(getwd(), '/bayesian_boundary.R'))
  
  
  #set a timer because sometimes this will run on an infinite loop
  start_time = Sys.time()
  time_out = FALSE
  
  #============= Sample Sigma Parameters ============
  sigma_bounds = c(0, 3)
  
    if (diff_noise == FALSE) {
    #sigma parameters
    sigma_mu = c(1,	0.9,	0.8,	0.7)
    sigma_sd = rep(0.5, 4)
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
    vis_sigma_mu = c(1,	0.9,	0.8,	0.7)
    aud_sigma_mu = c(1,	0.9,	0.8,	0.7)
    sigma_sd = rep(0.5, 4)
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

  #============= Noise Distribution Parameter ============
  if (distributional_noise == TRUE) {
    sigma_sd_mu = 1.1
    repeat {
      sigma_sd <- rnorm(1, sigma_sd_mu, 0.3)
      if (sigma_sd > 1 & sigma_sd < 4) {break}
    }
  }

  ## =========== Sample Weight Parameters =============
  #if biased bimodal model, need to sample weights too 
  if (modality == 'bimodal') {
    #if we are using the biased bimodal model we need to sample weights 
    if (bimodal_type == 'biased') {
      weight_vis_mu = 0.5
      #sample weights 
      repeat {
        weight_vis <- rnorm(1, weight_vis_mu, 0.2)
        if (weight_vis > 0 & weight_vis < 1) {
          weight_aud = 1 - weight_vis
          break
        }
      }
    } else {
      weight_vis = 1
      weight_aud = 1
    }
  } 
  #================================
  # Sample Boundary & Category Parameters
  #================================
    boundary_draws <-  matrix(0, 4, 7)
    repeat {
      # sample boundaries
      boundaries <- (rnorm(length(boundaries_mu), 0, 1)*boundaries_sd)+boundaries_mu
      
      ## find good category parameters
      if (fit_prior == TRUE & (modality == 'visual' | modality == 'auditory')) {
      repeat { 
      cat_one_sigma <- (rnorm(length(cat_one_sigma_mu), 0, 1)*cat_sd)+cat_one_sigma_mu
      cat_two_sigma <- (rnorm(length(cat_two_sigma_mu), 0, 1)*cat_sd)+cat_two_sigma_mu
      values = c(cat_one_sigma, cat_two_sigma)
      if (all(values > cat_bounds[1]) & all(values < cat_bounds[2])) {
        break
      }
      }
      } else if (fit_prior == TRUE & modality == 'bimodal') {
        repeat {
          cat_one_vis_sigma <- (rnorm(length(cat_one_vis_sigma_mu), 0, 1)*cat_sd)+cat_one_vis_sigma_mu
          cat_two_vis_sigma <- (rnorm(length(cat_two_vis_sigma_mu), 0, 1)*cat_sd)+cat_two_vis_sigma_mu
          cat_one_aud_sigma <- (rnorm(length(cat_one_aud_sigma_mu), 0, 1)*cat_sd)+cat_one_aud_sigma_mu
          cat_two_aud_sigma <- (rnorm(length(cat_two_aud_sigma_mu), 0, 1)*cat_sd)+cat_two_aud_sigma_mu
          values = c(cat_one_vis_sigma, cat_two_vis_sigma, cat_one_aud_sigma, cat_two_aud_sigma)
          if(all(values > cat_bounds[1]) & all(values < cat_bounds[2])) {
            break
          }
        }
      } else if (fit_prior == FALSE & modality == 'visual' | modality == 'auditory') {
        cat_one_sigma <- cat_one_sigma_mu
        cat_two_sigma <- cat_two_sigma_mu
      } else if (fit_prior == FALSE & modality == 'bimodal') {
        cat_one_vis_sigma = 0.3427148
        cat_two_vis_sigma = 1.370859
        cat_one_aud_sigma = 0.3429777
        cat_two_aud_sigma = 1.371911
      }
      
        #get all possible boundary draws for each intensity level
        if (modality == 'bimodal') {
          if (diff_noise == FALSE) {
          vis_bounds <- t(sapply(sigmas, function(x) calculate_x((boundaries), x, cat_one_vis_sigma, cat_two_vis_sigma)))
          aud_bounds <- t(sapply(sigmas, function(x) calculate_x((boundaries), x, cat_one_aud_sigma, cat_two_aud_sigma)))
          } else if (diff_noise == TRUE) {
            vis_bounds <- t(sapply(vis_sigmas, function(x) calculate_x((boundaries), x, cat_one_vis_sigma, cat_two_vis_sigma)))
            aud_bounds <- t(sapply(aud_sigmas, function(x) calculate_x((boundaries), x, cat_one_aud_sigma, cat_two_aud_sigma)))
          }
          if (bimodal_type == 'biased') {  
           #calculate boundaries for each intensity level using distance measure 
            boundary_draws = sapply(1:ncol(vis_bounds), function(x) sqrt( (vis_bounds[,x]*vis_bounds[,x]) + 
                                                                            (aud_bounds[,x]*aud_bounds[,x]) ) )
          } else if (bimodal_type == 'max') {
            boundary_draws = rbind(vis_bounds, aud_bounds)
          }
        } else if (modality != 'bimodal') {
          if (distributional_noise == TRUE) {
            samples_per_trial = 100
            #returns probability values linearly spaced between 1% and 99% of length 
            #samples per trials, one for each value of sigma
            probability_sequence = rep(seq(0.01, 0.99, length.out = samples_per_trial), 
                                       length(sigmas))
            #returns the value of sigma for a given probability, using log normal distribution
            #mean = log of sigma value for each intensity level, sd = fixed across all sigma levels 
            sigma_list = qlnorm(probability_sequence, 
                                meanlog = log(rep(sigmas, each= samples_per_trial)),
                                sdlog = log(rep(sigma_sd, samples_per_trial*length(sigmas))))
            #calculate boundary parameters 
            boundary_draws <- t(sapply(sigma_list, function(x) calculate_x(boundaries, x, cat_one_sigma, cat_two_sigma)))
            } else {
              #calculate boundary paramters for other models 
            boundary_draws <- t(sapply(sigmas, function(x) calculate_x(boundaries, x, cat_one_sigma, cat_two_sigma)))
            }
          vis_bounds = aud_bounds = boundary_draws #so we can break the loop, if boundaries meet conditions 
        }
        
        #check that boundary draws meet conditions
        if ((all(vis_bounds > boundaries_bounds[1])) & #lower bound check
            (all(vis_bounds < boundaries_bounds[2])) & #upper bound check 
            (all(aud_bounds > boundaries_bounds[1])) & 
            (all(aud_bounds < boundaries_bounds[2])) & 
            all(boundary_draws < boundaries_bounds[2]) & 
            all(t(apply(boundary_draws, 1, function(x) sort(x))) == boundary_draws)) { #check that boundaries are monotonically increasing 
          break   #stop sampling if conditions have been met  
        }
      
      #if sampling is taking too long (which it shouldn't), stop the sampling
      run_time = Sys.time()
      if (difftime(run_time, start_time, units = "mins") > 10) {
        time_out = TRUE
        break
      }
    }

diff_bounds = diff(c(0, boundaries)) #need this for likelihood function

#============= Return Parameters ============
  generating_parameters <- c(diff_bounds, sigmas) 
  if (fit_prior == TRUE) {
    if (modality != 'bimodal') {
      generating_parameters = c(generating_parameters, cat_one_sigma, cat_two_sigma)
    } else if (modality == 'bimodal') {
      generating_parameters = c(generating_parameters, cat_one_vis_sigma, cat_two_vis_sigma,
                                cat_one_aud_sigma, cat_two_aud_sigma)
    }
  }
  if (bimodal_type == 'biased') {
    generating_parameters = c(generating_parameters, weight_vis)
  }
  if (distributional_noise == TRUE) {
    generating_parameters <- c(generating_parameters, sigma_sd)
  }

if (time_out == TRUE) {
  generating_parameters = rep(NA, length(generating_parameters))
}
return(generating_parameters)
}
