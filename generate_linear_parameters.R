generate_linear_parameters <- function(model, modality, bimodal_type = 'NA', diff_noise = FALSE, distributional_noise = FALSE) {
#============================
#Rebecca West 2022
#Last Update: Feb 2023 
#This function randomly generates parameters for the scaled evidence strength model class.
#Parameters include: ks + ms (boundaries), sigmas, exponent, attention weights (bimodal models).
#Parameters are sampled from a normal distribution with a defined mean (referred to with mu) 
#and standard deviation (referred to as sd). All parameters also have a lower and upper bound. 
  
#Function continues to resample parameters from the define distribution until all parameters meet 
#the requirements of that model

library(reshape2)
  
#number of samples for the distributional noise parameter   
samples = 100
#============================
#Function used to calculate boundary position 
  boundary <- function(k, m, s, exp) {
    b = k+(m*(s^exp))
    return(b)
  }

  source("multi_sd.R")

##============= Sample Sigma Parameters ===========
#Upper and lower bounds defined
#sigma_bounds = c(0.3, 5)
sigma_bounds = c(0.1, 3)

if (diff_noise == FALSE) {
#Define sigma parameters for models with 4 noise levels 
#define means
sigma_mu = c(1.23,	1.00,	0.81,	0.79) 

#define SDs 
#has to be smaller for the linear models otherwise it takes too long to sample 
if (model == 'lin') {
  sigma_sd = rep(0.2, length(sigma_mu))
} else {
  sigma_sd = rep(0.5, length(sigma_mu))
}
}

if (diff_noise == TRUE) {
  #Define sigma parameters for models with 8 noise levels 
  #define means
  vis_sigma_mu = c(1.23,	1.00,	0.81,	0.79) 
  aud_sigma_mu = c(1.23,	1.00,	0.81,	0.79) 
  
  #define SDs 
  #has to be smaller for the linear models otherwise it takes too long to sample 
  if (model == 'lin') {
    sigma_sd = rep(0.2, length(vis_sigma_mu))
  } else {
    sigma_sd = rep(0.5, length(vis_sigma_mu))
  }
}

#============= Noise Distribution Parameter ============
if (distributional_noise == TRUE) {
  sigma_sd_mu = 1.1
  repeat {
    sigma_sd <- rnorm(1, sigma_sd_mu, 0.2)
    if (sigma_sd > 1 & sigma_sd < 4) {break}
  }
}
#============= Weight Parameters ============
#if biased bimodal model, need to sample weights too 
  if (bimodal_type == 'biased') {
    weight_vis_mu = 0.5
    #sample weights 
     repeat {
       weight_vis <- rnorm(1, weight_vis_mu, 0.2)
     if (weight_vis > 0 & weight_vis < 1) {break}
       }
      weight_aud <- 1 - weight_vis
  }

#============= scaler Model Parameters ============
if (model == 'scaler' | model == 'offset') {
scaler_sd = 0.5
scaler_bounds = c(0.1, 3)
if (model == "scaler") {
  scaler_mu = 1.2
} else if (model == "offset") {
  scaler_mu = 0.5
}
repeat {
  scaler <- rnorm(1, scaler_mu, scaler_sd)
  if (scaler > scaler_bounds[1] & 
      scaler < scaler_bounds[2]) {break}
}
}

#============= Define Boundary Parameters ============
# linear Model 
if (model == 'lin') {
  #old starting parameters that are too constrained 
  #kboundaries_mu = c(0.26,	0.50,	0.92,	0.28,	-0.42,	-0.86,	-0.18)
  #mboundaries_mu = c(-0.19,	-0.36, -0.53,	0.73,	2.14,	3.17,	2.78)
  kboundaries_mu = c(0.37, 0.55, 0.66, 0.29, -0.42, -0.85, -0.17)
  mboundaries_mu = c(-0.04, -0.10, -0.12, 0.72, 2.13, 3.16, 2.78)
  exponent = 1 #fixed to 1 for linear model 
  kboundaries_sd = rep(0.5, 7)
  mboundaries_sd = rep(0.5, 7)
#Quadratic Model 
} else if (model == 'quad') {
  kboundaries_mu = c(0.35, 0.5, 0.6, 0.64, 0.62, 0.69, 1.18) #c(0.17, 0.34, 0.66, 0.64, 0.62, 0.69, 1.18)
  mboundaries_mu = c(-0.02, -0.05, -0.06, 0.36, 1.06, 1.25, 1.38) #c(-0.09, -0.18, -0.2, 0.36, 1.06, 1.57, 1.38)
  kboundaries_sd = rep(0.5, 7)
  mboundaries_sd = rep(0.5, 7)
  exponent = 2 #fit to 2 for quadratic model 
#Free-Exponent Model 
} else if (model == 'exp' | model == 'noise' | model == 'offset' | model == 'scaler' |
           model == 'common' | model == 'flexible') {
  kboundaries_mu = c(0.35, 0.5, 0.6, 0.64, 0.62, 0.69, 1.18) #c(0.17, 0.34, 0.66, 0.64, 0.62, 0.69, 1.18)
  mboundaries_mu = c(-0.02, -0.05, -0.06, 0.36, 1.06, 1.25, 1.38) #c(-0.09, -0.18, -0.2, 0.36, 1.06, 1.57, 1.38)
  kboundaries_sd = rep(0.2, 7) #0.2
  mboundaries_sd = rep(0.2, 7) #0.2
  
  #sample exponent parameter
  exponent_mu = 2
  exponent_sd = 1
  exponent_bounds = c(1, 4)
  repeat {
    exponent <- rnorm(1, exponent_mu, exponent_sd)
    if ((exponent > exponent_bounds[1]) &
        (exponent < exponent_bounds[2])) { 
      break
    }
  }
}
#boundaries_bounds = c(0, 15) #limits on boundary position (once calculated using k + m*sigma)
boundaries_bounds = c(0, 20) 

#============= Sample Boundary Parameters ============
#set a timer because sometimes this will run on an infinite loop 
#if the sampled sigma parameters suck 
start_time = Sys.time()
time_out = FALSE

#Do this differently for 4 noise levels because it is less involved
repeat {
  
  if (diff_noise == FALSE) {
    #sample the parameters
    repeat {
      # sampling sigma first
      sigmas <- (rnorm(length(sigma_mu), 0, 1)*sigma_sd)+sigma_mu
      
      #if sampling sigma - need them to be ordered consecutively
      if (all(sort(sigmas,  decreasing = TRUE) == sigmas) & #order check
          all(sigmas > sigma_bounds[1]) & #lower bound check
          all(sigmas < sigma_bounds[2])) { #upper bound check
        break
      }
    }
  } else if (diff_noise == TRUE) {
    #sample the parameters
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
  
  if (bimodal_type == 'biased') {
    if (diff_noise == FALSE) {
      vis_sigmas = aud_sigmas = sigmas
    }
    sigma_list = sapply(1:4, 
                        function(x) multi_sd(weight_vis, 
                                             vis_sigmas[x], 
                                             aud_sigmas[x]))
  } else if (model == "offset") {
    sigma_list = c(sigmas, scaler + sigmas)
  } else if (model == "scaler") {
    sigma_list = c(sigmas, scaler*sigmas)
  } else if (distributional_noise == TRUE) {
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
  } else {
    #for noise, unimodal & max models 
    sigma_list = sigmas
  }
  
  #sample k and m parameters
  kbounds <- (rnorm(length(kboundaries_mu), 0, 1)*kboundaries_sd)+kboundaries_mu
  mbounds <- (rnorm(length(mboundaries_mu), 0, 1)*mboundaries_sd)+mboundaries_mu
  
  boundaries = sapply(sigma_list, function(x) boundary(kbounds, mbounds, x, exponent))
  
  #check if boundaries meet conditions 
  if (all(boundaries > boundaries_bounds[1]) & #lower bound check
      all(boundaries < boundaries_bounds[2]) & # upper bound check
      all(apply(boundaries, 2, sort) == boundaries)) { #make sure boundaries are monotonically increasing
    break
  }
  
  #check timer 
  run_time = Sys.time()
  if (difftime(run_time, start_time, units = "mins") > 10) {
    time_out = TRUE
    break
  }
}

#============= Return Parameters ============
if (modality == 'visual' | modality == 'auditory' | (modality == 'bimodal' & 
    bimodal_type == 'max')) {
if (model == 'lin' | model == 'quad') {
generating_parameters <- c(kbounds, mbounds, sigmas)
} else if (model == 'exp') {
generating_parameters <- c(kbounds, mbounds, sigmas, exponent)
}
}

#this will always be an exponent model 
if (model == "offset" | model == "scaler") {
  generating_parameters <- c(kbounds, mbounds, sigmas, exponent, scaler)
} else if (model == "common" | model == "noise") {
  generating_parameters <- c(kbounds, mbounds, sigmas, exponent)
} else if (model == "flexible") {
  generating_parameters <- c(kbounds, mbounds, sigmas, exponent)
}

if (modality == 'bimodal' & bimodal_type == 'biased') {
  if (model == 'lin' | model == 'quad') {
    generating_parameters <- c(kbounds, mbounds, sigmas, weight_vis)
  } else if (model == 'exp') {
    generating_parameters <- c(kbounds, mbounds, sigmas, exponent, weight_vis)
  }
}

if (distributional_noise == TRUE) {
  generating_parameters <- c(generating_parameters, sigma_sd)
}

#if we couldn't find a suitable set of parameters 
if (time_out == TRUE) {
  generating_parameters = rep(NA, length(generating_parameters))
}

return(generating_parameters)
}