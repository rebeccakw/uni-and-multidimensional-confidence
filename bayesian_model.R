bayesian_model_function <- function(confidencedata, sub, reps = 100, starting_parameters, 
                                    modality = 'visual', fit_prior = FALSE, unimodal = TRUE,
                                    model, bimodal_type = 'integrated', diff_noise = FALSE,
                                    parameter_names = parameter_names, recovery = FALSE,
                                    distributional_noise = FALSE) {
  
  library(reshape2)
  source(paste0(getwd(), '/different_noise.R'))
  source(paste0(getwd(), '/multi_sd.R'))

  #=================================
  #confidencedata$stim_orientation = scale(confidencedata$stim_orientation)
  if (all(confidencedata$stim_orientation == 0)) {
    confidencedata$stim_orientation = confidencedata$stim_frequency #hacky way of dealing with auditory data 
  }
  
  if (distributional_noise == TRUE) {
    samples_per_trial = 100
    probability_sequence = rep(seq(0.01, 0.99, length.out = samples_per_trial), 
                               nrow(confidencedata))
  }
  #=================== Required Functions =================
  source(paste0(getwd(), '/bayesian_boundary.R'))
  #====================================  
  n_params = length(starting_parameters)
  n_intensities = length(unique(confidencedata$stim_reliability_level))
 
  #=================== Create Storage =================
  #create dataframe to store parameter estimates
  parameters <- data.frame(matrix(0, reps, n_params + 4))
  colnames(parameters) <- c(rep('parameter', n_params), "subject", 
                            "log_likelihood", "AIC", "BIC")
  
  if (length(sub) > 1) {
    parameters$subject <- 0
  } else {
    parameters$subject <- sub  
    confidencedata = confidencedata[confidencedata$subject_name == sub,]
  }
  
  #create dataframe to store likelihoods in each rep
  n_trials = nrow(confidencedata)
  b_high <- matrix(Inf, nrow(confidencedata), 1)
  b_low <- matrix(0, nrow(confidencedata), 1)
  
  #need to set an initial value for this to work
  log.likelihoods <- 0
  
  #get category distribution parameters if we aren't fitting them
  #these are in standardised units - based on our data - but represent mean/sd of 
  #the generating distributions 
  if (fit_prior == FALSE) {
    if (modality == 'visual') {
      cat_one_sigma = 0.3427148
      cat_two_sigma = 1.370859
    } else if (modality == 'auditory') {
      cat_one_sigma = 0.3429777
      cat_two_sigma = 1.371911
    } else if (modality == 'bimodal') {
      cat_one_vis_sigma = 0.3427148
      cat_two_vis_sigma = 1.370859
      cat_one_aud_sigma = 0.3429777
      cat_two_aud_sigma = 1.371911
    }
  }
  
  for (rep in 1:reps) {   
    
    bayesian_model <- function(par){
      
      ## ======== Get Relevant Parameter Values ==========
      #boundary parameters 
      boundaries = c(0, cumsum(par[1:7]), Inf)
      
      #get sigma parameters - index using parameter names 
      if (diff_noise == FALSE) {  
        sigs = par[8:11]
      } else if (diff_noise == TRUE) {
        vis_sigmas_index = sapply(parameter_names, function(x) grepl("vis_sigma", x))
        vis_sigs = par[vis_sigmas_index]
        aud_sigmas_index = sapply(parameter_names, function(x) grepl("aud_sigma", x))
        aud_sigs = par[aud_sigmas_index]
        sigs = rbind(aud_sigs, vis_sigs)
      }
      
      #get category sds if fitting the prior 
      if (fit_prior == TRUE) {
        if (unimodal == TRUE) {
          cat_one_sigma_index = sapply(parameter_names, function(x) grepl("cat1_sd", x))
          cat_two_sigma_index = sapply(parameter_names, function(x) grepl("cat2_sd", x))
          cat_one_sigma = par[cat_one_sigma_index]
          cat_two_sigma = par[cat_two_sigma_index]
        } else if (unimodal == FALSE) {
          vis_cat_one_sigma_index = sapply(parameter_names, function(x) grepl("vis_cat1_sd", x))
          vis_cat_two_sigma_index = sapply(parameter_names, function(x) grepl("vis_cat2_sd", x))
          aud_cat_one_sigma_index = sapply(parameter_names, function(x) grepl("aud_cat1_sd", x))
          aud_cat_two_sigma_index = sapply(parameter_names, function(x) grepl("aud_cat2_sd", x))
          cat_one_vis_sigma = par[vis_cat_one_sigma_index]
          cat_two_vis_sigma = par[vis_cat_two_sigma_index]
          cat_one_aud_sigma = par[aud_cat_one_sigma_index]
          cat_two_aud_sigma = par[aud_cat_two_sigma_index]
        }
      }
      
      #get weight parameter, if necessary
      if (bimodal_type == 'biased') {
        vis_weight_index = sapply(parameter_names, function(x) grepl("weight_vis", x))
        weight_vis = par[vis_weight_index]
        weight_aud = 1 - weight_vis
      } else if (bimodal_type == 'max') {
        weight_vis = 1
        weight_aud = 1
      }
      
      #get distributional noise parameter, if applicable 
      if (distributional_noise == TRUE) {
        sigma_sd_index = sapply(parameter_names, function(x) grepl("sig_sd", x))
        sigma_sd = par[sigma_sd_index]
      }
      
      ## ======== First Test of Parameter Values ==========
      # || is true if at least one of the conditions is True
      if (any(sigs <= 0) || #none of the sigma parameters can be 0 
          any(sigs > 3) || #upper limit on sigma parameters 
          any(cumsum(par[1:7]) < -10) ||
          (diff_noise == FALSE && (sum(diff(sigs) < 0) != (length(sigs) - 1))) || #sigmas should be decreasing
          (diff_noise == TRUE && any(t(apply(sigs, 1, function(x) (sum(diff(x) < 0) != (length(x)-1)))))) ||
          (bimodal_type == 'biased' && ((weight_vis + weight_aud) !=  1)) ||
          (bimodal_type == 'biased' && any(c(weight_vis, weight_aud) < 0)) ||
          (modality != 'bimodal' && any(c(cat_one_sigma, cat_two_sigma) <= 0)) ||
          (modality == 'bimodal' && any(c(cat_one_vis_sigma, cat_two_vis_sigma, 
                                          cat_one_aud_sigma, cat_two_aud_sigma) <= 0)) ||
          (modality == 'bimodal' && any(c(cat_one_vis_sigma, cat_two_vis_sigma,
                                      cat_one_aud_sigma, cat_two_aud_sigma) > 4)) ||
          (distributional_noise == TRUE && sigma_sd < 1)) {
        log.likelihoods <- Inf
      }
      
      ## ==================
      if (log.likelihoods < Inf) { 
        if (unimodal == TRUE) {  
          if (distributional_noise == TRUE) {
            #standard sigma repeated for each trial 
            sigma = rep(sigs[confidencedata$stim_reliability_level], each= samples_per_trial)
            
            #calculate boundaries for whole log distribution of sigma values 
            adjusted_sigma = qlnorm(probability_sequence, 
                                meanlog = log(sigma),
                                sdlog = log(rep(sigma_sd, samples_per_trial*nrow(confidencedata))))
            
            #calculate boundaries
            bs <- t(sapply(adjusted_sigma, function(x) c(0, calculate_x(cumsum(par[1:7]), x, cat_one_sigma, cat_two_sigma), Inf)))
            
            #need repeats for each trial 
            stim_distance = rep(confidencedata$stim_orientation, each = samples_per_trial)
            responses = rep(confidencedata$r, each = samples_per_trial)
            
            #normalise so that the trials will sum to 1
            densities = probability_sequence/sum(probability_sequence[1:samples_per_trial])
          } else {
          
          sigma = sigs[confidencedata$stim_reliability_level] #get baseline sigma for each reliability level
          bs <- t(sapply(sigma, function(x) c(0, calculate_x(cumsum(par[1:7]), x, cat_one_sigma, cat_two_sigma), Inf)))
          stim_distance = confidencedata$stim_orientation
          responses = confidencedata$r
          }
          
          #make sure lower boundary positions are not less than 0
          if (any(bs < 0) |
              any(t(apply(bs, 1, function(x) sort(x))) != bs)) { #order check
            log.likelihoods <- Inf
          }
          
          if (log.likelihoods < Inf) { 
          
          b_low <- bs[cbind(1:length(responses), responses)]
          b_high <- bs[cbind(1:length(responses), responses + 1)]
          }
        } else if (unimodal == FALSE) {
          
          if (bimodal_type == 'biased') {
            if (diff_noise == FALSE) {
              vis_sigs = aud_sigs = sigs
            }
            #use different noise function to get draws of stimulus orientations, freuquencies 
            #and the joint normalised density associated with each  - returns a list 
            stimulus_draws = different_noise(confidencedata$stim_orientation, confidencedata$stim_frequency,
                                             vis_sigs[confidencedata$stim_reliability_level], aud_sigs[confidencedata$stim_reliability_level])
            #vectorise draws to pass to pnorm() - each row of 100 would represent 1 trial
            orientations = unlist(stimulus_draws[[1]])
            frequencies = unlist(stimulus_draws[[2]])
            densities = as.vector(stimulus_draws[[3]])
            samples_per_trial = length(orientations)/nrow(confidencedata)
            
            #match up the trial data with the vectorised stimulus draws 
            max_index = rep(confidencedata$visual_mostevidence, each = samples_per_trial)
            responses = rep(confidencedata$r, each = samples_per_trial)
            reliability_level = rep(confidencedata$stim_reliability_level, each = samples_per_trial)
          } else if (bimodal_type == 'max') {
            #using the data in its original form 
            #if bimodal model is a 'max' model - we just ignore the other modality  
            orientations = confidencedata$stim_orientation
            frequencies = confidencedata$stim_frequency
            max_index = confidencedata$visual_mostevidence
            responses = confidencedata$r
            reliability_level = confidencedata$stim_reliability_level
          }
          ## ======= calculate boundaries ===========
          if (diff_noise == FALSE) {
          #calculate boundaries - transform d values to perceptual units 
          vis_bounds <- t(sapply(sigs, function(x) calculate_x((cumsum(par[1:7])), x, 
                                                                     cat_one_vis_sigma, cat_two_vis_sigma)))
          aud_bounds <- t(sapply(sigs, function(x) calculate_x((cumsum(par[1:7])), x, 
                                                                     cat_one_aud_sigma, cat_two_aud_sigma)))
          sigma = sigs[reliability_level]
          } else if (diff_noise == TRUE) {
            #transform d values to perceptual units 
            vis_bounds <- t(sapply(vis_sigs, function(x) calculate_x((cumsum(par[1:7])), x, 
                                                                           cat_one_vis_sigma, cat_two_vis_sigma)))
            aud_bounds <- t(sapply(aud_sigs, function(x) calculate_x((cumsum(par[1:7])), x, 
                                                                           cat_one_aud_sigma, cat_two_aud_sigma)))
            if (diff_noise == TRUE & bimodal_type == 'max') {
              #index sigma values for the attended modality: uses  visual_mostevidence to 
              #select the sigma values in the appropriate row and stim_reliability_level
              #to index the column 
              sigma = sigs[cbind((max_index + 1), reliability_level)]
            } else if (bimodal_type == 'biased') {
              #combine sigma values across modalities
              sigma = sapply(1:length(reliability_level), 
                             function(x) multi_sd(weight_vis, 
                                                  vis_sigs[(reliability_level[x])],
                                                  aud_sigs[(reliability_level[x])]))
            }
            }
          if (bimodal_type == 'biased') { 
            #convert boundaires im sensory space to a distance metric
          all_bounds = cbind(0, sqrt(((vis_bounds*vis_bounds) + (aud_bounds*aud_bounds))), Inf)
          #get stimulus distances
          #use both modalities
          #recalculate stimulus values as distance from 0 - the centre of the distributions
          stim_distance = sapply(1:length(orientations), function(x) 
            sqrt (
            (weight_vis*(orientations[x]*orientations[x])) +
            (weight_aud*(frequencies[x]*frequencies[x]))
            ))
          
          } else if (bimodal_type == 'max') {
            stim_distance = cbind(frequencies, orientations)[cbind(1:length(orientations),
                                                             (max_index + 1))]
            #has to be coded as distance from the centre of the category distributions 
            stim_distance = abs(stim_distance)

            all_bounds = cbind(0, rbind(aud_bounds, vis_bounds), Inf)

            #index trials where attending to auditory stimulus 
            max_index = (max_index + 1)
            reliability_level[max_index == 2] = 4 + reliability_level[max_index == 2]
          }
          
          #need to check that all boundaries meet the requirements 
          if (any(all_bounds[, 2:8] < 0) |
              any(t(apply(all_bounds, 1, function(x) sort(x))) != all_bounds)) { #order check
            log.likelihoods <- Inf
          }
          
          if (log.likelihoods < Inf) {
            b_high = all_bounds[cbind(reliability_level, responses + 1)]
            b_low =  all_bounds[cbind(reliability_level, responses)]
          }
        }
        
        if (log.likelihoods < Inf) {
            #get likelihood for upper positive boundary 
            likelihoods = (((pnorm(b_high, mean = stim_distance, 
                                   sd = sigma)) - 
                              #get likelihood for lower positive boundary 
                              (pnorm(b_low, mean = stim_distance, 
                                     sd = sigma))) + 
                             #get likelihood for upper negative boundary
                             ((pnorm(-b_low, mean = stim_distance, 
                                     sd = sigma)) -
                                #get likelihood for lower positive boundary 
                                (pnorm(-b_high, mean = stim_distance, 
                                       sd = sigma))))
          
            likelihoods[likelihoods == 0] <- min(likelihoods[likelihoods>0])
            
            if ((bimodal_type == 'biased') | (distributional_noise == TRUE)) {
              #multiply likelihoods by density of that draw and then shape
              #to seperate out trials. Each row is a trial and each column is 
              #a likelihood for a draw in that trial 
              normalised_likelihoods = matrix(densities*likelihoods,
                                              nrow = samples_per_trial, ncol = nrow(confidencedata))
              #sum each row to get a likelihood for each trial 
              summed_likelihoods = colSums(normalised_likelihoods)
              log.likelihoods <- -sum(log(summed_likelihoods))
            } else {
              log.likelihoods <- -sum(log(likelihoods))
            }
}
      }
      
      return(log.likelihoods)
  }
    
    # =================== Optimisation =================
    
    #if first rep - use starting_parameters
    if (rep == 1) { 
      parameter.fits <- optim(par = starting_parameters,
                              fn = bayesian_model)
    } 
    
    # if rep is greater than one - use parameter values from previous iteration
    else {
      parameter.fits <- optim(par = as.double(parameters[(rep - 1), 1:n_params]),
                              fn = bayesian_model) 
    }
    
    
    parameters[rep, 1:n_params] <- parameter.fits$par
    parameters$log_likelihood[rep] <- parameter.fits$value
    
    #calculate and save BIC & AIC
    bic <- (-2*(-1*parameter.fits$value)) + length(parameter.fits$par) * log(nrow(confidencedata))
    aic <- (-2*(-1*parameter.fits$value)) + (2*length(parameter.fits$par))
    parameters$AIC[rep] <- aic
    parameters$BIC[rep] <- bic
    
    print(rep) 
    
    rep = rep + 1 
    
  }
  
  best_estimate = which.min(parameters$log_likelihood)
  parameters <- data.frame(parameters[best_estimate,])
  
  if (recovery == TRUE) {
    parameters = cbind(parameters, overall_simulation_set = unique(confidencedata$overall_simulation_set),
                       simulated_model = unique(confidencedata$simulated_model),
                       simulated_bimodal_type = unique(confidencedata$simulated_bimodal_type),
                       simulated_diff_noise = unique(confidencedata$simulated_diff_noise))
  }
  return(parameters)
  
}


