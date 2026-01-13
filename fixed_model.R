fixed_model_function <- function(confidencedata, sub, reps = 50, starting_parameters, 
                                 unimodal = TRUE, model, bimodal_type,
                                 diff_noise, parameter_names, recovery = FALSE) {
  source(paste0(getwd(), '/different_noise.R'))
  source(paste0(getwd(), '/multi_sd.R'))
  
  #============= Standardise Data ======================
  #confidencedata$stim_orientation <- scale(confidencedata$stim_orientation)
  #confidencedata$stim_frequency <- scale(confidencedata$stim_frequency)
  if (all(confidencedata$stim_orientation == 0)) {
    confidencedata$stim_orientation = confidencedata$stim_frequency
  }
  #=================== Set Some Inital Values =================
  n_params = length(starting_parameters)
  n_intensities = length(unique(confidencedata$stim_reliability_level))
  
  #=================== Create Storage =================
  #create dataframe to store parameter estimates
  parameters <- data.frame(matrix(0, reps, n_params + 4))
  colnames(parameters) <- c(rep('parameter', n_params), 
                            "subject", "log_likelihood", "AIC", "BIC")
  
  #for fitting group model
  if (length(sub) > 1) {
    parameters$subject <- 0
  } else {
  #filter for individual subject if fitting individual
    parameters$subject <- sub 
    confidencedata <- confidencedata[confidencedata$subject_name == sub,] 
  }
  
  #create dataframe to store likelihoods in each rep
  likelihoods <- matrix(0, nrow(confidencedata), 1)
  
  #need to set an initial value for this to work
  log.likelihoods <- 0
  
  #=================== Run Iterations of Model Fitting =================
  for (rep in 1:reps) {   
    
    #likelihood function 
    fixed_model <- function(par) {
      
      #get boundaries - always will be first 4 parameters
      boundaries = c(0, par[1:7], Inf)
      
      #get sigma parameters - index using parameter names 
      if (diff_noise == FALSE) {  
      sigmas = par[8:11]
      } else if (diff_noise == TRUE) {
        vis_sigmas_index = sapply(parameter_names, function(x) grepl("vis_sigma", x))
        vis_sigmas = par[vis_sigmas_index]
        aud_sigmas_index = sapply(parameter_names, function(x) grepl("aud_sigma", x))
        aud_sigmas = par[aud_sigmas_index]
        sigmas = rbind(aud_sigmas, vis_sigmas)
      }
      #get weight parameters if applicable 
        if (bimodal_type == 'biased') {
          vis_weight_index = sapply(parameter_names, function(x) grepl("weight_vis", x))
          weight_vis = par[vis_weight_index]
          weight_aud = 1 - weight_vis
        }
      
      #test if any of the parameters are undesirable values
      if (any(boundaries[2:8] <= 0) || #none of the boundaries can be less than 0 
          any(boundaries[2:8] > 8) ||
          any(sigmas > 3) ||
          any(sigmas <= 0) || #none of the sigma parameters can be less than 0 
          (bimodal_type == 'biased' && ((weight_vis + weight_aud) !=  1)) ||
          (bimodal_type == 'biased' && any(c(weight_vis, weight_aud) < 0)) ||
          (sum(diff(boundaries) > 0) != (length(boundaries) - 1)) || #make sure boundaries are increasing
          (diff_noise == FALSE && (sum(diff(sigmas) < 0) != (length(sigmas) - 1))) || #sigmas should be decreasing
          (diff_noise == TRUE && any(t(apply(sigmas, 1, function(x) (sum(diff(x) < 0) != (length(x)-1))))))) {
        log.likelihoods <- Inf
      }
      
      if (log.likelihoods < Inf) {
        
        if (diff_noise == FALSE & bimodal_type != 'biased') {
          sigma = sigmas[confidencedata$stim_reliability_level] #get baseline sigma for each reliability level
        } else if (diff_noise == TRUE & bimodal_type == 'max') {
          #index sigma values for the attended modality: uses  visual_mostevidence to 
          #select the sigma values in the appropriate row and stim_reliability_level
          #to index the column 
          sigma = sigmas[cbind((confidencedata$visual_mostevidence + 1), confidencedata$stim_reliability_level)]
        } else if (bimodal_type == 'biased') {
          if (diff_noise == FALSE) {
          vis_sigmas = aud_sigmas = sigmas
           sigma = sigmas[confidencedata$stim_reliability_level]
          } else {
          #calculate boundary parameters for biased model 
          sigma = sapply(1:nrow(confidencedata), 
                        function(x) multi_sd(weight_vis,
                                             vis_sigmas[(confidencedata$stim_reliability_level[x])],
                                             aud_sigmas[(confidencedata$stim_reliability_level[x])]))
          }
        }
        
        if (unimodal == TRUE) {
          #rename frequency or orientation values if fitting unimodal data 
       stim_distance = confidencedata$stim_orientation
       responses = confidencedata$r
        } else if (unimodal == FALSE) {
          
          if (bimodal_type == 'biased') {
            #use different noise function to get draws of stimulus orientations, freuquencies 
            #and the joint normalised density associated with each  - returns a list 
            stimulus_draws = different_noise(confidencedata$stim_orientation, confidencedata$stim_frequency,
            vis_sigmas[confidencedata$stim_reliability_level], aud_sigmas[confidencedata$stim_reliability_level])
            #vectorise draws to pass to pnorm() - each row of 100 would represent 1 trial
            orientations = unlist(stimulus_draws[[1]])
            frequencies = unlist(stimulus_draws[[2]])
            densities = as.vector(stimulus_draws[[3]])
            samples_per_trial = length(orientations)/nrow(confidencedata)
            
            #match up the trial data with the vectorised stimulus draws 
            #i.e. repeat the relevant value on each trial 400 times for each draw
            max_index = rep(confidencedata$visual_mostevidence, each = samples_per_trial)
            responses = rep(confidencedata$r, each = samples_per_trial)
            sigma = rep(sigma, each = samples_per_trial)
            
            stim_distance = sapply(1:length(orientations), function(x) 
              sqrt (
                (weight_vis*(orientations[x]*orientations[x])) +
                  (weight_aud*(frequencies[x]*frequencies[x]))
              ))
            
          } else if (bimodal_type == 'max') {
            responses = confidencedata$r
            
            #get the stimulus value for the most diagnostic modality 
            stim_distance = cbind(confidencedata$stim_frequency, confidencedata$stim_orientation)[cbind(1:length(confidencedata$stim_orientation),
                                                                   (confidencedata$visual_mostevidence + 1))]
            #has to be coded as distance from the centre of the category distributions 
            stim_distance = abs(stim_distance)
          }
        }
        
        likelihoods = ((pnorm((boundaries[(responses + 1)]), mean = stim_distance, 
                              sd = sigma)) - 
                         #get likelihood for lower positive boundary 
                         (pnorm((boundaries[responses]), mean = stim_distance, 
                                sd = sigma))) + 
          #get likelihood for upper negative boundary
          ((pnorm(-(boundaries[responses]), mean = stim_distance, 
                  sd = sigma)) -
             #get likelihood for lower positive boundary 
             (pnorm(-(boundaries[(responses + 1)]), mean = stim_distance, 
                    sd = sigma)))
        
        #can't take the log of a negative value 
        likelihoods[likelihoods <= 0] = min(likelihoods[likelihoods > 0])
        
        if (bimodal_type == 'biased') {
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
      
      return(log.likelihoods)
    }
    
    # =================== Optimisation Stuff =================
    if (rep == 1) { 
      parameter.fits <- optim(par = starting_parameters,
                             fn = fixed_model)
    } 
    
    # if rep is greater than one - use parameter values from previous iteration
    else {
      parameter.fits <- optim(par = as.double(parameters[(rep - 1), 1:n_params]),
                              fn = fixed_model) 
    }
    
    
    #save parameters and log likelihood for fit
    parameters[rep, 1:n_params] <- parameter.fits$par
    parameters$log_likelihood[rep] <- parameter.fits$value
    
    #calculate and save model comparison metrics (AIC and BIC)
    bic <- (-2*(-1*parameter.fits$value)) + length(parameter.fits$par) * log(nrow(confidencedata))
    aic <- (-2*(-1*parameter.fits$value)) + (2*length(parameter.fits$par))
    parameters$AIC[rep] <- aic
    parameters$BIC[rep] <- bic
    
    rep = rep + 1 
    
    print(rep)
  }
  
  #save parameters from the best fit
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
