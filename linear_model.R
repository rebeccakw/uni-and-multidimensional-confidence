## FUNCTIONS FOR BAYESIAN MODEL 
linear_model_function <- function(confidencedata, sub, reps = 50, starting_parameters, 
                                  unimodal = TRUE, model, bimodal_type = 'integrated', diff_noise = FALSE,
                                  parameter_names = parameter_names, recovery = FALSE, distributional_noise = FALSE) {
  
  library(reshape2)
  source(paste0(getwd(), '/different_noise.R'))
  source(paste0(getwd(), '/multi_sd.R'))

#===================================
#confidencedata$stim_orientation <- scale(confidencedata$stim_orientation)
  if (all(confidencedata$stim_orientation == 0)) {
    confidencedata$stim_orientation = confidencedata$stim_frequency
  }
  
  if (distributional_noise == TRUE) {
  samples_per_trial = 100
  probability_sequence = rep(seq(0.01, 0.99, length.out = samples_per_trial), 
                             nrow(confidencedata))
  }
#=================== Set Inital Values =================
n_params = length(starting_parameters)
n_intensities = length(unique(confidencedata$stim_reliability_level))
 
#define function to calculate boundary positions  
  boundary <- function(k, m, s, exp) {
    b = k+(m*(s^exp))
    return(b)
  }
  #=================== Create Storage =================
  #create dataframe to store parameter estimates
  parameters <- data.frame(matrix(0, reps, n_params + 4))
  colnames(parameters) <- c(rep('parameter', n_params), "subject", 
                            "log_likelihood", "AIC", "BIC")
  
  #for fitting group model
  if (length(sub) > 1) {
    parameters$subject <- 0
  } else {
    parameters$subject <- sub 
    confidencedata <- confidencedata[confidencedata$subject_name == sub,] 
  }
  
  #create dataframe to store likelihoods in each rep
  likelihoods <- matrix(0, nrow(confidencedata), 1)
  
  #need to set an initial value for this to work
  log.likelihoods <- 0
  
  for (rep in 1:reps) {   
    
    #likelihood function 
    linear_model <- function(par) {
      
      #boundary parameterse 
      k_params = c(0, par[1:7], Inf)
      m_params = c(0, par[8:14], Inf)
      
      #get sigma parameters - index using parameter names 
      if (diff_noise == FALSE) {  
        sigmas = par[15:18]
        vis_sigmas = aud_sigmas = sigmas
      } else if (diff_noise == TRUE) {
        vis_sigmas_index = sapply(parameter_names, function(x) grepl("vis_sigma", x))
        vis_sigmas = par[vis_sigmas_index]
        aud_sigmas_index = sapply(parameter_names, function(x) grepl("aud_sigma", x))
        aud_sigmas = par[aud_sigmas_index]
        sigmas = rbind(aud_sigmas, vis_sigmas)
      }
      
      #get exponent 
      if (model == 'lin') {
        exp = 1
      } else if (model == 'quad') {
        exp = 2
      } else if (model == 'exp' | model == 'offset' | model == 'scaler' |
                 model == 'common' | model == 'noise' | model == 'flexible') {
        exp_index = sapply(parameter_names, function(x) grepl("exp", x))
        exp = par[exp_index]
      }
    
      #get weight parameters, if applicable 
      if (bimodal_type == 'biased') {
        vis_weight_index = sapply(parameter_names, function(x) grepl("weight_vis", x))
        weight_vis = par[vis_weight_index]
        weight_aud = 1- weight_vis
      }
      #get distributional noise parameter, if applicable 
      if (distributional_noise == TRUE) {
        sigma_sd_index = sapply(parameter_names, function(x) grepl("sig_sd", x))
        sigma_sd = par[sigma_sd_index]
      }
      
      # if (model == 'offset' | model == "scaler") {
      #   scaler_index = sapply(parameter_names, function(x) grepl("scaler", x))
      #   scaler = par[scaler_index]
      #   if (model == 'offset') {
      #     sigmas = rbind(sigmas, scaler+sigmas)
      #   } else if (model == "scaler") {
      #     sigmas = rbind(sigmas, scaler*sigmas)
      #   }
      # }
      
      #calculate boundary parameters according to different models to check they are fine 
      if (bimodal_type == 'biased') {
      #calculate boundary parameters for biased model 
        sigma_list = sapply(1:nrow(confidencedata), 
                            function(x) multi_sd(weight_vis,
                                                 vis_sigmas[(confidencedata$stim_reliability_level[x])],
                                                 aud_sigmas[(confidencedata$stim_reliability_level[x])]))
        #calculate boundary parameters based on computed values of sigma  
        bs <- t(sapply(sigma_list, function(x) 
          boundary(k_params, m_params, x, exp)))
      } else if (distributional_noise == TRUE) {
        sigma_list = qlnorm(probability_sequence, 
               meanlog = log(rep(sigmas[confidencedata$stim_reliability_level], each= samples_per_trial)),
               sdlog = log(rep(sigma_sd, samples_per_trial*nrow(confidencedata))))
        bs <- t(sapply(sigma_list, function(x) 
          boundary(k_params, m_params, x, exp)))
       } else {
        #calculate boundary parameters for other models 
        #sigs will either contain both the visual and auditory sigmas or shared sigmas 
        bs <- t(sapply(sigmas, function(x) 
          boundary(k_params, m_params, x, exp)))
      }
      
      #make sure the parameters meet the conditions 
      if (any(bs[,2:8] <= 0) || #lower limit on boundaries 
          any(bs[,2:8] > 30) || #upper limit on boundaries
          any(sigmas > 3) || #upper limit on sigmas
          exp < 1 || #lower limit on exponent
          exp > 5 || #upper limit on exponent 
          any(sigmas <= 0) || #lower limit on sigmas 
          (bimodal_type == 'biased' && ((weight_vis + weight_aud) !=  1)) ||
          (bimodal_type == 'biased' && any(c(weight_vis, weight_aud) < 0)) ||
          any(t(apply(bs, 1, function(x) (sum(diff(x) > 0) != (length(x) - 1))))) || #make sure boundaries are increasing
          ((diff_noise == FALSE & model != "offset" & model != "scaler") && (sum(diff(sigmas) < 0) != (length(sigmas) - 1))) || #sigmas should be decreasing
          ((diff_noise == TRUE | model == "offset" | model == "scaler") && any(t(apply(sigmas, 1, function(x) (sum(diff(x) < 0) != (length(x)-1)))))) ||
          #((model == 'offset' | model == 'scaler') && scaler < 0) |
          (distributional_noise == TRUE && sigma_sd < 1)) {
        log.likelihoods <- Inf
      }
      
      if (log.likelihoods < Inf) {
        
        #get sigma parameters for every trial
        if (diff_noise == TRUE & bimodal_type == 'max') {
          #index sigma values for the attended modality: uses  visual_mostevidence to 
          #select the sigma values in the appropriate row and stim_reliability_level
          #to index the column 
          sigma = sigmas[cbind((confidencedata$visual_mostevidence + 1), confidencedata$stim_reliability_level)]
        } else if (bimodal_type == 'biased' & diff_noise == TRUE | distributional_noise == TRUE) {
          #calculated sigma values for the biased model earlier 
          sigma = sigma_list
        } else if (model == "noise") {
          sigma = c(vis_sigmas[confidencedata$stim_reliability_level[confidencedata$visual_modality == 1]], 
                    aud_sigmas[confidencedata$stim_reliability_level[confidencedata$visual_modality == 0]])
        # } else if (model == "offset") {
        #   sigma = c(sigmas[confidencedata$stim_reliability_level[confidencedata$visual_modality == 1]], 
        #             scaler+(sigmas[confidencedata$stim_reliability_level[confidencedata$visual_modality == 0]]))
        # } else if (model == "scaler") {
        #   sigma = c(sigmas[confidencedata$stim_reliability_level[confidencedata$visual_modality == 1]], 
        #             scaler*(sigmas[confidencedata$stim_reliability_level[confidencedata$visual_modality == 0]]))
        } else if (model == "common") {
          sigma = c(sigmas[confidencedata$stim_reliability_level])
        } else {
          sigma = sigmas[confidencedata$stim_reliability_level] #get baseline sigma for each reliability level
        }

        if (unimodal == TRUE | model == 'flexible') {
          if (distributional_noise == TRUE) {
            stim_distance = rep(confidencedata$stim_orientation, each = samples_per_trial)
            responses = rep(confidencedata$r, each = samples_per_trial)
            densities = probability_sequence/sum(probability_sequence[1:samples_per_trial])
          } else {
          stim_distance = confidencedata$stim_orientation
          responses = confidencedata$r
          }
          } else if (unimodal == "cross-modal" && model != 'flexible') {
            #for all cross-modal models, have visual data first and then auditory data 
            stim_distance = c(confidencedata$stim_orientation[confidencedata$visual_modality == 1], 
                              confidencedata$stim_orientation[confidencedata$visual_modality == 0]) 
            responses = c(confidencedata$r[confidencedata$visual_modality == 1], 
                          confidencedata$r[confidencedata$visual_modality == 0])
            } else if (unimodal == FALSE) {
            if (bimodal_type == 'biased') {
              if (diff_noise == FALSE) {
                vis_sigmas = aud_sigmas = sigmas 
              }
              #use different noise function to get draws of stimulus orientations, frequencies 
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
              
              
              #recalculate stimulus values as distance from 0 - the centre of the distributions
              stim_distance = sapply(1:length(orientations), function(x) 
                sqrt (
                  (weight_vis*(orientations[x]*orientations[x])) +
                    (weight_aud*(frequencies[x]*frequencies[x]))
                ))
              
            } else {
              #Max models 
              responses = confidencedata$r
              
              #get the stimulus value for the most diagnostic modality 
              stim_distance = cbind(confidencedata$stim_frequency, confidencedata$stim_orientation)[cbind(1:length(confidencedata$stim_orientation),
                                                                     (confidencedata$visual_mostevidence + 1))]
              #has to be coded as distance from the centre of the category distributions 
              stim_distance = abs(stim_distance)
            }
            }
        
        #get upper and lower boundaries 
        b_high = boundary(k_params[(responses + 1)], m_params[(responses + 1)], sigma, exp)
        b_low = boundary(k_params[(responses)], m_params[(responses)], sigma, exp)

        #get likelihood for upper postive boundary
        likelihoods = ((pnorm(b_high, mean = stim_distance, 
                              sd = sigma)) - 
                         #get likelihood for lower positive boundary 
                         (pnorm(b_low, mean = stim_distance, 
                                sd = sigma))) + 
          #get likelihood for upper negative boundary
          ((pnorm(-(b_low), mean = stim_distance, 
                  sd = sigma)) -
             #get likelihood for lower positive boundary 
             (pnorm(-(b_high), mean = stim_distance, 
                    sd = sigma)))
        
        likelihoods[likelihoods <= 0] <- min(likelihoods[likelihoods > 0])
        
        if (bimodal_type == 'biased' | distributional_noise == TRUE) {
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
    # =================== Optimisation =================
    
    #if first rep - use starting_parameters
    if (rep == 1) { 
      parameter.fits <- optim(par = starting_parameters,
                              fn = linear_model)
    } 
    
    # if rep is greater than one - use parameter values from previous iteration
    else {
      parameter.fits <- optim(par = as.double(parameters[(rep - 1), 1:n_params]),
                              fn = linear_model) 
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
