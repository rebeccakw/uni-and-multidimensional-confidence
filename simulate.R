simulate <- function(sub, confidencedata, parameters, modality = 'visual', 
                     unimodal, model, class, bimodal_type, diff_noise, parameter_names, parameter_recovery = FALSE, 
                     model_recovery = FALSE, distributional_noise = FALSE)  {

  library(mvtnorm)
  ##============DETERMINE WHAT DATA TO FIT===========
  #confidencedata$stim_orientation = scale(confidencedata$stim_orientation)
  if (all(confidencedata$stim_orientation == 0)) {
    confidencedata$stim_orientation = confidencedata$stim_frequency #hacky way of dealing with auditory data 
  } 
  
  #if fitting group data 
  if (sub != 0) {
    confidencedata = confidencedata[confidencedata$subject_name == sub,]
  }
  
##===========LOAD LIBRARIES=============  
  library(dplyr) 

##============ FUNCTIONS================
#bayesian function
source(paste0(getwd(), '/bayesian_boundary.R'))
  source(paste0(getwd(), '/multi_sd.R'))
  
  linear_boundary <- function(k, m, s, exp) {
    b = k + (m*(s^exp))
    return(b)
  }
  
##=========== GET PARAMETERS =================  
  #create dataframe with 100 repeats of every trial 
  confidencedata$trial = 1:nrow(confidencedata)
  if (parameter_recovery == TRUE | model_recovery == TRUE) {
    #otherwise creates too much data for the parameter recovery 
    #only want one simulated response for each trial 
    simulated_data <- confidencedata[rep(seq_len(nrow(confidencedata)), each = 1), ]
  } else {
  simulated_data <- confidencedata[rep(seq_len(nrow(confidencedata)), each = 100), ]
  }
  ## ============= Get Weight Parameters for All Models ===================
  if (bimodal_type == 'biased') {
    vis_weight_index = sapply(parameter_names, function(x) grepl("weight_vis", x))
    vis_weight = parameters[vis_weight_index]
    aud_weight = 1 - vis_weight
  }
 
  ## ============= Get Scaler Parameter =================== 
  # if (model == 'offset' | model == "scaler") {
  #   scaler_index = sapply(parameter_names, function(x) grepl("scaler", x))
  #   scaler = parameters[scaler_index]
  # }
  # 
  ## ============= Get Sigma SD Parameter =================== 
  if (distributional_noise == TRUE) {
    sigma_sd_index = sapply(parameter_names, function(x) grepl("sig_sd", x))
    sigma_sd = parameters[sigma_sd_index]
  }
## =========== Get Sigma Parameters for All Models ===================
  if (diff_noise == FALSE) {
    sigmas_index = sapply(parameter_names, function(x) grepl("sigma", x))
    sigmas = parameters[sigmas_index]
  } else if (diff_noise == TRUE) {
    vis_sigmas_index = sapply(parameter_names, function(x) grepl("vis_sigma", x))
    aud_sigmas_index = sapply(parameter_names, function(x) grepl("aud_sigma", x))
    vis_sigmas = parameters[vis_sigmas_index]
    aud_sigmas = parameters[aud_sigmas_index]
  }
  ## =========== Create a list of sigma values for each trial =================== 
  if (bimodal_type == 'biased') {
    if (diff_noise == FALSE) {
      vis_sigmas = aud_sigmas = sigmas
      sigma_list = sigmas[simulated_data$stim_reliability_level]
   } else {
    sigma_list = sapply(1:nrow(simulated_data), 
                        function(x) multi_sd(vis_weight, 
                                             vis_sigmas[(simulated_data$stim_reliability_level[x])],
                                             aud_sigmas[(simulated_data$stim_reliability_level[x])]))
    }
    } else if (bimodal_type == 'max' & diff_noise == TRUE) {
    all_sigmas = cbind(aud_sigmas[simulated_data$stim_reliability_level], vis_sigmas[simulated_data$stim_reliability_level])  
    sigma_list = all_sigmas[cbind(1:nrow(all_sigmas), (simulated_data$visual_mostevidence + 1))] #returns sigmas for max value, index is either 1 or 2
  } else if (model == 'noise') {
    sigma_list = c(vis_sigmas[simulated_data$stim_reliability_level[simulated_data$visual_modality == 1]], 
                   aud_sigmas[simulated_data$stim_reliability_level[simulated_data$visual_modality == 0]])
  } else if (model == "offset") {
    sigma_list = c(sigmas[simulated_data$stim_reliability_level[simulated_data$visual_modality == 1]], 
                   scaler+ sigmas[simulated_data$stim_reliability_level[simulated_data$visual_modality == 0]])
  } else if (model == "scaler") {
    sigma_list = c(sigmas[simulated_data$stim_reliability_level[simulated_data$visual_modality == 1]], 
                   scaler*sigmas[simulated_data$stim_reliability_level[simulated_data$visual_modality == 0]])
  } else if (distributional_noise == TRUE) {
    #baseline sigma values 
    sigma_list = sigmas[simulated_data$stim_reliability_level]
    #randomly sample sigma values for boundary calculation 
    adjusted_sigmas = rlnorm(nrow(simulated_data), meanlog = log(sigma_list),
                sdlog = log(sigma_sd))
   } else { 
     sigma_list = sigmas[simulated_data$stim_reliability_level]
  }
# ================ Boundary Parameters and Model Specific Parameters ===========
  if (class == "bayesian") {
  b_list <- cumsum(as.numeric(parameters[1:7])) #list of boundary parameters

  #get priors 
  if (model == 'bayes_prior') {
    if (modality == 'visual' | modality == 'auditory') {
      cat_one_sigma_index = sapply(parameter_names, function(x) grepl("cat1_sd", x))
      cat_two_sigma_index = sapply(parameter_names, function(x) grepl("cat2_sd", x))
      cat_one_sigma = parameters[cat_one_sigma_index]
      cat_two_sigma = parameters[cat_two_sigma_index]
    } else if (modality == 'bimodal') {
      vis_cat_one_sigma_index = sapply(parameter_names, function(x) grepl("vis_cat1_sd", x))
      vis_cat_two_sigma_index = sapply(parameter_names, function(x) grepl("vis_cat2_sd", x))
      aud_cat_one_sigma_index = sapply(parameter_names, function(x) grepl("aud_cat1_sd", x))
      aud_cat_two_sigma_index = sapply(parameter_names, function(x) grepl("aud_cat2_sd", x))
      vis_cat_one_sigma = parameters[vis_cat_one_sigma_index]
      vis_cat_two_sigma = parameters[vis_cat_two_sigma_index]
      aud_cat_one_sigma = parameters[aud_cat_one_sigma_index]
      aud_cat_two_sigma = parameters[aud_cat_two_sigma_index]
    }
  } else {
    if (modality == 'visual') {
      cat_one_sigma = 0.3427148
      cat_two_sigma = 1.370859
    } else if (modality == 'auditory') {
      cat_one_sigma = 0.3429777
      cat_two_sigma = 1.371911
    } else if (modality == 'bimodal') {
      vis_cat_one_sigma = 0.3427148
      vis_cat_two_sigma = 1.370859
      aud_cat_one_sigma = 0.3429777
      aud_cat_two_sigma = 1.371911
    }
  }

    if (modality != 'bimodal') {
      if (distributional_noise == TRUE) {
        pos_boundaries = t(sapply(adjusted_sigmas, function(x) calculate_x(b_list, x, cat_one_sigma, cat_two_sigma)))
        neg_boundaries = t(apply(pos_boundaries, 1, function(x) -rev(x)))
        all_boundaries = cbind(neg_boundaries, pos_boundaries)
      } else {
      pos_boundaries = t(sapply(sigma_list, function(x) calculate_x(b_list, x, cat_one_sigma, cat_two_sigma)))
      neg_boundaries = t(apply(pos_boundaries, 1, function(x) -rev(x)))
      all_boundaries = cbind(neg_boundaries, pos_boundaries)
      }
    } else if (modality == 'bimodal') {
      if (bimodal_type == 'max') {
        #get correct prior for attended modality for each trial
        #visual_mostevidence is the attended modality
        cat_one_prior = c(aud_cat_one_sigma, vis_cat_one_sigma)[(simulated_data$visual_mostevidence + 1)]
        cat_two_prior = c(aud_cat_two_sigma, vis_cat_two_sigma)[(simulated_data$visual_mostevidence + 1)]
        boundaries = t(sapply(1:length(sigma_list), function(x) calculate_x(b_list, sigma_list[x], 
                                                                                  cat_one_prior[x],
                                                                                  cat_two_prior[x])))
        all_boundaries = t(apply(boundaries, 1, function(x) c(-rev(x), x)))
      } else if (bimodal_type == 'biased') {
        #integrated and biased models with the same noise use the average of the boundaries 
        #biased model uses weights applied to stim_distance 
        vis_boundaries = t(sapply(vis_sigmas[simulated_data$stim_reliability_level], 
                                  function(x) calculate_x((b_list), x, vis_cat_one_sigma, 
                                                                vis_cat_two_sigma)))
        aud_boundaries = t(sapply(aud_sigmas[simulated_data$stim_reliability_level], 
                                  function(x) calculate_x((b_list), x, aud_cat_one_sigma, 
                                                                aud_cat_two_sigma)))
        boundaries = sqrt(((vis_boundaries*vis_boundaries) + (aud_boundaries*aud_boundaries)))
        all_boundaries = t(apply(boundaries, 1, function(x) c(-rev(x), x)))
      } 
  } 
} else if (class == "linear") {
  ks = parameters[1:7]
  ms = parameters[8:14]
  if (model == 'lin') {
    exponent = 1
  } else if (model == 'quad') {
    exponent = 2
  } else if (model == 'exp' | model == "common" | model == "offset" | model == "scaler"|
             model == 'noise' | model == 'flexible') {
    exponent_index = sapply(parameter_names, function(x) grepl("exp", x)) 
    exponent = parameters[exponent_index]
  }
 if (distributional_noise == TRUE) {
    all_boundaries = t(sapply(adjusted_sigmas, function(x) c(-rev(linear_boundary(ks, ms, x, exponent)), 
                                                        linear_boundary(ks, ms, x, exponent)))) 
    } else { 
  #get boundary parameters 
    all_boundaries = t(sapply(sigma_list, function(x) c(-rev(linear_boundary(ks, ms, x, exponent)), 
                                                        linear_boundary(ks, ms, x, exponent)))) 
  }
} else if (class == "fixed") {
  b_list <- as.numeric(parameters[1:7]) #list of boundary parameters
    #get boundary parameters 
    boundaries = c(-rev(b_list), b_list)
    all_boundaries = t(matrix(boundaries, length(boundaries), nrow(simulated_data)))
}

  if (unimodal == TRUE) {
    #recode unidimensional stimulus as stimulus distance 
    stim_distance = simulated_data$stim_orientation
  } else if (unimodal == FALSE) {
    if (bimodal_type == 'biased') {
      samples = t(sapply(1:nrow(simulated_data), function(x) rmvnorm(1, 
                        mean = c(simulated_data$stim_orientation[x], simulated_data$stim_frequency[x]),
                        sigma = matrix(c(vis_sigmas[simulated_data$stim_reliability_level[x]], 0,
                                         0, aud_sigmas[simulated_data$stim_reliability_level[x]]),nrow = 2))))
      
       # if (class == "bayesian") {
       #   #do not apply the weights because they applied to d values instead 
       #   stim_distance = sqrt(
       #     (((samples[,1]*samples[,1])) + 
       #        ((samples[,2]*samples[,2])))
       #   )
       # } else {
      stim_distance = sqrt(
        ((vis_weight*(samples[,1]*samples[,1])) + 
        (aud_weight*(samples[,2]*samples[,2])))
        )
      #}
    } else if (bimodal_type == 'max') {
      #doesn't change with different noise - will just resample values using the correct sigma 
      #for the attended modality 
      stim_distance = cbind(simulated_data$stim_frequency, 
  simulated_data$stim_orientation)[cbind(1:length(simulated_data$stim_orientation), (simulated_data$visual_mostevidence + 1))]
      
      #has to be coded as distance from the centre of the category distributions 
      stim_distance  = abs(stim_distance)
      
    } 
  } else if (unimodal == "cross-modal") {
    stim_distance = c(simulated_data$stim_orientation[simulated_data$visual_modality == 1], 
                      simulated_data$stim_orientation[simulated_data$visual_modality == 0])
  }
  #get random draws for stimulus distance 
  psychological_orientation <- rnorm(length(stim_distance), 
                                                    mean = stim_distance, 
                                                    sd = sigma_list)

  #find where orientation lies between boundaries positions
  line_up = cbind(psychological_orientation, all_boundaries)
  
  #it actually would make a lot of sense to use order() here but I can't figure out how to get it to work
  sorted_line_up = t(apply(line_up, 1, sort)) 
  
    #find position of stimulus orientation and save it as the response 
    indexes = apply(cbind(psychological_orientation, sorted_line_up), 
                    1, function(x) which(x[1] == x[2:16]))
    if (parameter_recovery == TRUE | model_recovery == TRUE) {
    responses = c("8", "7", "6", "5", "4", "3", "2", "1", "2", "3", "4", "5", "6", "7", "8")
    } else {
    responses = c("4", "3", "2", "1", "-1", "-2", "-3", "-4", "-3", "-2", "-1", "1", "2", "3", "4")
    }
  simulated_data$simulated_r = as.numeric(responses[indexes])
  
  if (unimodal == TRUE) {
    simulated_data$visual_mostevidence = NA
  }
  
  if (parameter_recovery == TRUE) {
    summarised_simulated_data <- simulated_data %>%
      group_by(trial) %>%
      summarise(stim_orientation = mean(stim_orientation),
                stim_frequency = mean(stim_frequency),
                stim_reliability_level = mean(stim_reliability_level),
                simulation_set = mean(simulation_set),
                #cat1_evidence = mean(cat1_evidence),
                #cat2_evidence = mean(cat2_evidence),
                #evi_cat2 = mean(evi_cat2),
                #evi_allcat = mean(evi_allcat),
                visual_mostevidence = mean(visual_mostevidence),
                r = mean(simulated_r),
                psychological_confidence = mean(abs(simulated_r)),
                psychological_category = mean((simulated_r > 0) + 1))
  } else if (model_recovery == TRUE) {
    summarised_simulated_data <- simulated_data %>%
      group_by(trial) %>%
      summarise(stim_orientation = mean(stim_orientation),
                stim_frequency = mean(stim_frequency),
                stim_reliability_level = mean(stim_reliability_level),
                model_simulation_set = mean(model_simulation_set),
                overall_simulation_set = mean(overall_simulation_set),
                simulated_model = unique(simulated_model),
                simulated_bimodal_type = unique(simulated_bimodal_type), 
                simulated_diff_noise = unique(simulated_diff_noise),
                visual_modality = mean(visual_modality),
                #cat1_evidence = mean(cat1_evidence),
                #cat2_evidence = mean(cat2_evidence),
                #evi_cat2 = mean(evi_cat2),
                #evi_allcat = mean(evi_allcat),
                visual_mostevidence = mean(visual_mostevidence),
                r = mean(simulated_r),
                psychological_confidence = mean(abs(simulated_r)),
                psychological_category = mean((simulated_r > 0) + 1))
  } else {
  summarised_simulated_data <- simulated_data %>%
      group_by(trial) %>%
      summarise(subject_name = unique(subject_name),
                stim_type = unique(stim_type),
                task_type = unique(task_type),
                resp_category = mean(resp_category),
                resp_confidence = mean(resp_confidence),
                resp_correct = mean(resp_correct),
                stim_orientation = mean(stim_orientation),
                stim_frequency = mean(stim_frequency),
                stim_reliability = mean(stim_reliability),
                stim_category = mean(stim_category),
                stim_reliability_level = mean(stim_reliability_level),
                #ideal_accuracy = mean(ideal_accuracy),
                r = mean(r),
                #cat1_evidence = mean(cat1_evidence),
                #cat2_evidence = mean(cat2_evidence),
                #evi_cat2 = mean(evi_cat2),
                #evi_allcat = mean(evi_allcat),
                #visual_mostevidence = mean(visual_mostevidence),
                psychological_r = mean(simulated_r),
                psychological_confidence = mean(abs(simulated_r)),
                psychological_category = mean((simulated_r > 0) + 1))
  }
    return(summarised_simulated_data)
}