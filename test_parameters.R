test_parameters <- function(parameters, class, model, bimodal_type, diff_noise, datas, parameter_names, 
                            confidencedata, distributional_noise = FALSE) {
  
  source("multi_sd.R")
  source("different_noise.R")
  
  ## ===== Test Number of Parameters is Correct =====
  #============ Fixed Model Class ===================== 
  if (class == 'fixed') {
    if (bimodal_type != 'biased') {
      #unimodal models  
      #bimodal intergrated + bimodal max models
      if ((diff_noise == FALSE & length(parameters) == 11) ||
          (diff_noise == TRUE & length(parameters) == 15)) {
        length_test = TRUE 
      } else {
        length_test = FALSE
      }
    } else if (bimodal_type == 'biased') {
      #bimodal biased model
      if  ((diff_noise == FALSE & length(parameters) == 12) ||
           (diff_noise == TRUE & length(parameters) == 16)) {
        length_test = TRUE
      } else {
        length_test = FALSE
      }
    }
  }
  #============ Linear Model Class =====================
  if (class == 'linear') {
    if (bimodal_type != 'biased') {
      #unimodal models  
      #bimodal max models with same noise 
      if (model != 'exp') {
        if ((diff_noise == FALSE & length(parameters) == 18) ||
            (diff_noise == TRUE & length(parameters) == 22) ||
            (distributional_noise == TRUE & length(parameters) == 19)) {
          length_test = TRUE
        }
      } else if (model == 'exp') {
        if ((diff_noise == FALSE & length(parameters) == 19) ||
            (diff_noise == TRUE & length(parameters) == 23) ||
            (distributional_noise == TRUE & length(parameters) == 20)) {
          length_test = TRUE
        }
      } else {
        length_test = FALSE 
      }
    } else if (bimodal_type == 'biased') {
      #bimodal biased model with same noise 
      if (model != 'exp') {
        if ((diff_noise == FALSE & length(parameters) == 19) ||
            (diff_noise == TRUE & length(parameters) == 23)) {
          length_test = TRUE
        }
      } else if (model == 'exp') {
        if ((diff_noise == FALSE & length(parameters) == 20) ||
            (diff_noise == TRUE & length(parameters) == 24)) {
          length_test = TRUE
        }
      } else {
        length_test = FALSE 
      }
    }
  }
  #============ Bayesian Model Class =====================
  if (class == 'bayesian') {
    if (bimodal_type != 'biased') {
      if (fit_prior == FALSE & 
          ((diff_noise == FALSE & (length(parameters) == 11)) |
          (diff_noise == TRUE & (length(parameters) == 15)) |
          (distributional_noise == TRUE & length(parameters) == 12))) {
        #standard Bayesian models: unimodal  &
        #bimodal integreated + max models with same noise
        length_test = TRUE
      } else if (fit_prior == TRUE & datas == 'bimodal' & 
                 ((diff_noise == FALSE & (length(parameters) == 15)) |
                  (diff_noise == TRUE & (length(parameters) == 19)))) {
        #free prior Bayesian models:
        #bimodal integreated + max models with same noise
        length_test = TRUE
      } else if (fit_prior == TRUE & datas != 'bimodal' & 
                 ((diff_noise == FALSE & (length(parameters) == 13)) |
                  (diff_noise == TRUE & (length(parameters) == 17)) |
                  (distributional_noise == TRUE & length(parameters) == 14))) {
        #free prior Bayesian models: unimodal
        length_test = TRUE
      } else {
        length_test = FALSE 
      }
    } else if (bimodal_type == 'biased') {
      if (fit_prior == FALSE & ((diff_noise == FALSE & (length(parameters) == 12)) |
                                (diff_noise == TRUE & (length(parameters) == 16)))) {
        #standard Bayesian models: 
        #biased with same noise 
        length_test = TRUE
      } else if (fit_prior == TRUE & datas == 'bimodal' & 
                 ((diff_noise == FALSE & (length(parameters) == 16)) |
                  (diff_noise == TRUE & (length(parameters) == 20)))) {
        #free prior Bayesian models:
        #biased with same noise 
        length_test = TRUE
      } else {
        length_test = FALSE 
      }
    }
  }
  ## ===== Test Number if Parameters meet Constraints =====
  ## ============ Test Sigma Parameters ==================
  #get sigma parameters
  if (diff_noise == FALSE) {
    sigmas_index = sapply(parameter_names, function(x) grepl("sigma", x))
    sigmas = parameters[sigmas_index]
  } else if (diff_noise == TRUE) {
    vis_sigmas_index = sapply(parameter_names, function(x) grepl("vis_sigma", x))
    aud_sigmas_index = sapply(parameter_names, function(x) grepl("aud_sigma", x))
    vis_sigmas = parameters[vis_sigmas_index]
    aud_sigmas = parameters[aud_sigmas_index]
    sigmas = c(vis_sigmas, aud_sigmas)
  }
  #test the sigma parameters
  if (any(sigmas <= 0) || #lower limit on sigma parameters
      any(sigmas > 20) || #upper limit on sigma parameters
      (diff_noise == TRUE && any(sort(vis_sigmas, decreasing = TRUE) != vis_sigmas)) || #order check
      (diff_noise == TRUE && any(sort(aud_sigmas, decreasing = TRUE) != aud_sigmas)) || # order check
      (diff_noise == FALSE && any(sort(sigmas, decreasing = TRUE) != sigmas))
      ) { #order check
    sigma_test = FALSE
  } else {
    sigma_test = TRUE
  }
  ## ============ Weight Parameters ================== 
  #index weight parameters
  if (bimodal_type == 'biased') {
    vis_weight_index = sapply(parameter_names, function(x) grepl("weight_vis", x))
    vis_weight = parameters[vis_weight_index]
    aud_weight = 1 - vis_weight
    if ((vis_weight + aud_weight == 1) & 
        vis_weight > 0) {
      weights_test = TRUE
    } else {
      weights_test = FALSE
    }
  } else {
    weights_test = TRUE
    vis_weight = 1 
    aud_weight = 1
  }
  
  ## ============ Distributional Noise Parameter ================== 
  if (distributional_noise == TRUE) {
    sigma_sd_index = sapply(parameter_names, function(x) grepl("sig_sd", x))
    sigma_sd = parameters[sigma_sd_index]
    if (sigma_sd > 1) {
      sigma_sd_test = TRUE
    } else {
      sigma_sd_test = FALSE
    }
  } else {
    sigma_sd_test = TRUE
  }
  
  ## ============ Test Boundary Parameters ================== 
  #define functions to calculate boundaries 
source(paste0(getwd(), "/bayesian_boundary.R"))
  
  linear_boundary <- function(k, m, s, exp) {
    b = k+(m*(s^exp))
    return(b)
  }
  
  if  (class == 'fixed') {
    boundaries = parameters[1:7]
    
    #test the boundaries 
    if (any(boundaries <= 0) ||
      any(sort(boundaries) != boundaries)) {
      boundary_test = FALSE
    } else {
      boundary_test = TRUE
    }
  } else if (class == 'linear') {
    k = parameters[1:7]
    m = parameters[8:14]
    
    #define exponent
    if (model == 'exp') {
      exp_index = sapply(parameter_names, function(x) grepl("exp", x))
      exp = parameters[exp_index]
    } else if (model == 'lin') {
      exp = 1
    } else if (model == 'quad') {
      exp = 2
    }
    
    #get sigma parameters to calculate boundaries for biased model
    if (bimodal_type == 'biased' & diff_noise == TRUE) {
      #calculate boundary parameters for biased model 
      sigma_list = sapply(1:nrow(confidencedata), 
                    function(x) multi_sd(vis_weight,
                                         vis_sigmas[(confidencedata$stim_reliability_level[x])],
                                         aud_sigmas[(confidencedata$stim_reliability_level[x])]))
      boundaries = sapply(sigma_list, function(x) linear_boundary(k, m, x, exp))
    } else if (distributional_noise == TRUE) {
      #calculate for distributional noise parameter 
      samples_per_trial = 100
      probability_sequence = rep(seq(0.01, 0.99, length.out = samples_per_trial), 
                                 length(sigmas))
      sigma_list = qlnorm(probability_sequence, 
                          meanlog = log(rep(sigmas, each= samples_per_trial)),
                          sdlog = log(rep(sigma_sd, samples_per_trial*length(sigmas))))
      boundaries = sapply(sigma_list, function(x) linear_boundary(k, m, x, exp))
    } else {
    #sigmas will either be shared sigmas or 
    # c(vis_sigmas, aud_sigmas) for different noise max models 
    #or as above for biased model, different noise models 
    boundaries = sapply(sigmas, function(x) linear_boundary(k, m, x, exp))
    }
    
  #test boundaries 
    if (any(boundaries <= 0) || #lower limit
       exp < 1 || 
      (any(apply(boundaries, 2, function(x) sort(x)) != boundaries))) { #order check
        boundary_test = FALSE
      } else {
        boundary_test = TRUE
      }
  } else if (class == 'bayesian') {
   bs = cumsum(parameters[1:7]) 
   if (fit_prior == FALSE) {
     if (datas == 'visual') {
       cat_one_sigma = 0.3427148
       cat_two_sigma = 1.370859
     } else if (datas == 'auditory') {
       cat_one_sigma = 0.3429777
       cat_two_sigma = 1.371911
     } else if (datas == 'bimodal') {
       vis_cat_one_sigma = 0.3427148
       vis_cat_two_sigma = 1.370859
       aud_cat_one_sigma = 0.3429777
       aud_cat_two_sigma = 1.371911
     }
   } else if (fit_prior == TRUE) {
     if (datas == 'visual' | datas == 'auditory') {
       cat_one_sigma_index = sapply(parameter_names, function(x) grepl("cat1_sd", x))
       cat_two_sigma_index = sapply(parameter_names, function(x) grepl("cat2_sd", x))
       cat_one_sigma = parameters[cat_one_sigma_index]
       cat_two_sigma = parameters[cat_two_sigma_index]
     } else if (datas == 'bimodal') {
       vis_cat_one_sigma_index = sapply(parameter_names, function(x) grepl("vis_cat1_sd", x))
       vis_cat_two_sigma_index = sapply(parameter_names, function(x) grepl("vis_cat2_sd", x))
       aud_cat_one_sigma_index = sapply(parameter_names, function(x) grepl("aud_cat1_sd", x))
       aud_cat_two_sigma_index = sapply(parameter_names, function(x) grepl("aud_cat2_sd", x))
       vis_cat_one_sigma = parameters[vis_cat_one_sigma_index]
       vis_cat_two_sigma = parameters[vis_cat_two_sigma_index]
       aud_cat_one_sigma = parameters[aud_cat_one_sigma_index]
       aud_cat_two_sigma = parameters[aud_cat_two_sigma_index]
     }
   }

   if (diff_noise == FALSE) {
     if (datas != 'bimodal') {
       boundaries = sapply(sigmas, function(x) calculate_x(bs, x, cat_one_sigma, cat_two_sigma))
     } else if (datas == 'bimodal') {
       vis_boundaries = sapply(sigmas, function(x) calculate_x((bs), x, vis_cat_one_sigma, 
                                                                     vis_cat_two_sigma))
       aud_boundaries = sapply(sigmas, function(x) calculate_x((bs), x, aud_cat_one_sigma, 
                                                                     aud_cat_two_sigma))
       boundaries = sqrt(((vis_boundaries*vis_boundaries) + (aud_boundaries*aud_boundaries)))
     }
   } else if (diff_noise == TRUE) {
     vis_boundaries = sapply(vis_sigmas, function(x) calculate_x((bs), x, vis_cat_one_sigma, 
                                                                       vis_cat_two_sigma))
     aud_boundaries =  sapply(aud_sigmas, function(x) calculate_x((bs), x, 
                                                                        aud_cat_one_sigma, aud_cat_two_sigma))
     if (bimodal_type == 'max') {
       boundaries = cbind(vis_boundaries, aud_boundaries)
     } else if (bimodal_type != 'max') {
       #biased models - calculate boundaries for each intensity level using distance measure 
       boundaries = sapply(1:ncol(vis_boundaries), function(x) sqrt( (vis_boundaries[,x]*vis_boundaries[,x]) + 
                                                         (aud_boundaries[,x]*aud_boundaries[,x]) ) )
     }
   }
   
   #test boundaries 
   if (any(boundaries <= 0) || #lower limit
       (datas != 'bimodal' && any(c(cat_one_sigma, cat_two_sigma) <= 0)) || #prior check
       (datas == 'bimodal' && any(c(vis_cat_one_sigma, vis_cat_two_sigma, 
                                    aud_cat_one_sigma, aud_cat_two_sigma) <= 0)) || #prior check
       (any((apply(boundaries, 2, function(x) sort(x))) != boundaries))) { #order
     boundary_test = FALSE
   } else {
     boundary_test = TRUE
   }
  }
  #did we pass all of the tests 
  all_tests = c(length_test, sigma_test, weights_test, sigma_sd_test, boundary_test)
  return(all_tests)
}

