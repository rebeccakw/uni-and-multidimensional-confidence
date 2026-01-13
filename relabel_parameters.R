relabel_parameters <- function(parameters, class, model, bimodal_type, diff_noise, datas, distributional_noise = FALSE) {
  #============ Fixed Model Class ===================== 
  if (class == 'fixed') {
      if (diff_noise == FALSE) {
      parameter_names = c("b1", "b2", "b3", "b4", "b5", "b6", "b7",
                          "sigma1", "sigma2", "sigma3", "sigma4")
      } else if (diff_noise == TRUE) {
        parameter_names = c("b1", "b2", "b3", "b4", "b5", "b6", "b7",
                            "vis_sigma1", "vis_sigma2", "vis_sigma3", "vis_sigma4",
                            "aud_sigma1", "aud_sigma2", "aud_sigma3", "aud_sigma4")  
      }
   if (bimodal_type == 'biased') {
     parameter_names = c(parameter_names, "weight_vis")
   }
  }
#============ Linear Model Class =====================
  if (class == 'linear') {
  if (bimodal_type != 'biased') {
    if (diff_noise == FALSE) {
    parameter_names = c("k1", "k2", "k3", "k4", "k5", "k6", "k7",
                         "m1", "m2", "m3", "m4", "m5", "m6", "m7",
                         "sigma1", "sigma2", "sigma3", "sigma4")
    } else if (diff_noise == TRUE) {
      parameter_names = c("k1", "k2", "k3", "k4", "k5", "k6", "k7",
                          "m1", "m2", "m3", "m4", "m5", "m6", "m7",
                          "vis_sigma1", "vis_sigma2", "vis_sigma3", "vis_sigma4",
                          "aud_sigma1", "aud_sigma2", "aud_sigma3", "aud_sigma4") 
    }
    if (model == 'exp'| model == 'offset' | model == 'scaler'| model == 'common' |
        model == 'noise' | model == 'flexible') {
      parameter_names = c(parameter_names, "exp")
    } 
    if (model == 'offset' | model == 'scaler') {
      parameter_names = c(parameter_names, "scaler")
    }
  } else if (bimodal_type == 'biased') {
    if (diff_noise == FALSE) {
    parameter_names = c("k1", "k2", "k3", "k4", "k5", "k6", "k7",
                        "m1", "m2", "m3", "m4", "m5", "m6", "m7",
                        "sigma1", "sigma2", "sigma3", "sigma4",
                        "weight_vis")
    } else if (diff_noise == TRUE) {
      parameter_names = c("k1", "k2", "k3", "k4", "k5", "k6", "k7",
                          "m1", "m2", "m3", "m4", "m5", "m6", "m7",
                          "vis_sigma1", "vis_sigma2", "vis_sigma3", "vis_sigma4",
                          "aud_sigma1", "aud_sigma2", "aud_sigma3", "aud_sigma4",
                          "weight_vis") 
    }
    if (model == 'exp') {
      if (diff_noise == FALSE) {
        parameter_names = c("k1", "k2", "k3", "k4", "k5", "k6", "k7",
                            "m1", "m2", "m3", "m4", "m5", "m6", "m7",
                            "sigma1", "sigma2", "sigma3", "sigma4",
                            "exp", "weight_vis")
      } else if (diff_noise == TRUE) {
        parameter_names = c("k1", "k2", "k3", "k4", "k5", "k6", "k7",
                            "m1", "m2", "m3", "m4", "m5", "m6", "m7",
                            "vis_sigma1", "vis_sigma2", "vis_sigma3", "vis_sigma4",
                            "aud_sigma1", "aud_sigma2", "aud_sigma3", "aud_sigma4",
                            "exp", "weight_vis") 
      }
    }
  }
  }
  #============ Bayesian Model Class =====================
if (class == 'bayesian') {
    if (bimodal_type != 'biased') {
      if (diff_noise == FALSE) {
      parameter_names = c("b1", "b2", "b3", "b4", "b5", "b6", "b7",
                          "sigma1", "sigma2", "sigma3", "sigma4")
      } else if (diff_noise == TRUE) {
        parameter_names = c("b1", "b2", "b3", "b4", "b5", "b6", "b7",
                            "vis_sigma1", "vis_sigma2", "vis_sigma3", "vis_sigma4",
                            "aud_sigma1", "aud_sigma2", "aud_sigma3", "aud_sigma4")
      }
      if (model == "bayes_prior") {
        if (datas == 'bimodal') {
          parameter_names = c(parameter_names, "vis_cat1_sd", "vis_cat2_sd",
            "aud_cat1_sd", "aud_cat2_sd")
        } else {
          parameter_names = c(parameter_names, "cat1_sd", "cat2_sd")
        }
      }
    } else if (bimodal_type == 'biased') {
      if (diff_noise == FALSE) {
      parameter_names = c("b1", "b2", "b3", "b4", "b5", "b6", "b7",
                          "sigma1", "sigma2", "sigma3", "sigma4",
                          "weight_vis")
      } else if (diff_noise == TRUE) {
        parameter_names = c("b1", "b2", "b3", "b4", "b5", "b6", "b7",
                            "vis_sigma1", "vis_sigma2", "vis_sigma3", "vis_sigma4",
                            "aud_sigma1", "aud_sigma2", "aud_sigma3", "aud_sigma4",
                            "weight_vis") 
      }
      if (model == "bayes_prior") {
        if (diff_noise == FALSE) {
          parameter_names = c("b1", "b2", "b3", "b4", "b5", "b6", "b7",
                              "sigma1", "sigma2", "sigma3", "sigma4",
                              "vis_cat1_sd", "vis_cat2_sd",
                              "aud_cat1_sd", "aud_cat2_sd", "weight_vis")
        } else if (diff_noise == TRUE) {
          parameter_names = c("b1", "b2", "b3", "b4", "b5", "b6", "b7",
                              "vis_sigma1", "vis_sigma2", "vis_sigma3", "vis_sigma4",
                              "aud_sigma1", "aud_sigma2", "aud_sigma3", "aud_sigma4",
                              "vis_cat1_sd", "vis_cat2_sd",
                              "aud_cat1_sd", "aud_cat2_sd", "weight_vis")
        }
      }
    }
}
  
  if (distributional_noise == TRUE) {
    parameter_names = c(parameter_names, "sig_sd")
  }
  return(parameter_names)
}