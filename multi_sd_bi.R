multi_sd_bi <- function(vis_sd, aud_sd, weight_vis) {
  ##predict some values for the visual sigmas
  #need to do log transform to fit linear model
  y_vis = log10(vis_sd)
  #intensity (contrast) values in the visual dimension
  x_vis = c(3.3,5,6.7,13.5)
  #fit a linear model
  model_vis <- lm(y_vis ~ x_vis)
  #get some predictions for the new intensity values in the visual dimension 
  new_data_vis = data.frame(x_vis = c(4.2, 5.3))
  new_vis_sigmas = exp(c(y_vis[1], as.numeric(predict(model_vis, new_data_vis)), y_vis[3]))
    
  ##predict some values for the auditory sigmas
  y_aud = log10(aud_sd)
  x_aud = c(-3,4,10.5,17)
  model_aud <- lm(y_aud ~ x_aud)
  new_data_aud = data.frame(x_aud = c(2, 6))
  new_aud_sigmas = exp(c(y_aud[1], as.numeric(predict(model_aud, new_data_aud)), y_aud[3]))
  
  #combine these estimates across modalities
  vis_variance = (weight_vis)*(new_vis_sigmas*new_vis_sigmas)
  aud_variance = (1- weight_vis) *(new_aud_sigmas*new_aud_sigmas)
  sd_combined = (vis_variance + aud_variance)
  bimodal_sd = sqrt(sd_combined)
  return(bimodal_sd)
}