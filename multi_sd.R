multi_sd <- function(vis_weight, vis_sd, aud_sd) {
  # weighted_vis_variance = vis_weight*(vis_sd^2)
  # weighted_aud_variance = (1-vis_weight)*(aud_sd^2)
  # weighted_means = vis_weight*(1-vis_weight)*((vis_mean-aud_mean)^2)
  # sum = weighted_vis_variance + weighted_aud_variance + weighted_means
  # sd_combined = sqrt(sum)
  
  weighted_vis_variance = (vis_weight)*(vis_sd*vis_sd)
  weighted_aud_variance = (1-vis_weight)*(aud_sd*aud_sd)
  sd_combined = sqrt((weighted_vis_variance + weighted_aud_variance))
  return(sd_combined)
}