different_noise <- function(stim_orientation, stim_frequency, vis_sigma, aud_sigma) {
  #stim_orientation - vector of stimulus orientations - 1 value for each trial 
  #stim_frequency - vector of stimulus frequncies - 1 value for each trial 
  #vis_sigma - vector of sigma values for corresponding stimulus orientations  
  #aud_sigma - vector of sigma values for corresponding stimulus frequencies 
library(mvtnorm)

  #get 11 evenly spaced draws of orientation values for each 
#trial from 5th to 95th percentile - need to combine these 
#with frequency draws so will have a total of 121 combinations
#(11*11)
oris = sapply(1:length(stim_orientation), function(x) 
  seq(qnorm(0.05, stim_orientation[x],vis_sigma[x]),
            qnorm(0.95, stim_orientation[x], vis_sigma[x]),
            length.out = 11))

#get frequency draws as above
freqs = sapply(1:length(stim_frequency), function(x) 
  seq(qnorm(0.05, stim_frequency[x],aud_sigma[x]),
            qnorm(0.95, stim_frequency[x], aud_sigma[x]),
            length.out = 11))

#get all possible combinations of orientation and frequency draws for 
#each trial
combinations = sapply(1:ncol(oris), function(x) expand.grid(oris[,x],freqs[,x]))
rownames(combinations) <- c("oris","freqs")

#===== get densities for draws ======== 
#get density for draws
density = sapply(1:length(stim_orientation), function(x) 
  dmvnorm(cbind((combinations[1,x]$oris), (combinations[2,x]$freqs)), 
          mean = c(stim_orientation[x], stim_frequency[x]), 
                  sigma = matrix(c(vis_sigma[x], 0, 0, aud_sigma[x]), nrow = 2)))
#normalise so that densities sum to 1
normalised_density = apply(density, 2, function(x) x/sum(x))

#return everything in a list 
oris_freqs_densities = list(combinations[1,], combinations[2,], normalised_density)
return(oris_freqs_densities)
}


