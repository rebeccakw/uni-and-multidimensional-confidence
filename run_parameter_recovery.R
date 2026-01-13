rm(list = ls())

master_directory = "/Users/rebeccawest/Dropbox/Documents/multimodal_confidence"
setwd(paste0(master_directory, "/updated/Bidimensional_Confidence"))

#load libraries
library(doParallel)
library(foreach)
library(dplyr)
library(beepr)
library(tidyverse)

data_size = 720
n_subjects = 20 #10
parameters_per_subject = 1 #5
reps = 10
simulation_set_n = n_subjects*parameters_per_subject #3
datas = c("visual") #c("visual", "bimodal")
models = c("bayes_prior") #c("fixed", "lin", "quad", "exp", "bayes", "bayes_prior")
bimodal_types = c("NA") #c("max", "biased") use NA for unimodal models  
diff_noise_types = c(FALSE)
distributional_noise_types = c(FALSE)

#set up parallel processing 
cores = 8
cl <- makeCluster(cores)
registerDoParallel(cl)

source(paste0(getwd(), "/relabel_parameters.R"))
source(paste0(getwd(), "/test_parameters.R"))
source(paste0(getwd(), "/FacetEqualWrap.R"))

#set up some data to simulate model responses for 
confidencedata <- data.frame(matrix(vector(), data_size*simulation_set_n, 5, 
                                    dimnames = list(c(), c("stim_orientation", "stim_frequency", "stim_reliability_level",
                                                           "r", "simulation_set"))))
confidencedata$simulation_set = rep(1:simulation_set_n, each = data_size)

for (data in datas) {
  ##========= LOAD DATA =================
  #load data - this won't work for multiple data sets 
  if (data == "visual" | data == "auditory") {
    ## ====== Simulate Experimental Data =====
    for (set in 1:simulation_set_n) {
      #sample equally form each reliability level
      confidencedata$stim_reliability_level[confidencedata$simulation_set == set] = rep(1:4, data_size/4)
      #randomly sample from the standardised category distributions
      confidencedata$stim_orientation[confidencedata$simulation_set == set] = c(rnorm(data_size/2, 0, 0.35), 
                                                                                rnorm(data_size/2, 0, 1.37))
      confidencedata$stim_frequency = 0
    }
  } else if (data == "bimodal") {
    for (set in 1:simulation_set_n) {
      #sample equally form each reliability level
      confidencedata$stim_reliability_level[confidencedata$simulation_set == set] = rep(1:4, data_size/4)
      #randomly sample from the standardised category distributions
      confidencedata$stim_orientation[confidencedata$simulation_set == set] = c(rnorm(data_size/2, 0, 0.35), 
                                                                                rnorm(data_size/2, 0, 1.37))
      confidencedata$stim_frequency[confidencedata$simulation_set == set] = c(rnorm(data_size/2, 0, 0.35), 
                                                                                rnorm(data_size/2, 0, 1.37))
      #calculate which dimension is the most informative for each stimlus combination
      #which category has the most evidence 
      aud_evidence = apply(cbind(dnorm(confidencedata$stim_frequency, 0, 0.35), 
                         dnorm(confidencedata$stim_frequency, 0, 1.37)),1, max)
      visual_evidence = apply(cbind(dnorm(confidencedata$stim_orientation, 0, 0.35), 
                                 dnorm(confidencedata$stim_orientation, 0, 1.37)), 1,max)
      #does the visual dimension have more evidence
      confidencedata$visual_mostevidence = as.numeric(visual_evidence > aud_evidence)
    }
  }
  
  #loop over models for dataset 
  for (model in models) {
    if (data == 'visual' | data == 'auditory') {
      this_model_bimodal_types = FALSE
      this_model_diff_noise_types = FALSE
      this_model_distributional_noise_types = distributional_noise_types 
    } else if (data == 'bimodal') {
      #fit all bimodal versions if bimodal data 
      this_model_bimodal_types = bimodal_types
      this_model_diff_noise_types = diff_noise_types
      this_model_distributional_noise_types = FALSE
    }
    
    #loop over bimodal_types if appropriate
    for (bimodal_type in this_model_bimodal_types) {
      for (diff_noise in this_model_diff_noise_types) {
        for (distributional_noise in this_model_distributional_noise_types) {
        #select model class 
        if (model == 'lin' | model == 'quad' | model == 'exp') {
          class = "linear"
        } else if (model == 'bayes' | model == 'bayes_prior') {
          class = "bayesian"
        } else if (model == 'fixed') {
          class = "fixed"
          distributional_noise = FALSE 
        }
        modality = data
        if (data == "visual" | data == "auditory") {
          unimodal = TRUE
        } else {unimodal = FALSE}
        ##========= GENERATE STARTING PARAMETERS =================
        #generate starting parameters  - only works with a single model right now 
        source(paste0(master_directory, "/updated/Bidimensional_Confidence", "/generate_", class, "_parameters.R"))
        tic = Sys.time()
        if (class == "fixed") {
          starting_parameters <- foreach(sub = 1:simulation_set_n,
                                         .combine = "rbind")  %dopar% {
                                           output = generate_fixed_parameters(bimodal_type = bimodal_type,
                                                                              diff_noise = diff_noise)
                                         }
        } else if (class == "linear") {
          #generate more starting parameters than we need because this is pretty
          #quick and fails often
          starting_parameters <- foreach(sub = 1:(simulation_set_n+10),
                                         .combine = "rbind")  %dopar% {
                                           output = generate_linear_parameters(model = model, 
                                                                               modality = modality,
                                                                               bimodal_type = bimodal_type,
                                                                               diff_noise = diff_noise,
                                                                               distributional_noise = distributional_noise)
                                         }
        } else if (class == "bayesian") {
          if (model == "bayes_prior") {
            fit_prior = TRUE
          } else {fit_prior = FALSE}
          starting_parameters <-foreach(sub = 1:(simulation_set_n+5),
                                        .combine = "rbind")  %dopar% {
                                          output = generate_bayesian_parameters(modality = modality,
                                                                                fit_prior = fit_prior, 
                                                                                bimodal_type = bimodal_type,
                                                                                diff_noise = diff_noise,
                                                                                distributional_noise = distributional_noise)
                                        }
        }
        toc = Sys.time()
        toc - tic
        beep()
        
        #get starting parameters that aren't NA
        starting_parameters = starting_parameters[which(!is.na(starting_parameters[,1])),]
        ## =========== Test the chosen starting parameters ===================
        parameter_names = relabel_parameters(starting_parameters, class, model, bimodal_type,
                                            diff_noise, data, distributional_noise)
        test = apply(starting_parameters, 1, function(x)
          test_parameters(x, class, model, bimodal_type, diff_noise, data, parameter_names, confidencedata, distributional_noise))
        if (all(test == TRUE)) {
          print(paste(data, class, model, 
                      paste("(bimodal_type =", bimodal_type, ")"), 
                      paste("(diff_noise =", diff_noise, ")"),
                      paste("(distributional_noise =", distributional_noise, ")"),
                      "starting parameters passed"))
        } else {
          print(paste(data, class, model, 
                      paste("(bimodal_type =", bimodal_type, ")"),
                      paste("(diff_noise =", diff_noise, ")"),
                      paste("(distributional_noise =", distributional_noise, ")"),
                      "starting parameters did NOT pass"))
        }
        #=======================================================================================
        if (nrow(starting_parameters) < simulation_set_n) {
          print("Not enough starting_parameters")
        } else if (nrow(starting_parameters) > simulation_set_n) {
          #if we have too many starting parameters, randomly select a few of them 
          random_selection = sample(1:nrow(starting_parameters), simulation_set_n)
          starting_parameters = starting_parameters[random_selection,]
        }
        combinations = cbind(rep(1:n_subjects, each = parameters_per_subject), starting_parameters)
        
        ##=========== SIMULATE DATA==============
        source(paste0(master_directory, "/updated/Bidimensional_Confidence", "/simulate.R"))
                        simulated_data <- foreach(x = 1:nrow(starting_parameters),
                                  .combine = "rbind")  %dopar% {
                                    output = simulate(sub = 0, #subject_number N/A here
                                                      confidencedata = confidencedata[confidencedata$simulation_set == x,], 
                                                      parameters = as.numeric(starting_parameters[x,]), #simulate data using starting parameters
                                                      modality = data,
                                                      unimodal = unimodal, model = model,
                                                      class = class, bimodal_type = bimodal_type,
                                                      diff_noise = diff_noise,
                                                      parameter_names = parameter_names, 
                                                      parameter_recovery = TRUE, 
                                                      model_recovery = FALSE,
                                                      distributional_noise = distributional_noise)
                                  }
        
        
        ##========= RUN MODEL =================
        source(paste0(master_directory, "/updated/Bidimensional_Confidence", "/", class, "_model.R"))
        tic = Sys.time()
        n_parameters = ncol(starting_parameters)
        if (class == "fixed") {
          output <- foreach(x = 1:nrow(combinations),
                            .combine = "rbind")  %dopar% {
                              output = fixed_model_function(simulated_data[simulated_data$simulation_set == x,], 
                                                            1:n_subjects, #subject_number
                                                            reps = reps,  #number of repetitions
                                                            as.numeric(combinations[x, 2:(n_parameters+1)]), #starting_parameters 
                                                            unimodal = unimodal, model = model, 
                                                            bimodal_type = bimodal_type,
                                                            diff_noise = diff_noise,
                                                            parameter_names = parameter_names) 
                            }
        } else if (class == "linear") {
          output <- foreach(x = 1:nrow(combinations),
                            .combine = "rbind")  %dopar% {
                              output = linear_model_function(simulated_data[simulated_data$simulation_set == x,], 
                                                             1:n_subjects, #subject_number
                                                             reps = reps,  #number of repetitions
                                                             as.numeric(combinations[x, 2:(n_parameters+1)]), #starting_parameters 
                                                             unimodal = unimodal, model = model, 
                                                             bimodal_type = bimodal_type,
                                                             diff_noise = diff_noise,
                                                             parameter_names = parameter_names, 
                                                             distributional_noise = distributional_noise)
                            }
        } else if (class == "bayesian") {
          output <- foreach(x = 1:nrow(combinations),
                            .combine = "rbind")  %dopar% {
                              output = bayesian_model_function(simulated_data[simulated_data$simulation_set == x,], #get data for subject
                                                               1:n_subjects, #give function subject number  
                                                               reps = reps,
                                                               as.numeric(combinations[x, 2:(n_parameters+1)]), #starting parameters 
                                                               data, fit_prior = fit_prior, unimodal = unimodal,
                                                               model = model, bimodal_type = bimodal_type,
                                                               diff_noise = diff_noise,
                                                               parameter_names = parameter_names,
                                                               distributional_noise = distributional_noise) 
                            }
        }
        toc = Sys.time()
        toc - tic
        beep()
        
        ## =========== Test the estimated parameters ===================
        test = apply(output[,1:n_parameters], 1, function(x)
          test_parameters(x, class, model, bimodal_type, diff_noise, data, parameter_names, confidencedata, distributional_noise))
        if (all(test == TRUE)) {
          print(paste(data, class, model, 
                      paste("(bimodal_type =", bimodal_type, ")"), 
                      paste("(diff_noise =", diff_noise, ")"),
                      paste("(distributional_noise =", distributional_noise, ")"),
                      "estimated parameters passed"))
        } else {
          print(paste(data, class, model, 
                      paste("(bimodal_type =", bimodal_type, ")"),
                      paste("(diff_noise =", diff_noise, ")"),
                      paste("(distributional_noise =", distributional_noise, ")"),
                      "estimated parameters did NOT pass"))
        }
        
        
        colnames(output) <- c(parameter_names, "subject", "log_likelihood", "AIC", "BIC")
        parameter_recovery = cbind(stack(output[,1:n_parameters]), stack(data.frame(starting_parameters)))
        parameter_recovery$set = rep(1:nrow(starting_parameters), each = n_parameters)
        colnames(parameter_recovery) <- c("fitted", "parameter", "generating", "NA", "set")
        
        if (data == 'bimodal') {
          write.csv(parameter_recovery, paste0(master_directory, "/recovery_data/", 
                                               data, "_", model, "_", 
                                               paste0("(bimodal_type=", bimodal_type, ")"), "_",
                                               paste0("(diff_noise=", diff_noise, ")"),
                                               "720trials.csv"))
        } else {
          write.csv(parameter_recovery, paste0(master_directory, "/recovery_data/", 
                                               paste0("(distributional_noise=", distributional_noise, ")"),
                                      data, "_", model, "720trials.csv"))
        }
        
        ggplot(parameter_recovery) + geom_point(aes(generating, fitted)) + 
          facet_wrap_equal(~parameter, scales = "free", ncol = 3) +
          geom_abline(slope = 1, intercept = 0, alpha = 0.7, lty = 2) + xlab("Generating Parameter") + 
          ylab("Recovered Parameter") + theme(strip.text.x = element_text(size = 20), axis.text=element_text(size=14),
                                              axis.title=element_text(size=25))
        height = (2.5*(ceiling(n_parameters/3)))
        ggsave(paste0(master_directory, "/recovery_data/plots/", 
                      data, "_", model, "_", 
                      paste0("(bimodal_type=", bimodal_type, ")"), "_",
                      paste0("(diff_noise=", diff_noise, ")"),
                      paste0("(distributional_noise=", distributional_noise, ")"),
                      "720trials.pdf"), width = 9.72, height = height)
        rm(output)
    }
  }
  }
}
}

#save the log file 
#sink()

