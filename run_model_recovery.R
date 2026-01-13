rm(list = ls())

master_directory = "/Users/rebeccawest/Dropbox/Documents/multimodal_confidence"
setwd(paste0(master_directory, "/updated/Bidimensional_Confidence"))

data_size = 720
n_datasets = 100 #number of simulated datasets per model 
reps = 100
datas = c("visual") #c("visual", "bimodal", "cross-modal")
models = c("fixed", "lin", "quad", "exp", "bayes", "bayes_prior") #c("fixed", "lin", "quad", "exp", "bayes", "bayes_prior")
bimodal_types = c("NaN") #c("max", "biased") use NA for unimodal models  
diff_noise_types = c(FALSE)
distributional_noise_types = c(FALSE)

#load libraries
library(doParallel)
library(foreach)
library(dplyr)
library(beepr)
library(tidyverse)

#set up parallel processing 
cores = 8
cl <- makeCluster(cores)
registerDoParallel(cl)

source(paste0(getwd(), "/relabel_parameters.R"))
source(paste0(getwd(), "/test_parameters.R"))
source(paste0(getwd(), "/FacetEqualWrap.R"))


#set up some data to simulate model responses for 
confidencedata <- data.frame(matrix(vector(), data_size*n_datasets*length(models), 5, 
                                    dimnames = list(c(), c("stim_orientation", "stim_frequency", "stim_reliability_level",
                                                           "r", "visual_modality"))))
#this is for the cross modal models only 
confidencedata$visual_modality = "NaN"

#label the simulated model 
confidencedata$simulated_model = rep(models, each = data_size*n_datasets)

#simulation sets get labelled within models 
confidencedata$model_simulation_set = rep(rep(1:n_datasets, each = data_size), length(models))

#simulation sets get labelled across models 
confidencedata$overall_simulation_set = rep(1:(n_datasets*length(models)), each = data_size)

##========= SIMULATE ALL THE RELEVANT DATA =================
for (data in datas) {
  ##========= CREATE EXPERIMENTAL DATA =================
  #create some data 
  if (data == "visual" | data == "auditory") {
    ## ====== Simulate Experimental Data =====
    for (set in 1:max(confidencedata$overall_simulation_set)) {
      #sample equally form each reliability level
      confidencedata$stim_reliability_level[confidencedata$overall_simulation_set == set] = rep(1:4, data_size/4)
      #randomly sample from the standardised category distributions
      confidencedata$stim_orientation[confidencedata$overall_simulation_set == set] = c(rnorm(data_size/2, 0, 0.35), 
                                                                                rnorm(data_size/2, 0, 1.37))
      confidencedata$stim_frequency = 0
    }
  } else if (data == "bimodal") {
    for (set in 1:max(confidencedata$overall_simulation_set)) {
      #sample equally form each reliability level
      confidencedata$stim_reliability_level[confidencedata$overall_simulation_set == set] = rep(1:4, data_size/4)
      #randomly sample from the standardised category distributions
      confidencedata$stim_orientation[confidencedata$overall_simulation_set == set] = c(rnorm(data_size/2, 0, 0.35), 
                                                                                rnorm(data_size/2, 0, 1.37))
      confidencedata$stim_frequency[confidencedata$overall_simulation_set == set] = c(rnorm(data_size/2, 0, 0.35), 
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
    #get revelevant data for this model 
    this_confidencedata = confidencedata %>%
      filter(simulated_model == model)
    
    if (data == 'visual' | data == 'auditory' | data == 'cross-modal') {
      this_model_bimodal_types = FALSE
      this_model_diff_noise_types = FALSE
      this_model_distributional_noise_types = distributional_noise_types 
    } else if (data == 'bimodal') {
      #fit all bimodal versions if bimodal data 
      this_model_bimodal_types = bimodal_types
      this_model_diff_noise_types = diff_noise_types
      this_model_distributional_noise_types = FALSE
    } else if (model == 'noise' | model == 'flexible') {
      this_model_bimodal_types = 'NaN'
      this_model_diff_noise_types = TRUE
      this_model_distributional_noise_types = FALSE
    }
    
    #loop over bimodal_types if appropriate
    for (bimodal_type in this_model_bimodal_types) {
      for (diff_noise in this_model_diff_noise_types) {
        for (distributional_noise in this_model_distributional_noise_types)
        #select model class 
        if (model == 'lin' | model == 'quad' | model == 'exp' | 
            model == 'noise' | model == 'offset' | model == 'scaler' |
            model == 'common' | model == 'flexible') {
          class = "linear"
        } else if (model == 'bayes' | model == 'bayes_prior') {
          class = "bayesian"
        } else if (model == 'fixed') {
          class = "fixed"
        }
        modality = data
        if (data == "visual" | data == "auditory") {
          unimodal = TRUE
        } else  if (data == "cross-modal") {
          unimodal = "cross-modal"
        } else {unimodal = FALSE}
        ##========= GENERATE STARTING PARAMETERS =================
        #load in the relevant functions for this model
        source(paste0(master_directory, "/updated/Bidimensional_Confidence", "/generate_", class, "_parameters.R"))
        source(paste0(master_directory, "/updated/Bidimensional_Confidence", "/", class, "_model.R"))
        
        #sample the starting parameters for each model 
        tic = Sys.time()
        if (class == "fixed") {
          starting_parameters <- foreach(sub = 1:n_datasets,
                                         .combine = "rbind")  %dopar% {
                                           output = generate_fixed_parameters(bimodal_type = bimodal_type,
                                                                              diff_noise = diff_noise)
                                         }
        } else if (class == "linear") {
          #generate more starting parameters than we need because this is pretty
          #quick and fails often
          starting_parameters <- foreach(sub = 1:(n_datasets+10),
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
          starting_parameters <-foreach(sub = 1:(n_datasets+5),
                                        .combine = "rbind")  %dopar% {
                                          output = generate_bayesian_parameters(this_confidencedata, modality = modality,
                                                                                fit_prior = fit_prior, 
                                                                                bimodal_type = bimodal_type,
                                                                                diff_noise = diff_noise)
                                        }
        }
        toc = Sys.time()
        toc - tic
        beep()
        
        #get starting parameters that aren't NA
        starting_parameters = starting_parameters[which(!is.na(starting_parameters[,1])),]
        ## =========== Test the chosen starting parameters ===================
        parameter_names = relabel_parameters(starting_parameters[1,], class, model, bimodal_type,
                                             diff_noise, data, distributional_noise)
        test = apply(starting_parameters, 1, function(x)
          test_parameters(x, class, model, bimodal_type, diff_noise, data, parameter_names, this_confidencedata,
                          distributional_noise))
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
        if (nrow(starting_parameters) < n_datasets) {
          print("Not enough starting_parameters")
        } else if (nrow(starting_parameters) > n_datasets) {
          #if we have too many starting parameters, randomly select a few of them 
          random_selection = sample(1:nrow(starting_parameters), n_datasets)
          starting_parameters = starting_parameters[random_selection,]
        }
        
        ##=========== SIMULATE DATA==============
        source(paste0(master_directory, "/updated/Bidimensional_Confidence", "/simulate.R"))
        simulated_data <- foreach(x = 1:nrow(starting_parameters),
                                  .combine = "rbind")  %dopar% {
                                    output = simulate(0, #subject_number N/A here
                                                      this_confidencedata[this_confidencedata$model_simulation_set == x,], 
                                                      as.numeric(starting_parameters[x,]), #simulate data using starting parameters
                                                      modality = data,
                                                      unimodal = unimodal, model = model,
                                                      class = class, bimodal_type = bimodal_type,
                                                      diff_noise = diff_noise,
                                                      parameter_names = parameter_names, 
                                                      parameter_recovery = FALSE, 
                                                      model_recovery = TRUE,
                                                      distributional_noise = distributional_noise)
 
                                                                     }
        simulated_parameters = cbind(starting_parameters,1:nrow(starting_parameters),
                                     rep(model, nrow(starting_parameters)))
        colnames(simulated_parameters) = c(parameter_names, 'simulation_set', 'simulated_model')
        
        if (data == 'bimodal') {
          #save the simulated data 
          write.csv(simulated_data, paste0(master_directory, "/model_recovery/simulated_data/", 
                                               data, "_", model, "_", 
                                               paste0("(bimodal_type=", bimodal_type, ")"), "_",
                                               paste0("(diff_noise=", diff_noise, ")"),
                                               ".csv"))
          #save the parameters that we simulated data with 
          write.csv(simulated_parameters, paste0(master_directory, "/model_recovery/simulated_parameters/", class,
                                                 "/", data, "_", model, "_", 
                                           paste0("(bimodal_type=", bimodal_type, ")"), "_",
                                           paste0("(diff_noise=", diff_noise, ")"),
                                           "_parameters.csv"))
          
        } else {
          #save the simulated data
          write.csv(simulated_data, paste0(master_directory, "/model_recovery/simulated_data/", 
                                               data, "_", model, "_distributional_noise=",
                                               distributional_noise, ".csv"))
          
          #save the parameters we simulated data with 
          write.csv(simulated_parameters, paste0(master_directory, "/model_recovery/simulated_parameters/",class,
                                                 "/", data, "_", model, "_distributional_noise=",
                                                 distributional_noise, "_parameters.csv"))
        }
      }
    }
  }
}
#get all csv files from model recovery directory
files = list.files(paste0(master_directory, "/model_recovery/simulated_data"), pattern = ".csv", full.names = TRUE)

#use lapply to read in each file and combine them into a list of dataframes
df_list <- lapply(files, read.csv)

#use do.call to combine all dataframes into a single dataframe
all_simulated_data <- do.call(rbind, df_list)[,-1]

#want to fit every model to every simulated dataset
#how many models are we fitting
all_model_results = data.frame()
        
##========= FITS MODELS TO SIMULATED DATA =================
for (data in datas) {
for (model in models) {
  
if (model == 'lin' | model == 'quad' | model == 'exp') {
  class = "linear"
} else if (model == 'bayes' | model == 'bayes_prior') {
  class = "bayesian"
} else if (model == 'fixed') {
  class = "fixed"
}

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
      #load the starting parameters that we used to simulate data
      if (data == 'bimodal') {
        fit_starting_parameters = read.csv(paste0(master_directory, "/model_recovery/simulated_parameters/", class,
                                               "/", data, "_", model, "_", 
                                               paste0("(bimodal_type=", bimodal_type, ")"), "_",
                                               paste0("(diff_noise=", diff_noise, ")"),
                                               "_parameters.csv"))[,-1]
        
      } else {
        fit_starting_parameters = read.csv(paste0(master_directory, "/model_recovery/simulated_parameters/",class,
                                                  "/", data, "_", model, "_distributional_noise=",
                                                  distributional_noise, "_parameters.csv"))[,-1]
      }
      #remove last 2 columns (information we don't need here)
      all_starting_parameters = fit_starting_parameters[, -((ncol(fit_starting_parameters)-1):ncol(fit_starting_parameters))]
      
      #get parameter names 
      parameter_names = relabel_parameters(all_starting_parameters[1,], class, model, bimodal_type,
                                           diff_noise, data, distributional_noise)

        if (class == "fixed") {
          output <- foreach(x = 1:max(all_simulated_data$overall_simulation_set),
                            .combine = "rbind")  %dopar% {
                              output = fixed_model_function(all_simulated_data[all_simulated_data$overall_simulation_set == x,], #get the data for a single set 
                                                            1:n_datasets, #just needs to be more than 1 number 
                                                            reps = reps,  #number of repetitions
#hacky but ensures we use the generating parameters if simulated model == model we are fitting 
as.numeric(all_starting_parameters[unique(all_simulated_data$model_simulation_set[all_simulated_data$overall_simulation_set == x]),]), 
                                                            unimodal = unimodal, model = model, 
                                                            bimodal_type = bimodal_type,
                                                            diff_noise = diff_noise,
                                                            parameter_names = parameter_names, recovery = TRUE) 
                            }
          output <- cbind(output, fit_model = model)
        } else if (class == "linear") {
          output <- foreach(x = 1:max(all_simulated_data$overall_simulation_set),
                            .combine = "rbind")  %dopar% {
                              output = linear_model_function(all_simulated_data[all_simulated_data$overall_simulation_set == x,], 
                                                             1:n_datasets, #subject_number
                                                             reps = reps,  #number of repetitions
#hacky but ensures we use the generating parameters if simulated model == model we are fitting 
as.numeric(all_starting_parameters[unique(all_simulated_data$model_simulation_set[all_simulated_data$overall_simulation_set == x]),]), 
                                                             unimodal = unimodal, model = model, 
                                                             bimodal_type = bimodal_type,
                                                             diff_noise = diff_noise,
                                                             parameter_names = parameter_names, recovery = TRUE,
                                                            distributional_noise = distributional_noise) 
                            }
          
          output <- cbind(output, fit_model = model)
        } else if (class == "bayesian") {
          if (model == "bayes_prior") {
            fit_prior = TRUE
          } else {fit_prior = FALSE}
          output <- foreach(x = 1:max(all_simulated_data$overall_simulation_set),
                            .combine = "rbind")  %dopar% {
                              output = bayesian_model_function(all_simulated_data[all_simulated_data$overall_simulation_set == x,], #get data for subject
                                                               1:n_datasets, #give function subject number  
                                                               reps = reps,
#hacky but ensures we use the generating parameters if simulated model == model we are fitting 
as.numeric(all_starting_parameters[unique(all_simulated_data$model_simulation_set[all_simulated_data$overall_simulation_set == x]),]), 
                                                               modality = data, fit_prior = fit_prior, unimodal = unimodal,
                                                               model = model, bimodal_type = bimodal_type,
                                                               diff_noise = diff_noise,
                                                               parameter_names = parameter_names, recovery = TRUE, 
                                                              distributional_noise = distributional_noise) 
                            }
          output <- cbind(output, fit_model = model)
        }
      if (data == 'bimodal') {
        #save the simulated data 
        write.csv(output, paste0(master_directory, "/model_recovery/fit_parameters/", 
                                         data, "_", model, "_", 
                                         paste0("(bimodal_type=", bimodal_type, ")"), "_",
                                         paste0("(diff_noise=", diff_noise, ")"),
                                         ".csv"))
        
      } else {
        #save the parameters we simulated data with 
        write.csv(output, paste0(master_directory, "/model_recovery/fit_parameters/",class,
                                               "/", data, "_", model, "_distributional_noise=", 
                                 distributional_noise,"_parameters.csv"))
      }
#also get what we need so we can make the comparison
      this_model_results = output[, c("AIC", "BIC", "overall_simulation_set", "simulated_model", "fit_model")]
      all_model_results = rbind(all_model_results, this_model_results)
      
      print(paste(data, class, model, 
                  paste("(bimodal_type =", bimodal_type, ")"),
                  paste("(diff_noise =", diff_noise, ")"),
                  paste("(distributional_noise =", distributional_noise, ")"),
                  "fit"))
}
  }
}
}
}
write.csv(all_model_results, paste0(master_directory, "/model_recovery/fit_parameters/",
                                    data, "_model_recovery.csv"))

fit_models_files = list.files(path = paste0(master_directory, "/model_recovery/fit_parameters"), recursive = TRUE, pattern = 'distributional_noise=FALSE')

for (x in fit_models_files) {
all_model_results = rbind(all_model_results,read.csv(paste0(master_directory, "/model_recovery/fit_parameters/", x))[, c("AIC", "BIC", "overall_simulation_set", "simulated_model", "fit_model")])
}

all_model_results = read.csv(paste0(master_directory, "/model_recovery/fit_parameters/",
                                    data, "_model_recovery.csv"))[-1]

summarised_fits = all_model_results %>%
  #filter(fit_model != 'lin' & fit_model != 'quad' &
  #         simulated_model != 'lin' & simulated_model != 'quad' &
  #         simulated_model != 'bayes_prior' & fit_model != 'bayes_prior') %>%
  group_by(simulated_model, fit_model) %>%
  summarise(mean_AIC = mean(AIC),
            mean_BIC = mean(BIC)) %>%
  group_by(simulated_model) %>%
  mutate(min_AIC = min(mean_AIC),
         min_BIC = min(mean_BIC),
         best_model = fit_model[which.min(mean_BIC)])

all_model_results %>%
  group_by(overall_simulation_set) %>%
  mutate(best_score = min(BIC),
            best_model = unique(fit_model[BIC == best_score]),
            correct = fit_model == best_model) %>%
  group_by(simulated_model, fit_model) %>%
  summarise(simulated_model = unique(simulated_model),
    prop = sum(correct == TRUE)/n()) %>%
  ggplot(., aes(x = fit_model, y = simulated_model, fill = prop)) + geom_tile(color = "black", size = 1) + 
  geom_text(aes(x = fit_model, y = simulated_model, label = prop, col = round(prop)), size = 10) + theme_minimal() +
  scale_fill_gradientn(limits = c(0, 1), colours = c("darkblue", "yellow")) + 
  scale_colour_gradientn(limits = c(0, 1), colours = c("white", "black")) + 
  scale_x_discrete(name = "Fit Model", labels=c("Bayesian", "Bayesian \nFree Prior", "Exponent", "Fixed", "Linear", "Quadratic")) + 
  scale_y_discrete(name = "Simulated Model", labels=c("Bayesian", "Bayesian \nFree Prior", "Exponent", "Fixed", "Linear", "Quadratic")) + 
  theme(axis.text = element_text(size = 20, color = "black"), axis.title =  element_text(size = 25, color = "black"),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black")) + labs(fill = "Selection \nProbability") +
  guides(color = FALSE) # remove the guide for y
ggsave(paste0(master_directory, "/model_recovery/plots/BIC_large_recovery.pdf"), width  = 12, height = 9)



all_model_results %>%
  filter(fit_model != 'lin' & fit_model != 'quad' &
          simulated_model != 'lin' & simulated_model != 'quad' &
           simulated_model != 'bayes_prior' & fit_model != 'bayes_prior') %>%
  group_by(overall_simulation_set) %>%
  mutate(best_score = min(BIC),
         best_model = unique(fit_model[BIC == best_score]),
         correct = fit_model == best_model) %>%
  group_by(simulated_model, fit_model) %>%
  summarise(simulated_model = unique(simulated_model),
            prop = sum(correct == TRUE)/n()) %>%
  ggplot(., aes(x = fit_model, y = simulated_model, fill = prop)) + geom_tile(color = "black", size = 1) + 
  geom_text(aes(x = fit_model, y = simulated_model, label = prop, col = round(prop)), size = 10) + theme_minimal() +
  scale_fill_gradientn(limits = c(0, 1), colours = c("darkblue", "yellow")) + 
  scale_colour_gradientn(limits = c(0, 1), colours = c("white", "black")) + 
  scale_x_discrete(name = "Fit Model", labels=c("Bayesian", "Exponent", "Fixed")) + 
  scale_y_discrete(name = "Simulated Model", labels=c("Bayesian", "Exponent", "Fixed")) + 
  theme(axis.text = element_text(size = 20, color = "black"), axis.title =  element_text(size = 25, color = "black"),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black")) + labs(fill = "Selection \nProbability") +
  guides(color = FALSE) 
ggsave(paste0(master_directory, "/model_recovery/plots/BIC_small_recovery.pdf"), width  = 8, height = 6)



all_model_results %>%
  filter(fit_model != 'lin' & fit_model != 'quad' &
           simulated_model != 'lin' & simulated_model != 'quad') %>%
  group_by(overall_simulation_set) %>%
  mutate(best_score = min(AIC),
         best_model = unique(fit_model[AIC == best_score]),
         correct = fit_model == best_model) %>%
  group_by(simulated_model, fit_model) %>%
  summarise(simulated_model = unique(simulated_model),
            prop = sum(correct == TRUE)/n()) %>%
  ggplot(., aes(x = fit_model, y = simulated_model, fill = prop)) + geom_tile(color = "black", size = 1) + 
  geom_text(aes(x = fit_model, y = simulated_model, label = prop, col = round(prop)), size = 10) + theme_minimal() +
  scale_fill_gradientn(limits = c(0, 1), colours = c("darkblue", "yellow")) + 
  scale_colour_gradientn(limits = c(0, 1), colours = c("white", "black")) + 
  scale_x_discrete(name = "Fit Model", labels=c("Bayesian", "Bayesian Prior", "ES", "Unscaled ES")) + 
  scale_y_discrete(name = "Simulated Model", labels=c("Bayesian", "Bayesian Prior", "ES", "Unscaled ES")) + 
  theme(axis.text = element_text(size = 20, color = "black"), axis.title =  element_text(size = 25, color = "black"),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black")) + labs(fill = "Selection \nProbability") +
  guides(color = FALSE)


  ggplot(data = summarised_fits, aes(x = simulated_model, y = fit_model, fill = mean_BIC-min_BIC)) + geom_tile() + theme_minimal() +
    scale_fill_gradient(low = "black", high = "white")
        ggsave(paste0(master_directory, "/model_recovery/fit_parameters/",
                     data, "_model_recovery.pdf"))

