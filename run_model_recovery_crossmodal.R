#I've rewritten this so that it will only work for the cross-modal models
#to hard to integrate with other code
rm(list = ls())

master_directory = "/Users/rebeccawest/Dropbox/Documents/multimodal_confidence"
setwd(paste0(master_directory, "/updated/Bidimensional_Confidence"))

data_size = 720*2 #this per data set in each modality, so needs to  be at minimum 720*2
n_datasets = 100 #number of simulated datasets per model 
reps = 100
datas = c("cross-modal") #c("visual", "bimodal", "cross-modal")
models = c("noise", "flexible", "common") #c("common", "flexible", "offset", "scaler", "noise")

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
source(paste0(getwd(), "/FacetEqualWrap.R"))


#set up some data to simulate model responses for 
confidencedata <- data.frame(matrix(vector(), data_size*n_datasets*length(models), 4, 
                                    dimnames = list(c(), c("stim_orientation", "stim_frequency", "stim_reliability_level",
                                                           "r"))))
#easier not to use this 
confidencedata$stim_frequency = 0 

#label the simulated model 
confidencedata$simulated_model = rep(models, each = data_size*n_datasets)

#simulation sets get labelled within models 
confidencedata$model_simulation_set = rep(rep(1:n_datasets, each = data_size), length(models))

#simulation sets get labelled across models 
confidencedata$overall_simulation_set = rep(1:(n_datasets*length(models)), each = data_size)

  ##========= SIMULATE EXPERIMENTAL DATA =================
    for (set in 1:max(confidencedata$overall_simulation_set)) {
      #sample equally form each reliability level
      confidencedata$stim_reliability_level[confidencedata$overall_simulation_set == set] = rep(1:4, data_size/4)
      
      #first half is visual data and second half is auditory data 
      confidencedata$visual_modality = rep(1:0, each = data_size/2)
      
      #randomly sample from the standardised category distributions
      confidencedata$stim_orientation[confidencedata$overall_simulation_set == set] = c(rnorm(data_size/4, 0, 0.35), 
                                                                                        rnorm(data_size/4, 0, 1.37),
                                                                                          rnorm(data_size/4, 0, 0.35), 
                                                                                          rnorm(data_size/4, 0, 1.37))
    }

 
  #loop over models for dataset 
  for (model in models) {
    cond = 0
    while (cond < Inf) {
      if (model != 'flexible') {
        this_confidencedata = confidencedata %>%
        filter(simulated_model == model)
      } else if (model == 'flexible') {
        this_confidencedata = confidencedata %>%
          filter(simulated_model == model & visual_modality == cond)
      }
    
    #get revelevant data for this model 
    
    if (model == "common" | model == "offset" | model == "scaler" | model == 'flexible') {
     diff_noise = FALSE
    } else if (model == 'noise') {
    diff_noise = TRUE 
    }
    
        #select model class 
        class = "linear"
        unimodal = "cross-modal"
        bimodal_type = "NaN"
        data = datas
        modality = data
        ##========= GENERATE STARTING PARAMETERS =================
        #load in the relevant functions for this model
        source(paste0(master_directory, "/updated/Bidimensional_Confidence", "/generate_", class, "_parameters.R"))
        source(paste0(master_directory, "/updated/Bidimensional_Confidence", "/", class, "_model.R"))
        
        #sample the starting parameters 
          #generate more starting parameters than we need because this is pretty
          #quick and fails often
          starting_parameters <- foreach(sub = 1:(n_datasets+10),
                                         .combine = "rbind")  %dopar% {
                                           output = generate_linear_parameters(model = model, 
                                                                               modality = modality,
                                                                               bimodal_type = "NaN",
                                                                               diff_noise = diff_noise)
                                         }
          
        #get starting parameters that aren't NA
        starting_parameters = starting_parameters[which(!is.na(starting_parameters[,1])),]
        ## =========== Test the chosen starting parameters ===================
        parameter_names = relabel_parameters(starting_parameters[1,], class, model, bimodal_type,
                                             diff_noise, data)
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
                                                      class = class, bimodal_type = "NaN",
                                                      diff_noise = diff_noise,
                                                      parameter_names = parameter_names, 
                                                      parameter_recovery = FALSE, model_recovery = TRUE)
                                    
                                  }
        simulated_parameters = cbind(starting_parameters,1:nrow(starting_parameters),
                                     rep(model, nrow(starting_parameters)))
        colnames(simulated_parameters) = c(parameter_names, 'simulation_set', 'simulated_model')
        
          #save the simulated data
          write.csv(simulated_data, paste0(master_directory, "/model_recovery/cross-modal/simulated_data/temp/", 
                                           data, "_", model, "_modality=", cond, ".csv"))
          
          #save the parameters we simulated data with 
          write.csv(simulated_parameters, paste0(master_directory, "/model_recovery/cross-modal/simulated_parameters/temp/",
                                                 "/", data, "_", model, "_modality=", cond, "_parameters.csv"))
          print(model)
          
          if (model == 'flexible' & cond == 0) {
            cond = cond + 1
          } else if (cond == 1 || model != 'flexible') {
            cond = Inf
          }
    }
  }

#get all csv files from model recovery directory
files = list.files(paste0(master_directory, "/model_recovery/cross-modal/simulated_data/temp"), pattern = ".csv", full.names = TRUE)

#use lapply to read in each file and combine them into a list of dataframes
df_list <- lapply(files, read.csv)

#use do.call to combine all dataframes into a single dataframe
all_simulated_data <- do.call(rbind, df_list)[,-1]

#want to fit every model to every simulated dataset
#how many models are we fitting
all_model_results = data.frame()

##========= FITS MODELS TO SIMULATED DATA =================
  for (model in models) {
    cond = 0
    while (cond < Inf) {
        #load the starting parameters that we used to simulate data
        fit_starting_parameters = read.csv(paste0(master_directory, "/model_recovery/cross-modal/simulated_parameters/temp",
                                           "/", data, "_", model, "_modality=", cond, "_parameters.csv"))[,-1]
          
        #remove last 2 columns (information we don't need here)
        all_starting_parameters = fit_starting_parameters[, -((ncol(fit_starting_parameters)-1):ncol(fit_starting_parameters))]
        
        if (model == "common" | model == "offset" | model == "scaler" | model == 'flexible') {
          diff_noise = FALSE
        } else if (model == 'noise') {
          diff_noise = TRUE 
        }
        
        if (model != 'flexible') {
          this_simulated_data = all_simulated_data
        } else if (model == 'flexible') {
          this_simulated_data = all_simulated_data %>%
            filter(visual_modality == cond)
        }
        
        #get parameter names 
        parameter_names = relabel_parameters(all_starting_parameters[1,], class, model, bimodal_type,
                                             diff_noise, data)
        
        
          output <- foreach(x = 1:max(this_simulated_data$overall_simulation_set),
                            .combine = "rbind")  %dopar% {
                              output = linear_model_function(this_simulated_data[this_simulated_data$overall_simulation_set == x,], 
                                                             1:n_datasets, #subject_number
                                                             reps = reps,  #number of repetitions
                                                             #hacky but ensures we use the generating parameters if simulated model == model we are fitting 
                                                             as.numeric(all_starting_parameters[unique(this_simulated_data$model_simulation_set[this_simulated_data$overall_simulation_set == x]),]), 
                                                             unimodal = unimodal, model = model, 
                                                             bimodal_type = bimodal_type,
                                                             diff_noise = diff_noise,
                                                             parameter_names = parameter_names, recovery = TRUE) 
                            }
          output <- cbind(output, fit_model = model, modality = cond)
         #save the parameters we simulated data with 
          write.csv(output, paste0(master_directory, "/model_recovery/cross-modal/fit_parameters/temp/", 
                                   data, "_", model, "_modality=", cond, "_parameters.csv"))

        #also get what we need so we can make the comparison
        this_model_results = output[, c("AIC", "BIC", "overall_simulation_set", "simulated_model", "fit_model", "modality")]
        all_model_results = rbind(all_model_results, this_model_results)
        
        print(paste(data, class, model, 
                    paste("(bimodal_type =", bimodal_type, ")"),
                    paste("(diff_noise =", diff_noise, ")"),
                    "fit"))
        
        if (model == 'flexible' & cond == 0) {
          cond = cond + 1
        } else if (cond == 1 || model != 'flexible') {
          cond = Inf
        }
    }
  }

all_model_results <- read.csv(paste0(master_directory, "/model_recovery/fit_parameters/",
                                     data, "_model_recovery3.csv"))

combined_flexible = all_model_results %>%
  filter(fit_model == 'flexible') %>%
  group_by(overall_simulation_set) %>%
  summarise(AIC = sum(AIC),
            BIC = sum(BIC),
            simulated_model = unique(simulated_model),
            fit_model = unique(fit_model))
combined_all = rbind(all_model_results[all_model_results$fit_model != 'flexible', 
                  c("overall_simulation_set", "AIC", "BIC", "simulated_model", "fit_model")],
      combined_flexible)

#write.csv(all_model_results, paste0(master_directory, "/model_recovery/fit_parameters/",
#                                    data, "_model_recovery3.csv"))

combined_all %>%
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
  scale_x_discrete(name = "Fit Model", labels=c("Noise", "Flexible", "Common")) + 
  scale_y_discrete(name = "Simulated Model", labels=c("Noise", "Flexible", "Common")) + 
  theme(axis.text = element_text(size = 20, color = "black"), axis.title =  element_text(size = 25, color = "black"),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black")) + labs(fill = "Selection \nProbability") +
  guides(color = FALSE) 
ggsave(paste0(master_directory, "/model_recovery/cross-modal/plots/BIC_recovery.pdf"), width  = 8, height = 6)


isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
}

##get parameter recovery on this data 
for (model in models) {
  if (model != "flexible") {
  sim_parameters = read.csv(paste0(master_directory, "/model_recovery/cross-modal/simulated_parameters/temp/cross-modal_", model, "_modality=", 0, "_parameters.csv"))[,-1]
  fit_parameters = read.csv(paste0(master_directory, "/model_recovery/cross-modal/fit_parameters/temp/cross-modal_", model, "_modality=", 0, "_parameters.csv"))[,-1]
  } else if (model == "flexible") {
    #relabel
    vis_names = c("k1_vis", "k2_vis", "k3_vis", "k4_vis", "k5_vis", "k6_vis", "k7_vis",
                  "m1_vis", "m2_vis", "m3_vis", "m4_vis", "m5_vis", "m6_vis", "m7_vis",
                  "sigma1_vis", "sigma2_vis", "sigma3_vis", "sigma4_vis", "exp_vis", "simulation_set", "simulated_model", "fit_model")
    aud_names = c("k1_aud", "k2_aud", "k3_aud", "k4_aud", "k5_aud", "k6_aud", "k7_aud",
                  "m1_aud", "m2_aud", "m3_aud", "m4_aud", "m5_aud", "m6_aud", "m7_aud",
                  "sigma1_aud", "sigma2_aud", "sigma3_aud", "sigma4_aud", "exp_aud")
  
  #read in simulated parameters   
  aud_parameters = read.csv(paste0(master_directory, "/model_recovery/cross-modal/simulated_parameters/temp/cross-modal_", model, "_modality=", 0, "_parameters.csv"))[,-c(1, 21, 22)]
  vis_parameters = read.csv(paste0(master_directory, "/model_recovery/cross-modal/simulated_parameters/temp/cross-modal_", model, "_modality=", 1, "_parameters.csv"))[,-1]
  colnames(vis_parameters) = vis_names[-22]
  colnames(aud_parameters) = aud_names
  sim_parameters = cbind(aud_parameters, vis_parameters)
 
  #read in fit parameters 
  aud_parameters = read.csv(paste0(master_directory, "/model_recovery/cross-modal/fit_parameters/temp/cross-modal_", model, "_modality=", 0, "_parameters.csv"))[,-c(1, 21:28)]
 vis_parameters = read.csv(paste0(master_directory, "/model_recovery/cross-modal/fit_parameters/temp/cross-modal_", model, "_modality=", 1, "_parameters.csv"))[,-c(1,21:24,28)]
 fit_parameters = cbind(aud_parameters, vis_parameters)
  }
  fit_parameters = fit_parameters[fit_parameters$fit_model == model & fit_parameters$simulated_model == model,]
  n_parameters = sum(grepl('parameter', colnames(fit_parameters)))
  colnames(fit_parameters)[1:n_parameters] = colnames(sim_parameters)[1:n_parameters]
  recovery = cbind(pivot_longer(fit_parameters, 1:n_parameters, values_to = "fit_value")[, c("name", "fit_value")],
                   pivot_longer(sim_parameters, 1:n_parameters, values_to = "sim_value")[, c("sim_value")])
  recovery %>%
    group_by(name) %>%
    mutate(non_outlier = isnt_out_z(fit_value)) %>%
    filter(non_outlier == TRUE) %>%
  ggplot(data =.) + geom_point(aes(sim_value, fit_value)) +
    facet_wrap_equal(~name, scales = "free", ncol = 6) +
    geom_abline(slope = 1, intercept = 0, alpha = 0.7, lty = 2) + xlab("Generating Parameter") +
    ylab("Recovered Parameter") + theme(strip.text.x = element_text(size = 20), axis.text=element_text(size=14),
                                        axis.title=element_text(size=25))
  height = (2.5*(ceiling(n_parameters/3)))/2
  ggsave(paste0(master_directory, "/model_recovery/cross-modal/parameter_recovery_plots/", model, ".pdf"), width = 9.72*2, height = height)

  test = recovery %>%
    group_by(name) %>%
    mutate(non_outlier = isnt_out_z(fit_value)) %>%
    filter(non_outlier == TRUE) %>%
    group_by(name) %>%
    summarise(correlation = cor(fit_value, sim_value))
  write.csv(test, paste0(master_directory, "/model_recovery/cross-modal/parameter_recovery_plots/correlations/", model, ".csv"))
  
}

