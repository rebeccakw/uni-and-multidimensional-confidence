rm(list = ls())
master_directory = "/Users/rebeccawest/Dropbox/Documents/multimodal_confidence"
setwd(paste0(master_directory, "/updated/Bidimensional_Confidence"))

data_size = 720
n_datasets = 10 #number of simulated datasets per model 
reps = 1
datas = c("bimodal") #c("visual", "bimodal", "cross-modal")
models = c("fixed", "exp", "bayes") #c("fixed", "lin", "quad", "exp", "bayes", "bayes_prior")
bimodal_types = c("max", "biased") #c("max", "biased") use NA for unimodal models  
diff_noise_types = c(FALSE, TRUE)
distributional_noise_types = c(FALSE)

#load libraries
library(doParallel)
library(foreach)
library(dplyr)
library(beepr)
library(tidyverse)

#set up parallel processing 
cores = 6
cl <- makeCluster(cores)
registerDoParallel(cl)

source(paste0(getwd(), "/relabel_parameters.R"))
source(paste0(getwd(), "/test_parameters.R"))
source(paste0(getwd(), "/FacetEqualWrap.R"))


#set up some data to simulate model responses for 
all_trials = data_size*n_datasets*length(models)*length(bimodal_types)*length(diff_noise_types)

confidencedata <- data.frame(matrix(vector(), all_trials, 5, 
                                    dimnames = list(c(), c("stim_orientation", "stim_frequency", "stim_reliability_level",
                                                           "r", "visial_modality"))))
confidencedata$visual_modality = 0

#label the simulated model 
confidencedata$simulated_model = rep(models, each = all_trials/length(models))

#give each dataset a number 
confidencedata$overall_simulation_set = rep(1:(all_trials/data_size), each = data_size)

#label bimodal type and different noise type within each simulated model class 
confidencedata = confidencedata %>% 
  group_by(simulated_model) %>%
  mutate(simulated_bimodal_type = rep(bimodal_types, each = data_size*n_datasets*length(diff_noise_types))) %>%
  group_by(simulated_model, simulated_bimodal_type) %>%
  mutate(simulated_diff_noise = rep(diff_noise_types, each = data_size*n_datasets)) %>%
  group_by(simulated_model, simulated_bimodal_type, simulated_diff_noise) %>%
  mutate(model_simulation_set = rep(1:n_datasets, each = data_size))

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
        for (distributional_noise in this_model_distributional_noise_types) {
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
                      "starting parameters passed"))
        } else {
          print(paste(data, class, model, 
                      paste("(bimodal_type =", bimodal_type, ")"),
                      paste("(diff_noise =", diff_noise, ")"),
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
        
        this_confidencedata = confidencedata %>%
          filter(simulated_model == model & simulated_bimodal_type == bimodal_type &
                 simulated_diff_noise == diff_noise)
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
        simulated_parameters = cbind(starting_parameters, unique(this_confidencedata$model_simulation_set), unique(this_confidencedata$overall_simulation_set),
                                     rep(model, n_datasets), rep(bimodal_type, n_datasets), rep(diff_noise, n_datasets))
        colnames(simulated_parameters) = c(parameter_names, 'model_simulation_set', 'overall_simulation_set', 'simulated_model', 
                                           'simulated_bimodal_type', 'simulated_diff_noise')
        
        if (data == 'bimodal') {
          #save the simulated data
          write.csv(simulated_data, paste0(master_directory, "/model_recovery/simulated_data/test/",
                                           data, "_", model, "_",
                                           paste0("(bimodal_type=", bimodal_type, ")"), "_",
                                           paste0("(diff_noise=", diff_noise, ")"),
                                           ".csv"))
          #save the parameters that we simulated data with
          write.csv(simulated_parameters, paste0(master_directory, "/model_recovery/simulated_parameters/test/",                                                 "/", data, "_", model, "_",
                                                 paste0("(bimodal_type=", bimodal_type, ")"), "_",
                                                 paste0("(diff_noise=", diff_noise, ")"),
                                                 "_parameters.csv"))

        } 
        }
      }
    }
  }
}
#get all csv files from model recovery directory
#files = list.files(paste0(master_directory, "/model_recovery/simulated_data"), pattern = ".csv", full.names = TRUE)
files = list.files(paste0(master_directory, "/model_recovery/simulated_data/test"), pattern = c("bimodal", ".csv"), full.names = TRUE)

#use lapply to read in each file and combine them into a list of dataframes
df_list <- lapply(files, read.csv)

#use do.call to combine all dataframes into a single dataframe
all_simulated_data <- do.call(rbind, df_list)[,-1]

#want to fit every model to every simulated dataset
#how many models are we fitting
all_model_results = data.frame()

##========= FITS MODELS TO SIMULATED DATA =================
#editing this so that we can run it iteratively 

#figure out what models we have already fit 
#use the bracket so we only get the new stuff 
already_fit = list.files(paste0(master_directory, "/model_recovery/fit_parameters/fixed"), pattern = ").csv")

#for (data in datas) {
for (model in models) {
#model = "exp"
 
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
            fit_starting_parameters = read.csv(paste0(master_directory, "/model_recovery/simulated_parameters/test/", 
                                                      data, "_", model, "_", 
                                                      paste0("(bimodal_type=", bimodal_type, ")"), "_",
                                                      paste0("(diff_noise=", diff_noise, ")"),
                                                      "_parameters.csv"))[,-1]
            
          } else {
            fit_starting_parameters = read.csv(paste0(master_directory, "/model_recovery/simulated_parameters/",class,
                                                      "/", data, "_", model, "_distributional_noise=",
                                                      distributional_noise, "_parameters.csv"))[,-1]
          }
          #remove last 2 columns (information we don't need here)
          all_starting_parameters = fit_starting_parameters[, 1:(which(colnames(fit_starting_parameters) == "model_simulation_set") - 1)]
          
          #get parameter names 
          parameter_names = relabel_parameters(all_starting_parameters[1,], class, model, bimodal_type,
                                               diff_noise, data, distributional_noise)
          
          if (class == "fixed") {
           
            start_time = Sys.time()
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
            output <- cbind(output, fit_model = model, fit_bimodal_type = bimodal_type, fit_diff_noise = diff_noise)
            
            finish_time = Sys.time()
            duration = finish_time - start_time 
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
            output <- cbind(output, fit_model = model, fit_bimodal_type = bimodal_type, fit_diff_noise = diff_noise)
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
            output <- cbind(output, fit_model = model, fit_bimodal_type = bimodal_type, fit_diff_noise = diff_noise)
          }
          if (data == 'bimodal') {
            #save the simulated data 
            write.csv(output, paste0(master_directory, "/model_recovery/fit_parameters/test/", 
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
#}
write.csv(all_model_results, paste0(master_directory, "/model_recovery/fit_parameters/",
                                    data, "exp_model_recovery.csv"))

fit_models_files = list.files(path = paste0(master_directory, "/model_recovery/fit_parameters/test/"), recursive = FALSE, pattern = c('bimodal', '.csv'))

all_model_results = data.frame()
for (x in fit_models_files) {
  this_model_results = read.csv(paste0(master_directory, "/model_recovery/fit_parameters/test/", x))[, c("log_likelihood", "AIC", "BIC", "overall_simulation_set", 
                                                                                                                           "simulated_model", "simulated_bimodal_type", "simulated_diff_noise",
                                                                                                                           "fit_model", "fit_bimodal_type", "fit_diff_noise")]
  all_model_results = rbind(all_model_results, this_model_results)
}

all_model_results = read.csv(paste0(master_directory, "/model_recovery/fit_parameters/current_recovery_files/",
                                    data, "_model_recovery.csv"))[,-1]

all_model_results$full_simulated_model <- sapply(1:nrow(all_model_results), function(row) 
       paste0(all_model_results$simulated_model[row], "_",
              all_model_results$simulated_bimodal_type[row], "_",
              all_model_results$simulated_diff_noise[row]))
all_model_results$full_fit_model <- sapply(1:nrow(all_model_results), function(row) 
  paste0(all_model_results$fit_model[row], "_",
         all_model_results$fit_bimodal_type[row], "_",
         all_model_results$fit_diff_noise[row]))


all_model_results %>%
  group_by(overall_simulation_set) %>%
  mutate(best_score = min(BIC),
         best_model = unique(fit_model[BIC == best_score]),
         correct = fit_model == best_model) %>%
  group_by(simulated_model, fit_model) %>%
  summarise(prop = sum(correct == TRUE)/n()) %>%
  ggplot(., aes(x = fit_model, y = simulated_model, fill = prop)) + geom_tile(color = "black", size = 1) + 
  geom_text(aes(x = fit_model, y = simulated_model, label = prop, col = round(prop)), size = 10) + theme_minimal() +
  scale_fill_gradientn(limits = c(0, 1), colours = c("darkblue", "yellow")) + 
  scale_colour_gradientn(limits = c(0, 1), colours = c("white", "black")) + 
  # scale_x_discrete(name = "Fit Model", labels=c("Bayesian", "Bayesian \nFree Prior", "Exponent", "Fixed", "Linear", "Quadratic")) + 
  # scale_y_discrete(name = "Simulated Model", labels=c("Bayesian", "Bayesian \nFree Prior", "Exponent", "Fixed", "Linear", "Quadratic")) + 
   theme(axis.text = element_text(size = 20, color = "black"), axis.title =  element_text(size = 25, color = "black"),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black")) + labs(fill = "Selection \nProbability") +
  guides(color = FALSE) # remove the guide for y
#width is 9.6 for large recovery and 8 for small recovery
ggsave(paste0(master_directory, "/model_recovery/plots/BIC_small_recovery.pdf"), width  = 8, height = 6)



all_model_results %>%
  group_by(overall_simulation_set) %>%
  mutate(best_score = min(BIC),
         best_model = unique(full_fit_model[BIC == best_score]),
         correct = full_fit_model == best_model) %>%
  group_by(full_simulated_model, full_fit_model) %>%
  summarise(prop = sum(correct == TRUE)/n()) %>%
  ggplot(., aes(x = full_fit_model, y = full_simulated_model, fill = prop)) + geom_tile(color = "black", size = 1) + 
  geom_text(aes(x = full_fit_model, y = full_simulated_model, label = prop, col = round(prop)), size = 6) + theme_minimal() +
  scale_fill_gradientn(limits = c(0, 1), colours = c("white", "darkblue")) + 
  scale_colour_gradientn(limits = c(0, 1), colours = c("black", "white")) + 
  # scale_x_discrete(name = "Fit Model", labels=c("Bayesian", "Bayesian \nFree Prior", "Exponent", "Fixed", "Linear", "Quadratic")) + 
  # scale_y_discrete(name = "Simulated Model", labels=c("Bayesian", "Bayesian \nFree Prior", "Exponent", "Fixed", "Linear", "Quadratic")) + 
  theme(axis.text = element_text(size = 20, color = "black"), axis.title =  element_text(size = 25, color = "black"),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black")) + labs(fill = "Selection \nProbability") +
  guides(color = FALSE) # remove the guide for y


all_model_results %>%
  group_by(overall_simulation_set) %>%
  mutate(best_score = min(BIC),
         best_model = unique(fit_model[BIC == best_score]),
         correct = fit_model == best_model) %>%
  group_by(simulated_model, fit_model) %>%
  summarise(prop = sum(correct == TRUE)/n()) %>%
  ggplot(., aes(x = fit_model, y = simulated_model, fill = prop)) + geom_tile(color = "black", size = 1) + 
  geom_text(aes(x = fit_model, y = simulated_model, label = prop, col = round(prop)), size = 10) + theme_minimal() +
  scale_fill_gradientn(limits = c(0, 1), colours = c("white", "darkblue")) + 
  scale_colour_gradientn(limits = c(0, 1), colours = c("black", "white")) + 
  # scale_x_discrete(name = "Fit Model", labels=c("Bayesian", "Bayesian \nFree Prior", "Exponent", "Fixed", "Linear", "Quadratic")) + 
  # scale_y_discrete(name = "Simulated Model", labels=c("Bayesian", "Bayesian \nFree Prior", "Exponent", "Fixed", "Linear", "Quadratic")) + 
  theme(axis.text = element_text(size = 20, color = "black"), axis.title =  element_text(size = 25, color = "black"),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black")) + labs(fill = "Selection \nProbability") +
  guides(color = FALSE) # remove the guide for y






## just if you want to look at subset of bimodal assumptions 

all_model_results$full_simulated_model <- sapply(1:nrow(all_model_results), function(row) 
  paste0(
    all_model_results$simulated_bimodal_type[row], "_",
    all_model_results$simulated_diff_noise[row]))
all_model_results$full_fit_model <- sapply(1:nrow(all_model_results), function(row) 
  paste0(
    all_model_results$fit_bimodal_type[row], "_",
    all_model_results$fit_diff_noise[row]))



best_fits <- all_model_results %>%
  group_by(overall_simulation_set) %>%
  filter(AIC == min(AIC)) %>%  # Select the model with the lowest AIC for each dataset
  ungroup() %>%
  select(overall_simulation_set, full_simulated_model, BestFitModel = full_fit_model)

conf_matrix <- best_fits %>%
  count(full_simulated_model, BestFitModel) %>% # Count occurrences
  complete(full_simulated_model, BestFitModel, fill = list(n = 0)) %>% # Fill missing combinations
  group_by(full_simulated_model) %>%
  mutate(Proportion = n / sum(n)) # Calculate proportions per true model

ggplot(conf_matrix, aes(x = BestFitModel, y = full_simulated_model, fill = Proportion)) +
  geom_tile(color = "white") + # Heatmap tiles
  geom_text(aes(label = round(Proportion, 2)), color = "black") + # Text labels
  scale_fill_gradient(low = "white", high = "steelblue") + # Color gradient
  labs(
    title = "Confusability Matrix",
    x = "Best-Fit Model",
    y = "True Model",
    fill = "Proportion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
