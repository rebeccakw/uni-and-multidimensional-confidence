rm(list = ls())

master_directory = "/Users/rebeccawest/Dropbox/Documents/multimodal_confidence"
setwd(paste0(master_directory, "/updated/Bidimensional_Confidence"))

datas = c("bimodal") #c("visual", "auditory", "bimodal")
models = c("bayes_prior") #c("fixed", "lin", "quad", "exp", "bayes", "bayes_prior")
bimodal_types = c("max") #c("max", 'biased') use NA for unimodal models  
diff_noise_types = c(TRUE, FALSE)
distributional_noise_types = c(FALSE)

n_starting_parameters = 10

#load libraries
library(doParallel)
library(foreach)
library(dplyr)
library(beepr)

#set up parallel processing 
cores = 8
cl <- makeCluster(cores)
registerDoParallel(cl)

source(paste0(getwd(), "/relabel_parameters.R"))
source(paste0(getwd(), "/test_parameters.R"))

#save the output printed to the console in text file 
#sink(paste0("model_fitting_log_", Sys.Date(), ".txt"))

for (data in datas) {
##========= LOAD DATA =================
#load data - this won't work for multiple data sets 
if (data == "visual") {
    confidencedata <- read.csv(paste0(master_directory,"/data/bimodal/visual_data.csv"))
    unimodal = TRUE
    } else if (data == "auditory") {
  confidencedata <- read.csv(paste0(master_directory,"/data/bimodal/auditory_data.csv"))
unimodal = TRUE
  } else if (data == "bimodal") {
  confidencedata <- read.csv(paste0(master_directory,"/data/bimodal/bimodal_data.csv"))
unimodal = FALSE
  }

#loop over models for dataset 
for (model in models) {
  if (data == 'visual' | data == 'auditory') {
    this_model_bimodal_types = FALSE
    this_model_diff_noise_types = FALSE
  } else if (data == 'bimodal') {
    #fit all bimodal versions if bimodal data 
    this_model_bimodal_types = bimodal_types
    this_model_diff_noise_types = diff_noise_types
  }

#loop over bimodal_types if appropriate
for (bimodal_type in this_model_bimodal_types) {
for (diff_noise in this_model_diff_noise_types) {
  for (distributional_noise in distributional_noise_types) {
#select model class 
if (model == 'lin' | model == 'quad' | model == 'exp') {
class = "linear"
} else if (model == 'bayes' | model == 'bayes_prior') {
  class = "bayesian"
} else if (model == 'fixed') {
  class = "fixed"
}
modality = data
##========= GENERATE STARTING PARAMETERS =================
#generate starting parameters  - only works with a single model right now
source(paste0(getwd(), "/generate_", class, "_parameters.R"))
 tic = Sys.time()
if (class == "fixed") {
  starting_parameters <- foreach(sub = 1:n_starting_parameters,
                                 .combine = "rbind")  %dopar% {
                                   output = generate_fixed_parameters(bimodal_type = bimodal_type,
                                                                      diff_noise = diff_noise)
                                 }
} else if (class == "linear") {
  starting_parameters <- foreach(sub = 1:n_starting_parameters,
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
  starting_parameters <-foreach(sub = 1:n_starting_parameters,
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
parameter_names = relabel_parameters(starting_parameters, class, model, bimodal_type, diff_noise, data, distributional_noise)
test = apply(starting_parameters, 1, function(x)
  test_parameters(x, class, model, bimodal_type, diff_noise, data, parameter_names, confidencedata, distributional_noise))
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
# #=======================================================================================
n_subjects = length(unique(confidencedata$subject_name))
combinations <- expand.grid(1:nrow(starting_parameters), 1:n_subjects)

##========= RUN MODEL =================
source(paste0(getwd(), "/", class, "_model.R"))
if (data == "visual" | data == "auditory") {
  unimodal = TRUE
} else {unimodal = FALSE}

tic = Sys.time()
if (class == "fixed") {
  output <- foreach(x = 1:nrow(combinations),
                    .combine = "rbind")  %dopar% {
                      output = fixed_model_function(confidencedata, combinations[x, 2], #subject_number
                                                    100,  #number of repetitions
                                                    starting_parameters[combinations[x, 1],], #starting_parameters
                                                    unimodal = unimodal, model = model,
                                                    bimodal_type = bimodal_type,
                                                    diff_noise = diff_noise,
                                                    parameter_names = parameter_names,
                                                    )
                    }
} else if (class == "linear") {
  output <- foreach(x = 1:nrow(combinations),
                    .combine = "rbind")  %dopar% {
                      output = linear_model_function(confidencedata, sub = combinations[x, 2], #subject_number
                                                     reps = 100,  #number of repetitions
                                                     starting_parameters[combinations[x, 1],],
                                                     unimodal = unimodal, model = model,
                                                     bimodal_type = bimodal_type,
                                                     diff_noise = diff_noise,
                                                     parameter_names = parameter_names,
                                                     distributional_noise = distributional_noise) #starting_parameters
                    }
} else if (class == "bayesian") {
  output <- foreach(x = 1:nrow(combinations),
                    .combine = "rbind")  %dopar% {
                      output = bayesian_model_function(confidencedata, #get data for subject
                                                       sub = combinations[x, 2], #give function subject number
                                                       reps = 100,
                                                       starting_parameters[combinations[x, 1],], #get starting parameters
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

#get best fitting parameters for each subject
best_estimate = output %>%
  group_by(subject) %>%
  summarise(index = which.min(log_likelihood),
            lowest_likelihood = log_likelihood[index])

best_fits = matrix(0, max(output$subject), ncol(output))
for (sub in unique(output$subject)) {
  best_fits[sub,] = as.numeric(output[(output$subject == sub & output$log_likelihood == best_estimate$lowest_likelihood[sub]),])
}
colnames(best_fits) <- colnames(output)
#fit the columns with parameters (as opposed to columns with subject number, log_likelihood, AIC, BIC)
parameter_index = grepl("parameter", colnames(best_fits))

## =========== Test the estimated parameters ===================
#get parameter names - just uses the first set of estimated parameters
parameter_names = relabel_parameters(as.numeric(best_fits[1, parameter_index]), class, model, bimodal_type, diff_noise, data, distributional_noise)
#test all estimated parameters by looping over rows in best_fits (selecting only the parameters)
test = apply(best_fits[, parameter_index], 1, function(x) test_parameters(x, class, model, bimodal_type, diff_noise,
                                                                          data, parameter_names, confidencedata, distributional_noise))
if (all(test == TRUE)) {
  print(paste(data, class, model,
              paste("(bimodal_type =", bimodal_type, ")"),
              paste("(diff_noise =", diff_noise, ")"),
              "estimated parameters passed"))
} else {
  print(paste(data, class, model,
              paste("(bimodal_type =", bimodal_type, ")"),
              paste("(diff_noise =", diff_noise, ")"),
              "estimated parameters did NOT pass"))
}
#======================================================================================
##============= SAVE GOOD PARAMETERS ==========================
colnames(best_fits) = c(parameter_names, "subject", "log_likelihood", "AIC", "BIC")
if (data == 'bimodal') {
write.csv(best_fits, paste0(master_directory, "/parameters/",
                            data, "_", model, "_", bimodal_type, "_",
                            diff_noise, "_",".csv"))
} else {
  write.csv(best_fits, paste0(master_directory, "/parameters/study1/",
                              data, "_", model, ".csv"))
}
best_fits = read.csv(paste0(master_directory, "/parameters/",
                             data, "_", model, "_", bimodal_type, "_",
                             diff_noise, "_",".csv"))[,-1]
parameter_names = relabel_parameters(as.numeric(best_fits[1, parameter_index]), class, model, bimodal_type, diff_noise, data, distributional_noise)
##=========== SIMULATE DATA==============
source(paste0(getwd(), "/simulate.R"))
simulated_data <- foreach(sub = 1:n_subjects,
                          .combine = "rbind")  %dopar% {
                            output = simulate(sub, confidencedata, 
                                              as.numeric(best_fits[sub, 1:length(parameter_names)]), 
                                              modality = data, 
                                              unimodal = unimodal, model = model,
                                              class = class, bimodal_type = bimodal_type,
                                              diff_noise = diff_noise,
                                              parameter_names = parameter_names,
                                              parameter_recovery = FALSE,
                                              model_recovery = FALSE,
                                              distributional_noise = distributional_noise)
                          }
if (data == 'bimodal') {
  write.csv(simulated_data, paste0(master_directory, "/simulated_data/", 
                              data, "_", model, "_", bimodal_type, "_",
                              diff_noise, "_", ".csv"))
} else {
  write.csv(simulated_data, paste0(master_directory, "/simulated_data/study1/", 
                              data, "_", model, ".csv"))
}

rm(output)
}
}
}
}
}

#save the log file 
#sink()

