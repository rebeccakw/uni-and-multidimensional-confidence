#Create some figures baby 
library(tidyverse)
library(ggplot2)
library(stringr)

#set working directory 
master_directory = "/Users/rebeccawest/Dropbox/Documents/multimodal_confidence"

##============== CREATE MODEL PREDICTION PLOTS ==============
data_directory = paste0(master_directory, "/simulated_data/")

#get all files in the data directory 
files = list.files(data_directory)
files = files[-which(files == "study1")]
files = files[-which(files == "bimodal_lin_max_FALSE_.csv")]

#if you just want a subset of the files 
#files = files[grepl('visual', files)]
modalities = c("auditory", "visual", "bimodal")

#source the plotting functions 
source(paste0(master_directory, "/updated/Bidimensional_Confidence/plotting_functions/all_intensity.R"))
source(paste0(master_directory, "/updated/Bidimensional_Confidence/plotting_functions/seperate_intensity.R"))
source(paste0(master_directory, "/updated/Bidimensional_Confidence/plotting_functions/average_response_plots.R"))

for (f in files) {
  simulated_data = read.csv(paste0(data_directory, f))[, -1]
  simulated_data$category = -1
  simulated_data$category[simulated_data$resp_category == 2] <- 1
  simulated_data$resp = simulated_data$category*simulated_data$resp_confidence
  
  modality = modalities[str_detect(f, modalities)]
   if (modality == "bimodal") {
    #calculate an average of orientation and frequency values 
     average_value = sapply(1:nrow(simulated_data), 
        function(x) mean(simulated_data$stim_orientation[x], simulated_data$stim_frequency[x]))
 #save the average value in a new data frame, labelled as stim orientation
  edited_simulated_data = data.frame(select(simulated_data, -stim_orientation), stim_orientation = average_value)
  #create figure 
  gradient_plots(edited_simulated_data, master_directory, f) 
  }  else {
    #create figure 
     gradient_plots(simulated_data, master_directory, f)
   }
  
  #make other plots 
  #average_response_plots(simulated_data, master_directory, f)
  intensity_plots(simulated_data, master_directory, f)
}

##============== CREATE MODEL COMPARISON PLOTS ==============
model_fits = setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("subject", "AIC", "BIC", "model"))
parameter_files = list.files(paste0(master_directory, "/parameters/"))
 
parameter_files = parameter_files[-c(which(parameter_files == "study1"),
                                     which(parameter_files == "other"),
                which(str_detect(parameter_files, "integrated")),
                which(str_detect(parameter_files, "cross_modal_comparison")))]
model_classes = c("fixed", "linear", "linear", "linear", "bayes", "bayes")
model_types = c("fixed", "lin", "quad", "exp", "bayes", "bayes_prior")

#if you just want a subset of the files 
#parameter_files = parameter_files[grepl('visual', parameter_files)]

#extract subject, AIC, BIC for each model 
for (file in parameter_files) {
  best_fits = read.csv(paste0(master_directory, "/parameters/", file))[,-1]
  this_model_type = sapply(model_types, function(x) grepl(x, file, fixed = TRUE))
  this_model_class = model_classes[this_model_type]
  if (grepl("bimodal", file, fixed = TRUE)) {
    bimodal_type = c("max", "biased")[which(sapply(c("max", "biased"), function(x) grepl(x, file, fixed = TRUE)))]
    diff_noise = c("TRUE", "FALSE")[which(sapply(c("TRUE", "FALSE"), function(x) grepl(x, file, fixed = TRUE)))]
  } else {
    bimodal_type = "NA"
    diff_noise = "NA"
  }
this_data = cbind(subject = best_fits$subject, 
                  AIC = best_fits$AIC, 
                  BIC = best_fits$BIC,
                  #data.frame(model = rep(substr(file, 1, unlist(gregexpr(".csv", file))-1), nrow(best_fits))),
                  data.frame(model = model_types[max(which(sapply(model_types, function(x) grepl(x, file, fixed = TRUE))))]),
                  data.frame(class = this_model_class),
                  data.frame(bimodal_type = bimodal_type),
                  data.frame(diff_noise = diff_noise),
                  data.frame(modality = names(which(sapply(c("visual", "auditory", "bimodal"), 
                                                           function(x) grepl(x, file, fixed = TRUE))))))
model_fits = rbind(model_fits, this_data)
}

#make AIC and BIC plots 
group_sums = model_fits %>%
  group_by(modality, class, model, bimodal_type, diff_noise) %>%
  summarise(aic = sum(AIC),
            sd_aic = sd(AIC),
            bic = sum(BIC),
            sd_bic = sd(BIC))
participant_sums = model_fits %>%
  group_by(subject, modality) %>%
  filter(!model %in% c("lin", "quad")) %>%
  summarise(winning_model = model[which.min(AIC)])

best_models_aic = group_sums %>%
  group_by(modality, class) %>%
  slice_min(aic) %>%
  select(-bic) %>%
  rename(value = aic, sd = sd_aic) %>%
  mutate(metric_type = "AIC")
best_models_bic = group_sums %>%
  group_by(modality, class) %>%
  slice_min(bic) %>%
  select(-aic) %>%
  rename(value = bic, sd = sd_bic) %>%
  mutate(metric_type = "BIC")
best_models = rbind(best_models_aic, best_models_bic)
best_models$value = best_models$value - 15000
  ggplot(best_models, aes(x = class, y = value, fill = metric_type)) + 
    geom_bar(stat = "identity",position=position_dodge(width = 0.8)) +
    geom_errorbar(aes(ymin = value-100, ymax = value + sd, color = metric_type),
                      position=position_dodge(width=0.8), width = 0.5) +
  scale_fill_manual(values=c("black", "burlywood3")) +   scale_color_manual(values=c("black", "burlywood3")) +
    theme_minimal() + facet_wrap(~factor(modality, levels = c("visual", "auditory", "bimodal")), scale ="free_y") + 
    theme(panel.spacing = unit(1, "lines"), axis.text = element_text(size = 20, color = "black"),
          legend.position = "nonw")

  ggsave("/Users/rebeccawest/Dropbox/Documents/multimodal_confidence/figures/model_comparsion.pdf",
         width = 13, height= 3.4)

  
model_fits %>%
    filter(!model %in% c("lin", "quad")) %>%
    group_by(subject, modality, class) %>%
    summarise(winning_AIC_per_class = (AIC[which.min(AIC)])) %>%
    group_by(subject, modality) %>%
    mutate(w_AIC = exp(-(0.5*(winning_AIC_per_class - min(winning_AIC_per_class))))/ sum(exp(-(0.5*(winning_AIC_per_class - min(winning_AIC_per_class)))))) %>%
    #class = class[which.min(AIC)]) %>%
    mutate(class = fct_relevel(class, c("fixed", "bayes", "linear"))) %>%
    ggplot(., aes(x = class, y = as.factor(subject), fill = w_AIC)) + 
    geom_tile() + facet_wrap(~modality, scale = "free_x", labeller = as_labeller(c(auditory = "Auditory", bimodal = "Bimodal", visual = "Visual"))) + 
    scale_fill_viridis_c(option = "D", direction = -1, name = "Akaike Weights") +
    theme_minimal() + theme(axis.text = element_text(size = 20, colour = "black"),
                            axis.title = element_text(size = 20, colour = "black"),
                            axis.text.x = element_text(angle = 90, hjust = 0.6, vjust = 0.5),
                            legend.text = element_text(size = 20, colour = "black"),
                            strip.text = element_text(size = 20, colour = "black"),
                            legend.title = element_text(size = 20, colour = "black")) + 
    xlab("Model Class") + ylab("Participant") + 
    scale_x_discrete(labels = c("Unscaled \n\ ES", "Bayesian", "ES"))
  ggsave("/Users/rebeccawest/Dropbox/Documents/multimodal_confidence/figures/model_weights.pdf")

  #IS IT POSSIBLE THAT I WANT TO DO THIS BY CLASS INSTEAD
model_fits %>%
  filter(!model %in% c("lin", "quad")) %>%
  group_by(subject, modality) %>%
  mutate(w_AIC = exp(-(0.5*(AIC - min(AIC))))/ sum(exp(-(0.5*(AIC - min(AIC))))),
         model_no = 1:n()) %>%
ggplot(., aes(x = model_no, y = as.factor(subject), fill = w_AIC)) + 
  geom_tile() + facet_wrap(~modality, scale = "free_x", labeller = as_labeller(c(auditory = "Auditory", bimodal = "Bimodal", visual = "Visual"))) + 
  scale_fill_viridis_c(option = "B", direction = -1, name = "AIC") +
  theme_minimal() + theme(axis.text = element_text(size = 20, colour = "black"),
                          axis.title = element_text(size = 20, colour = "black"),
                          axis.text.x = element_text(angle = 35, hjust = 0.3, vjust = 0.5),
                          legend.text = element_text(size = 20, colour = "black"),
                          strip.text = element_text(size = 20, colour = "black"),
                          legend.title = element_text(size = 20, colour = "black")) + xlab("Model Class") + ylab("Participant")

model_fits %>%
  filter(modality == "bimodal") %>%
  group_by(subject) %>%
  mutate(w_AIC = exp(-(0.5*(AIC - min(AIC))))/ sum(exp(-(0.5*(AIC - min(AIC))))),
         model_no = paste0(model,bimodal_type, diff_noise)) %>%
  ggplot(., aes(x = model_no, y = as.factor(subject), fill = w_AIC)) + 
  geom_tile() + scale_fill_viridis_c(option = "D", direction = -1, name = "Akaike Weights") +
  theme_minimal() + theme(axis.text = element_text(size = 20, colour = "black"),
                          axis.title = element_text(size = 20, colour = "black"),
                          axis.text.x = element_text(angle = 90, hjust = 0.3, vjust = 0.5),
                          legend.text = element_text(size = 20, colour = "black"),
                          strip.text = element_text(size = 20, colour = "black"),
                          legend.title = element_text(size = 20, colour = "black")) + xlab("Model Class") + ylab("Participant")
ggsave("/Users/rebeccawest/Dropbox/Documents/multimodal_confidence/figures/bimodal_model_weights.pdf")

tmp <- model_fits %>%
  filter(modality == "bimodal") %>%
  group_by(subject) %>%
  mutate(w_AIC = exp(-(0.5*(AIC - min(AIC))))/ sum(exp(-(0.5*(AIC - min(AIC)))))) %>%
  group_by(subject, class) %>%
  mutate(min_value = max(w_AIC)) %>%
  filter(w_AIC == min_value) 

  

#checking performance 
data = read.csv("/Users/s4323621/Dropbox/Documents/multimodal_confidence/data/bimodal/all_data.csv")
data %>%
  group_by(subject_name, stim_type, stim_reliability_level) %>%
  summarise(m = mean(resp_correct)) %>%
  ggplot(., aes(x = stim_reliability_level, y = m)) + geom_point() + facet_wrap(~subject_name+stim_type)


data %>%
  group_by(stim_type, stim_reliability_level) %>%
  summarise(m = mean(resp_correct)) %>%
  ggplot(., aes(x = stim_reliability_level, y = m)) + geom_point() + facet_wrap(~stim_type)


#get the best fitting model for each subject 
best_per_subject <- model_fits %>%
  group_by(subject, modality) %>%
  slice_min(AIC)
  
