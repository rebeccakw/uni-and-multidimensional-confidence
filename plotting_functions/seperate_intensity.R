#=============================
gradient_plots <- function(simulated_data, master_directory, file_name) {
  simulated_data$resp_buttonid[simulated_data$resp_category == 1] <- -simulated_data$resp_confidence[simulated_data$resp_category == 1]
  simulated_data$resp_buttonid[simulated_data$resp_category == 2] <- simulated_data$resp_confidence[simulated_data$resp_category == 2]
  
# SIMULATED DATA 
#order data by intensity level 
simulated_data <- simulated_data[order(simulated_data$stim_reliability_level),]

#calculate the number of trials per intensity level 
trials_per_reliability = nrow(simulated_data)/length(unique(simulated_data$stim_reliability_level))

#loop over the levels of intensity and order data by psychological_orientation for that level of intensity 
for (x in seq(trials_per_reliability,nrow(simulated_data),trials_per_reliability)) {
  simulated_data[(x-(trials_per_reliability-1)):x,] <- simulated_data[(x-(trials_per_reliability-1)):x,] %>%
    arrange(stim_orientation)
}

#create bin, bin_size and order variables 
simulated_data$bin <- matrix(0, nrow(simulated_data), 1)
simulated_data$bin_size <- matrix(0, nrow(simulated_data), 1)
simulated_data$order <- 1:nrow(simulated_data)

#calculate the number of trials per bin for plots (here it works out to be 72 trials per data point)
n_per_bin = nrow(simulated_data)/40

#move through the ordered data selecting blocks of 72 trials and calculate mean orientation 
# (and record bin size - even though this should always be 72)
for (x in seq(n_per_bin,nrow(simulated_data),n_per_bin)) {
  simulated_data[(x-(n_per_bin-1)):x,] <- simulated_data %>%
    filter(order >= (x-(n_per_bin-1)) & order <= x) %>%
    mutate(bin = mean(stim_orientation),
           bin_size = n())
}

#check that there is no data where bin_size is 0
simulated_data <- simulated_data %>%
  filter(bin_size != 0)


binned_data <- simulated_data %>%
  group_by(subject_name, stim_reliability_level, bin) %>%
  summarise(conf_sim = mean(psychological_confidence),
            conf_real = mean(resp_confidence),
            cat_resp_sim = mean(psychological_category - 1),
            cat_resp_real = mean(resp_category - 1)) %>%
  group_by(stim_reliability_level, bin) %>%
  summarise(m_sim = mean(conf_sim),
            m_sim_sd = sd(conf_sim)/sqrt(10),
            m_real = mean(conf_real),
            m_real_sd = sd(conf_real)/sqrt(10),
            cat_sim = mean(cat_resp_sim - 1),
            cat_real = mean(cat_resp_real - 1))

colnames(binned_data)[1] <- "Intensity"
#all_rels <- all_rels %>%
#  filter(Intensity == 2 | Intensity == 4)
colnames(binned_data)
#=====================
ggplot(binned_data) + geom_ribbon(aes(x = bin, ymin = m_sim - m_sim_sd, ymax = m_sim + m_sim_sd), alpha = 0.07) +
  geom_line(aes(bin, m_sim, color = cat_sim), size = 4) + 
  #geom_point(aes(bin, m_sim, color = cat_sim), size = 5) +
  #geom_point(aes(bin, m_sim), shape = 1,size = 5, colour = "black") +
  geom_point(aes(bin, m_real, fill = cat_real), shape = 21, color = "black", size = 3) + 
  geom_errorbar(aes(x = bin, ymin = m_real - m_real_sd, ymax = m_real + m_real_sd), width = 0.1) +
  #geom_line(aes(bin, m_real, colour = cat_real), size = 1)  + 
  facet_wrap(~ Intensity, ncol = 4, labeller = label_both) + theme_minimal() + 
  theme(axis.title=element_text(size=25), axis.text=element_text(size=25, color = "black"), 
        strip.text = element_text(size=25), legend.text = element_text(size=25),
        legend.title = element_text(size=25), legend.position = "none") + 
  xlab("Standardised Stimulus Value") + ylab("Confidence") +
  xlim(-2.1,2.1) + ylim(1,4) + geom_vline(xintercept = 0, linetype = 'dotted') + 
  scale_color_gradient(name="Prop. Cat 2 Responses", low="red", high="yellow") +
scale_fill_gradient(name="Prop. Cat 2 Responses", low="red", high="yellow")
#find which model we are fitting based on file name
#ggsave(paste0(master_directory, '/figures/gradient_plots/bimodal_from_unimodal_predictions_adapted_withpsychometricfunction_weighted.pdf'), width = 11, height = 3)
ggsave(paste0(master_directory, '/figures/gradient_plots/', gsub(".csv", ".pdf", file_name)), width = 11, height = 3)

subset_data <- binned_data %>%
  filter(Intensity == 1 | Intensity == 4)
ggplot() + 
  geom_ribbon(data = subset_data[subset_data$Intensity == 1,], 
              aes(bin, ymin = m_sim - m_sim_sd, ymax = m_sim + m_sim_sd), alpha = 0.07) + 
  geom_ribbon(data = subset_data[subset_data$Intensity == 4,], 
              aes(bin,  ymin = m_sim - m_sim_sd, ymax = m_sim + m_sim_sd), alpha = 0.07) + 
  geom_line(data = subset_data[subset_data$Intensity == 1,], aes(bin, m_sim, color = cat_sim), size = 3.5) + 
  geom_line(data = subset_data[subset_data$Intensity == 4,], aes(bin, m_sim, color = cat_sim), size = 3.5) + 
  #geom_point(aes(bin, m_sim, color = cat_sim), size = 5) +
  #geom_point(aes(bin, m_sim), shape = 1,size = 5, colour = "black") +
  geom_errorbar(data = subset_data[subset_data$Intensity == 1,], 
                aes(x = bin, ymin = m_real - m_real_sd, ymax = m_real + m_real_sd)) +
  geom_errorbar(data = subset_data[subset_data$Intensity == 4,], 
                aes(x = bin, ymin = m_real - m_real_sd, ymax = m_real + m_real_sd)) + 
  geom_point(data = subset_data[subset_data$Intensity == 1,], aes(bin, m_real, fill = cat_real), shape = 21, color = "black", size = 3) +
  geom_point(data = subset_data[subset_data$Intensity == 4,], aes(bin, m_real, fill = cat_real), shape = 24, color = "black", size = 3) + 
   #geom_line(aes(bin, m_real, colour = cat_real), size = 1)  + 
  theme_minimal() + 
  theme(axis.title=element_text(size=25), axis.text=element_text(size=25, color = "black"), 
        strip.text = element_text(size=25), legend.text = element_text(size=25),
        legend.title = element_text(size=25), legend.position = "none") + 
  xlab("Standardised Stimulus Value") + ylab("Confidence") +
  xlim(-2.1,2.1) + ylim(1,4) + geom_vline(xintercept = 0, linetype = 'dotted') + 
  scale_color_gradient(name="Prop. Cat 2 Responses", low="red", high="yellow") +
  scale_fill_gradient(name="Prop. Cat 2 Responses", low="red", high="yellow")
#find which model we are fitting based on file name
ggsave(paste0(master_directory, '/figures/gradient_plots_subset/', gsub(".csv", ".pdf", file_name)), width = 4.5, height = 3)


}

