average_response_plots <- function(simulated_data, master_directory, file_name) {
simulated_data$category = -1
simulated_data$category[simulated_data$resp_category == 2] <- 1
simulated_data$resp = simulated_data$category*simulated_data$resp_confidence

simulated_data %>%
  group_by(stim_category, stim_reliability_level) %>%
  summarise(model = mean(psychological_r),
            data = mean(resp),
            model_sd = sd(psychological_r)/max(subject_name),
            data_sd = sd(resp)/max(subject_name)) %>%
  ggplot(.) + geom_point(aes(x = stim_reliability_level, y = data, col = factor(stim_category)), size = 4.5) + 
  geom_errorbar(aes(x = stim_reliability_level, ymin = data - data_sd, ymax = data + data_sd, col = factor(stim_category)), width = 0.1) +
  geom_line(aes(x = stim_reliability_level, y = model, col = factor(stim_category), size = model_sd), alpha = 0.7) +
  theme_minimal() + scale_colour_manual(values = c("red", "darkblue")) +  
  theme(axis.text=element_text(size=22,  color = "black"), strip.text = element_text(size = 30), 
        legend.position = "right", legend.text = element_text(size = 30, color = "black"), legend.title = element_text(size = 30),
        axis.title=element_text(size=30,  color = "black")) + theme(legend.position = "none") +
  xlab("Stimulus Intensity Level") + ylab("Mean Response") + ylim(-4, 4) + geom_hline(yintercept = 0, linetype = "dashed", size = 1)

#save figure
ggsave(paste0(master_directory, '/updated/figures/reliability_plots/', gsub(".csv", ".pdf", file_name)), width = 2340, height = 1650, units = "px")

}
