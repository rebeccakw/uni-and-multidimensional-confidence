intensity_plots <- function(simulated_data, master_directory, file_name) {
simulated_data$resp_buttonid[simulated_data$resp_category == 1] <- -simulated_data$resp_confidence[simulated_data$resp_category == 1]
simulated_data$resp_buttonid[simulated_data$resp_category == 2] <- simulated_data$resp_confidence[simulated_data$resp_category == 2]

library(dplyr)
bins_per_rel = 15
trials_per_bin = (nrow(simulated_data)/4)/bins_per_rel
bin_no = nrow(simulated_data)/trials_per_bin


sorted <- simulated_data %>%
  group_by(stim_reliability_level) %>%
  arrange(stim_orientation, .by_group = TRUE)

sorted$order <- rep(1:bin_no, each = trials_per_bin)

summaries <- sorted %>%
  group_by(stim_reliability_level, order) %>%
  summarise(bin = mean(stim_orientation),
            mean_category = mean(resp_category - 1),
            mean_confidence = mean(resp_confidence),
            mean_resp = mean(resp_buttonid),
            model_category = mean(psychological_category - 1),
            model_confidence = mean(psychological_confidence),
            model_resp = mean(psychological_r),
            #model_resp = mean(simulated_resp_buttonid),
            no = n(),
            std_error = sd(resp_buttonid)/sqrt(8))

ggplot(summaries) + geom_line(aes(bin, model_resp, color = factor(stim_reliability_level)), size = 4) + 
    #geom_point(aes(bin, model_resp, color = factor(stim_reliability_level)), size = 6) +
    #geom_point(aes(bin, model_resp), shape = 1,size = 6, colour = "black") +
    geom_point(aes(bin, mean_resp, colour = factor(stim_reliability_level)), size = 4) + 
    geom_point(aes(bin, mean_resp),shape = 1, size = 4, colour = "black") + theme_minimal() + theme(axis.title=element_text(size=30, colour = "black")) +  
    theme(axis.text=element_text(size=30, colour = "black"), strip.text = element_text(size = 30, colour = "black"), 
          legend.position = "top", legend.text = element_text(size = 30), legend.title = element_text(size = 30)) + 
    xlab("Orientation") + ylab("Response") + #ylim(-4,4) +
    viridis::scale_color_viridis(discrete = TRUE, begin = 0.95, end = 0) +
    labs(colour="Stimulus Intensity") + xlim(-2.5, 2.5) + ylim(-4, 4) 

#find which model we are fitting based on file name
end_name  = unlist(gregexpr(".csv", file_name))
model_name = substr(file_name, 1, end_name-1)
ggsave(paste0(master_directory, "/model_prediction_figures/", model_name, ".pdf"), width = 2000, height = 1450, units = "px")
}
