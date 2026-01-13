#This script is used to calculate various evidence values for the regression models 
#and the computational models. It also formats (calculates r,
#standardises stimulus values) & saves the data for each task variant.
#It should only need to be run once and then all subsequent analyses can access the saved data files. 
#RKW 16th Nov

setwd('/Users/s4323621/Dropbox/Documents/multimodal_confidence/data/bimodal/untransformed_datafiles/unstandardised')
library(mvtnorm)
library(dplyr)
## ============ Read in Data ==============
aud = read.csv("all_auditory_data.csv")
vis = read.csv("all_visual_data.csv")
bimodal = read.csv("all_bimodal_data.csv")
## ============ Bimodal Data ==============
#category means 
mus = c(0, 2700)
#category 1 covariance matrix
cat1_sigma = matrix(c(3^2, 0, 0, 125^2), 2, 2) 
#category 2 covariance matrix 
cat2_sigma = matrix(c(12^2, 0, 0, 500^2), 2, 2) 
#calculate evidence for category 1 for all stimulus values 
bimodal$cat1_evidence = dmvnorm(cbind(bimodal$stim_orientation,bimodal$stim_frequency), mus, cat1_sigma) 
#calculate evidence for category 2 for all stimulus values 
bimodal$cat2_evidence = dmvnorm(cbind(bimodal$stim_orientation,bimodal$stim_frequency), mus, cat2_sigma) 

#calculate evidence for category 2 normalised by total evidence for either category 
bimodal$evi_cat2 = (dmvnorm(cbind(bimodal$stim_orientation,bimodal$stim_frequency), mus, cat2_sigma))/
  ((dmvnorm(cbind(bimodal$stim_orientation,bimodal$stim_frequency), mus, cat1_sigma)) + 
     (dmvnorm(cbind(bimodal$stim_orientation,bimodal$stim_frequency), mus, cat2_sigma)))

#create new variable 'evi_allcat' - 'category diagnosticity'
#initalise this new variable with evidence for category 2
bimodal$evi_allcat = bimodal$cat2_evidence
#find where there is more evidence for category 1 across stimulus values and replace those values 
#with evidence for category 1 
#this 'evi_allcat' variable is now evidence for the most probably category 
bimodal$evi_allcat[bimodal$cat1_evidence > bimodal$cat2_evidence] = bimodal$cat1_evidence[bimodal$cat1_evidence > bimodal$cat2_evidence]
#normalise evidence for the most probable category by total evidence for either category
bimodal$evi_allcat = bimodal$evi_allcat/((dmvnorm(cbind(bimodal$stim_orientation,bimodal$stim_frequency), mus, cat1_sigma)) + 
                                           (dmvnorm(cbind(bimodal$stim_orientation,bimodal$stim_frequency), mus, cat2_sigma)))

## ============ Visual Data ==============
#steps repeated as above but for visual data 
vis$cat1_evidence <- dnorm(vis$stim_orientation, 0, 3)
vis$cat2_evidence <- dnorm(vis$stim_orientation, 0, 12)
vis$evi_cat2 = dnorm(vis$stim_orientation, 0, 12)/(dnorm(vis$stim_orientation, 0, 3) + dnorm(vis$stim_orientation, 0, 12))
vis$evi_allcat = vis$cat2_evidence
vis$evi_allcat[vis$cat1_evidence > vis$cat2_evidence] = vis$cat1_evidence[vis$cat1_evidence > vis$cat2_evidence]
vis$evi_allcat = vis$evi_allcat/(dnorm(vis$stim_orientation, 0, 3) + dnorm(vis$stim_orientation, 0, 12))


## ============ Auditory Data ==============
#steps repeated as above but for auditory data 
aud$cat1_evidence <- dnorm(aud$stim_frequency, 2700, 125)
aud$cat2_evidence <- dnorm(aud$stim_frequency, 2700, 500)
aud$evi_cat2 = dnorm(aud$stim_frequency, 2700, 500)/(dnorm(aud$stim_frequency, 2700, 125) + dnorm(aud$stim_frequency, 2700, 500))
aud$evi_allcat = aud$cat2_evidence
aud$evi_allcat[aud$cat1_evidence > aud$cat2_evidence] = aud$cat1_evidence[aud$cat1_evidence > aud$cat2_evidence]
aud$evi_allcat = aud$evi_allcat/(dnorm(aud$stim_frequency, 2700, 125) + dnorm(aud$stim_frequency, 2700, 500))

##===== Bimodal Data - which modality is most diagnostic?=======
#calculate evidence for category 1 using stimulus frequency only (ignoring orientation completely)
freq_cat1evidence = dnorm(bimodal$stim_frequency, 2700, 125)/(dnorm(bimodal$stim_frequency, 2700, 125) + dnorm(bimodal$stim_frequency, 2700, 500))
#calculate evidence for category 2 using stimulus frequency only (ignoring orientation completely)
freq_cat2evidence = dnorm(bimodal$stim_frequency, 2700, 500)/(dnorm(bimodal$stim_frequency, 2700, 125) + dnorm(bimodal$stim_frequency, 2700, 500))
#record the most evidence for either category
freq_mostevidence = apply(cbind(freq_cat1evidence, freq_cat2evidence), 1, function(x) max(x))

#as above 
ori_cat1evidence = dnorm(bimodal$stim_orientation, 0, 3)/(dnorm(bimodal$stim_orientation, 0, 3) + dnorm(bimodal$stim_orientation, 0, 12))
ori_cat2evidence = dnorm(bimodal$stim_orientation, 0, 12)/(dnorm(bimodal$stim_orientation, 0, 3) + dnorm(bimodal$stim_orientation, 0, 12))
ori_mostevidence = apply(cbind(ori_cat1evidence, ori_cat2evidence), 1, function(x) max(x))

#determine which modality has the most evidence for one of the categories 
#returns either 0 (visual has the most evidence) or 1 (audition has the most evidence)
bimodal$visual_mostevidence = apply(cbind(ori_mostevidence, freq_mostevidence), 1, function(x) (which.max(x) - 1))

#===== Reformatting ==============
vis$visual_mostevidence = 0
aud$visual_mostevidence = 0
#combine all data 
all_data <- rbind(vis, aud, bimodal)

#change subject names 
#all_data$subject_name <- as.factor(all_data$subject_name)
#all_data$subject_name <- as.numeric(all_data$subject_name)

#calculate r 
all_data$r[all_data$resp_category == 1] <- 5 - all_data$resp_confidence[all_data$resp_category == 1]
all_data$r[all_data$resp_category == 2] <- all_data$resp_confidence[all_data$resp_category == 2] + 4

#===== Standardise Data ==============
#select and standardise data within each task 
#could group_by() but easier to follow when saving as 
#separate variables 
visual <- all_data %>%
  filter(stim_type == 'grate') %>%
  mutate(stim_orientation = scale(stim_orientation))

auditory <- all_data %>%
  filter(stim_type == 'audio') %>%
  mutate(stim_frequency = scale(stim_frequency))

bimodal <- all_data %>%
  filter(stim_type == 'bimodal') %>%
  mutate(stim_orientation = scale(stim_orientation),
         stim_frequency = scale(stim_frequency))

#save the data 
setwd('/Users/s4323621/Dropbox/Documents/multimodal_confidence/data/bimodal')
write.csv(visual, 'visual_data.csv')
write.csv(auditory, 'auditory_data.csv')
write.csv(bimodal, 'bimodal_data.csv')

all_data <- rbind(visual, auditory, bimodal)
write.csv(all_data, 'all_data.csv')
