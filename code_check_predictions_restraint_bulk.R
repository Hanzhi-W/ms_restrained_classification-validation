library(tidyverse)
library(rstatix)
library(here)
i_am(".here")

source(here("code","project_status","check_predictions_restraint.R"))
synced_ppts <- read_csv(here("code","project_status","project_dashboard.csv")) %>% 
  filter(infant_synced_unr > 0) %>% 
  filter(!(sessions_dir %in%  c("/Volumes/padlab/study_imu_long/data/104/1",
                                "/Volumes/padlab/study_imu_long/data/185/2",
                                "/Volumes/padlab/study_imu_long/data/185/3",
                                "/Volumes/padlab/study_imu_long/data/148/1",
                                "/Volumes/padlab/study_imu_long/data/178/3",
                                "/Volumes/padlab/study_imu_long/data/132/3")))

# Compile stats

accuracy <- map2_dfr(synced_ppts$id, synced_ppts$session, ~ check_predictions_restraint(.x, .y))
accuracy$unique_id <- paste0(accuracy$id, "/", accuracy$session)

## Overall Stats
accuracy %>% get_summary_stats(`Balanced Accuracy`)
accuracy %>% get_summary_stats(Kappa)

accuracy %>% ggplot(data = ., aes(x = Kappa)) + 
  geom_histogram() + xlim(0,1)

## Correlations for time

accuracy %>% ggplot(aes(x = Prevalence, y = `Detection Prevalence`)) + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(alpha = .25, size = 2) + geom_smooth(method = "lm", se = F) + xlim(0,1) + ylim(0,1) +
   theme_minimal() + theme(legend.position = "none") +
  xlab("Actual prop. time unrestrained") + ylab("Predicted prop. time unrestrained")

accuracy %>% cor_test(Prevalence, `Detection Prevalence`)

## Correlations for num_transitions
accuracy$trans_error <- accuracy$transitions_pred - accuracy$transitions_actual
accuracy %>%
  ggplot(aes(x = transitions_actual, y = transitions_pred)) + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(alpha = .25, size = 2) + geom_smooth(method = "lm", se = F) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Actual # of transitions") + ylab("Predicted # of transitions")

accuracy %>% cor_test(transitions_actual, transitions_pred)
accuracy %>% get_summary_stats(trans_error)

accuracy %>% filter(position == "Sitting") %>% ggplot(data = ., aes(x = trans_error)) + 
  geom_histogram() 

