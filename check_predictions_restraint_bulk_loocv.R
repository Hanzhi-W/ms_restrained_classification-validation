# Calculate performance matrices for all models across sessions
# Called "check_predictions_restraint_loocv.R" and data on the server
library(tidyverse)
library(rstatix)
library(here)
library(cvms)
library(janitor)
i_am(".here")


source(here("code","project_status","check_predictions_restraint_loocv.R"))
synced_ppts <- read_csv(here("code","project_status","project_dashboard.csv")) %>% 
  filter(infant_synced_unr > 0) %>% 
  filter(!(sessions_dir %in%  c("/Volumes/padlab/study_imu_long/data/104/1",
                                "/Volumes/padlab/study_imu_long/data/185/2",
                                "/Volumes/padlab/study_imu_long/data/185/3")))
# "/Volumes/padlab/study_imu_long/data/148/1",
# "/Volumes/padlab/study_imu_long/data/178/3")))
# "/Volumes/padlab/study_imu_long/data/132/3")))

# Calculate performance matrices
matrix_30 <- 
  map2_dfr(synced_ppts$id, synced_ppts$session, ~ check_predictions_restraint_loocv(model = "30_no_pos", .x, .y))
matrix_16 <- 
  map2_dfr(synced_ppts$id, synced_ppts$session, ~ check_predictions_restraint_loocv(model = "16_no_pos", .x, .y))
matrix_4 <- 
  map2_dfr(synced_ppts$id, synced_ppts$session, ~ check_predictions_restraint_loocv(model = "4_no_pos", .x, .y))

save(matrix_4, matrix_16, matrix_30, file = "matrices.RData")