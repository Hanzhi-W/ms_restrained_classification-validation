check_predictions_restraint <- function(id, session) {
  require(tidyverse)
  require(here)
  require(cvms)
  i_am(".here")
  id <- 132
  session <- 3
  
  code_file = str_glue("/Volumes/padlab/study_imu_long/data/{id}/{session}/synced_data/restrained.csv")
  pos_file = str_glue("/Volumes/padlab/study_imu_long/data/{id}/{session}/synced_data/restrained_predictions_infant.csv")
  
  code =read_csv(code_file) %>% mutate(time_join = as.numeric(floor(seconds(time_start)))) 
  predictions = read_csv(pos_file) %>% mutate(time_join = as.numeric(floor(seconds(time_start)))) 
  
  ds <- left_join(code, predictions, by = "time_join") %>% filter(code != ".")
  
  ds$code <- factor(ds$code, levels = c("r","u"), labels = c("Restrained", "Unrestrained"))
  
  ds$pos <- factor(ds$pos)
  
  ds <- ds %>% drop_na(code, pos) 

  #u <- union(ds$pos, ds$code)
  #res <- confusion_matrix(factor(ds$code, u),factor(ds$pos, u))
  res <- confusion_matrix(ds$code,ds$pos)
  print(res$`Table`)
  print(res$`Kappa`)
  
  out <- res %>% select(`Positive Class`:`Prevalence`)

  n_bouts <- ds %>% mutate(transition_actual = as.numeric(lag(code) != code),
                           transition_pred = as.numeric(lag(pos) != pos)) %>% 
    select(code, transition_actual, pos, transition_pred)

  
  out <- out %>% 
    mutate(id = id, session = session, 
           transitions_actual = sum(n_bouts$transition_actual, na.rm = T),
           transitions_pred = sum(n_bouts$transition_pred, na.rm = T))
  
}
