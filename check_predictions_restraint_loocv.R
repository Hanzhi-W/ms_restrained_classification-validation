# modified to examine LOOCV predictions
check_predictions_restraint_loocv <- function(id, session) {
  require(tidyverse)
  require(here)
  require(cvms)
  i_am(".here")
  # id <- 132
  # session <- 3
  
  code_file = str_glue("/Volumes/padlab/study_imu_long/restraint/1 validation/model prediction/predict_30_no_pos/{id}_{session}_actual.csv") #human coding after filtering out NAs
  pos_file = str_glue("/Volumes/padlab/study_imu_long/restraint/1 validation/model prediction/predict_30_no_pos/{id}_{session}_predict.csv") #loocv prediction. both files should have same length
  
  code =read_csv(code_file)
  ref1 <- str_match(code$pool[1],"\\[\\\"(.*?)\\\"")[,2]
  code <- code %>% 
    mutate(code = ifelse((ref1=="Unrestrained" & ref == 1 | ref1=="Restrained" & ref == 2), "u","r"))
  
  predictions = read_csv(pos_file)
  ref2 <- str_match(predictions$pool[1],"\\[\\\"(.*?)\\\"")[,2]
  predictions <- predictions %>% 
    mutate(pos = ifelse((ref2=="Unrestrained" & ref == 1 | ref2=="Restrained" & ref == 2), "u","r"))
  
  ds <- data.frame(code = code$code, pos = predictions$pos) %>% 
    mutate(code = factor(code, levels=c("u","r")),
           pos = factor(pos, levels=c("u","r")))
  

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
