# Calculate performance matrix for each model on each session based on loocv predictions
check_predictions_restraint_loocv <- function(model, id, session) {
  require(tidyverse)
  require(here)
  require(cvms)
  i_am(".here")

  code_file = str_glue("/Volumes/padlab/study_imu_long/restraint/1 validation/model prediction/predict_{model}/{id}_{session}_actual.csv") #human coding after filtering out NAs
  pos_file = str_glue("/Volumes/padlab/study_imu_long/restraint/1 validation/model prediction/predict_{model}/{id}_{session}_predict.csv") #loocv prediction. both files should have same length
  
  rm(code,predictions)
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
  
  res_u <- confusion_matrix(ds$code, ds$pos, positive="u")
  res_r <- confusion_matrix(ds$code, ds$pos, positive="r")

  out <- rbind(res_u, res_r) %>% 
    select(`Positive Class`:`Prevalence`) %>% 
    mutate(model = model, id = id, session = session) %>% 
    select(model, id, session, everything())
  
  return(out)
}
