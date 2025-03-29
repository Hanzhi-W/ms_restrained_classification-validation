# ms_restrained_classification-validation
Project: IMUL restrained

Part 1: Analysis of restrained ML classification validation

--Input data: compiled data from lab server
  
--ML code: "train_group_loocv_restrained.jl"
  
--Output ML prediction: 
    (1) "metrics_xxx.csv" files. Note, "sensitivity", "specificity", "positive prediction value" were calculated wrong in julia code. They should be subtracted from 1. I modified the values in R code.
    (2) detailed prediction & actual annotation for each session -- uploaded to lab server
 
--Analysis code: "code_compare_models.R"
  
--Output figures: in "figure" folder.

Part 2: Manuscript
  "manuscript_restrained_classification validation.Rmd"
