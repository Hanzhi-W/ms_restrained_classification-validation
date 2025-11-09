# ms_restrained_classification-validation

Part 1: Model training and validation using Leave-one-out cross-validation 

  Input data: "data_compiled" folder. It contains windowed sensors' motion features and human annotation data. Window lengths vary from 4s, 16s, to 30s.
  
  Modeling is conducted in Julia using the file "train_group_loocv_restrained.jl"
  
  Output data: Model predicted body states in every window. For each window length, each study session has two output files: "id_session_actual" (for logging human annotation) and "id_session_predict" (for model prediction). Output data is not uploaded to the repository.

Part 2: Calculate model performance

  Input data: Model predicted and human annotated body states files from the last step.
  
  Used "check_predictions_restraint_loocv.R" to calculate the performance metrics for each session. Used "check_predictions_restraint_bulk_loocv.R" to aggregate across sessions.
  
  Output data: "matrices.RData" that contains all window lengths' performance metrics

Part 3: Final model training and application to the whole data set

  Input data: 
  
  Used "train_group_restrained.jl" to get the complete prediction of the whole data set. Window length was set to be 30s.
  
  Output data: "whole-model-prediction.RData"

Part 4: Analysis and manuscript

  All the analysese and manuscript are in "manuscript_restrained_classification validation.qmd"
  
