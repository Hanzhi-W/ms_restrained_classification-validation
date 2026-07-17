This project is about measuring infants' daily restrained time using wearable sensors.

Step 1: Compile the motion features with human annotations

This step is conducted on the server. We used R scripts (i.e., "compile_data_restrained_4s.R", "compile_data_restrained_16s.R", "compile_data_restrained.R") to compile the motion features and synchronized human annotations for each session.

Scripts are shared, but the raw data are too big to share. Please contact the author if you want to get the raw data.

Output data are in the "data_compiled" folder. They take on different formats:

First, because window lengths vary from 4s, 16s, to 30s, different window lengths have different compiled data files, separately.

Second, we included different features in different compiled files, including:

-the compiled files that only contain windows with 90% clean code (i.e., the infant remained in a single status for over 90% of the window time). There are "compiled_data_restrained_4s.RData","compiled_data_restrained_16s.RData","compiled_data_restrained_30s.RData".

-"_keepall" files are the ones with all windows, regardless of the proportion of time a certain code takes up.

-"_withtime" files are the ones with start time stamp for each window, specifically for bout-level validation.

Step 2: Model training and validation using leave-one-out cross-validation in julia

Input data: compiled data from the "data_compiled" folder. It contains windowed sensors' motion features and synchronized human annotations. Note, we only kept the windows with 90% clean code.

LOOCV was conducted in Julia using the file "train_group_loocv_restrained.jl"

Output data: Each study session has two output files: "id_session_actual" (for logging human annotation) and "id_session_predict" (for model prediction). Each model has its own folder. For example, "predict_4_no_pos". The output data are not shared (but can be easily generated on your own) due to its big size.

Step 3: Calculate model performance

Input data: Individual files of predictions and codes for each session from the last step.

The model performance was calculated on the server. We used the "check_predictions_restraint_loocv.R" script to calculate performance metrics for a single session and used "check_predictions_restraint_bulk_loocv.R" to aggregate across sessions. Open the "bulk" one directly and it will call the function.

The scripts are shared, but the input data are not shared (but can be easily generated on your own) from the last step. But you can also save the trouble, because we shared the output data.

Output data: "matrices.RData" for 4s, 16s, 30s models.

Step 4: Feature importance analysis

We used "feature_importance.jl" script, which is basically the same as loocv script but has extra lines to keep the feature importance and save the data.

Input data: compiled data

Output data: feature importance lists for each loocv iteration. csv files are saved in folders "feature_importance_4s", "feature_importance_16s", "feature_importance_30s"

Analysis: see the appendix part at the manuscript quarto file "manuscript_restrained_classification validation.qmd"

Step 5: Final model predictions

After validating the modeling process, we used all 1.5-hour human codes as ground truth labels to predict infant restraint for all the sensor data we collected.

The code for training the final model is "train_restrained_group_model.jl".

The windowed human codes and final model predictions (30s and 4s) for each session is saved on the server in each individual session's folder. The session-by-session predictions are not shared, but we combined all the sessions into "whole-model-prediction.RData" and "whole-model-4s-prediction.RData".

The whole-model-prediction is then loaded for empirical analyses, instead of reading them from the server again.

Part 5: Bout-level performance evaluation

We want to see if the model performs well in the bout level (windowed). The script to check the reliability is "check_bout_reliability_restraint.R". The script is shared, but the input data (session-by-session predictions from the last step) are not.

The output is the timeline data with bout labeled. Including "combined_codes_predictions_4s_delete_e.RData", "combined_codes_predictions_4s_keep_e.RData", "combined_codes_predictions_30s_delete_e.RData", "combined_codes_predictions_30s_keep_e.RData".

The analysis code for bout-level performance is not included in the manuscript quarto file, but in a following project.
