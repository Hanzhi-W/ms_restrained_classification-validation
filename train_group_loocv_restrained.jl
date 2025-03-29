# Load packages (install first using Pkg_install.jl)
using CategoricalArrays, MLJBase, DecisionTree, StatisticalMeasures, Random
using CSV, DataFrames, DataFrameMacros, Chain, Statistics, RData

############# BEFORE RUNNING DIFFERENT BATCHES, MODIFY:
# Load compiled data from R
objs = RData.load("data_compiled/compiled_data_restrained_4s.RData") 
# objs = RData.load("data_compiled/compiled_data_restrained.RData") 
ds = objs["slide_filt"]

# Decide which folder to store individual predictions
# subfolder = "predict_30_pos"
# subfolder = "predict_30_no_pos"
# subfolder = "predict_4_pos"
subfolder = "predict_4_no_pos"

# Decide to include position features or not. 1=include, 0=not include
with_pos = 0

# Current window size. 30s or 4s.
window = 4

# File name for saving matrix
# save_file = "metrics_restrained_30_pos.csv"
# save_file = "metrics_restrained_30_no_pos.csv"
# save_file = "metrics_restrained_4_pos.csv"
save_file = "metrics_restrained_4_no_pos.csv"
####################################################

# Set seed for random forest
Random.seed!(2025)

# Delete pilot study, subject 104
ds = filter!(row -> row.id != "104/1", ds) 

# Pull the ids to set up the LOOCV analysis
ids = unique(ds.id)
id_len = length(ids)
print(id_len)

# Create a dataframe with the ids to store the accuracy results
metrics = DataFrame("id" => ids, "overall_acc" => -.01, "overall_sens" => -.01, "overall_spec" => -.01, "overall_posi_pred" => -.01)
@transform!(metrics, @subset(:overall_acc  == -.01), :overall_acc = missing, :overall_sens = missing, :overall_spec = missing, :overall_posi_pred = missing,)

# Get # column of position features
if window == 30
    pos_num = 25 # the last 25 columns are position featurs
else
    pos_num = 5
end

# Loop through each of the ids to fit a model
# Threads.@threads will run the loop on individual CPU cores
# Just remove it and start the line at "for" to run sequentially on a single CPU core
Threads.@threads for i in eachindex(ids)
    id = ids[i]
    println("Fitting $id")

    # Partition the data into training (everyone but current id) and testing (current id)
    training = @subset(ds, :id != id)
    testing =  @subset(ds, :id == id)
    
    # Decide to include position features or not
    if with_pos == 1
    else
        training = training[:,1:end-pos_num]
        testing = testing[:,1:end-pos_num]
    end

    println("training #val: $(size(training,2))")

    # Make sure there are no rows with missing data
    training = dropmissing(training)
    testing = dropmissing(testing)

    # Create a matrix with the code (y) and the features (X)
    y = training[:,"code"]
    X = Matrix(training[:,Not(["code", "id"])])

    # Fit a random forest classifier. 
    # N_trees has a big influence on runtime; 150 is a reasonable estimate, but models tend to work better with 500
    model = RandomForestClassifier(n_trees=150)
    DecisionTree.fit!(model,X,y)

    # Predict testing data codes from resulting model
    code_predicted = MLJBase.coerce(DecisionTree.predict(model, Matrix(testing[:,Not(["code", "id"])])), Multiclass) 
    code_actual = MLJBase.coerce(testing[:,"code"], Multiclass)

    # save individual predict file
    CSV.write(subfolder*"/"*replace(id, "/" => "_")*"_predict.csv", code_predicted)
    CSV.write(subfolder*"/"*replace(id, "/" => "_")*"_actual.csv",code_actual)

    # Calculate accuracy and store in the dataframe
    # accuracy = MLJBase.accuracy(code_predicted, code_actual) -- This code does not work: UndefVarError: `accuracy` not defined in `MLJBase`
    # accuracy = sum(code_predicted .== code_actual) / length(code_actual)
    accuracy = StatisticalMeasures.accuracy(code_predicted, code_actual)
    sensitivity = StatisticalMeasures.sensitivity(code_predicted, code_actual)
    specificity = StatisticalMeasures.specificity(code_predicted, code_actual)
    positive_predict = StatisticalMeasures.positive_predictive_value(code_predicted, code_actual)

    metrics[i, :overall_acc] = accuracy
    metrics[i, :overall_sens] = sensitivity
    metrics[i, :overall_spec] = specificity
    metrics[i, :overall_posi_pred] = positive_predict

    println("id: $id; accuracy: $accuracy")
end

# Save to file
CSV.write(save_file, metrics)