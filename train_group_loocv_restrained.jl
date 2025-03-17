# Load packages (install first using Pkg_install.jl)
using CategoricalArrays, MLJBase, DecisionTree
using CSV, DataFrames, DataFrameMacros, Chain, Statistics, RData

# Load compiled data from R
objs = RData.load("data_compiled/compiled_data_restrained_4s.RData") 
ds = objs["slide_filt"]

# Pull the ids to set up the LOOCV analysis
ids = unique(ds.id)
id_len = length(ids)
print(id_len)

# Create a dataframe with the ids to store the accuracy results
metrics = DataFrame("id" => ids, "overall_acc" => -.01)
@transform!(metrics, @subset(:overall_acc  == -.01), :overall_acc = missing)

# Loop through each of the ids to fit a model
# Threads.@threads will run the loop on individual CPU cores
# Just remove it and start the line at "for" to run sequentially on a single CPU core

# Decide to include position features or not
with_pos = 1
# with_pos = 0

# Decide window size
window = 4
# window = 30

# Get position features column number
if window == 4
    pos_num = 5 # the last 5 columns are position featurs
else
    pos_num = 25
end

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

    # println("training #val: $(size(training,2))")

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

    # Calculate accuracy and store in the dataframe
    # accuracy = MLJBase.accuracy(code_predicted, code_actual) -- This code does not work: UndefVarError: `accuracy` not defined in `MLJBase`
    accuracy = sum(code_predicted .== code_actual) / length(code_actual)
    metrics[i, :overall_acc] = accuracy
    println("id: $id; accuracy: $accuracy")
end

# Save to file
CSV.write("metrics_restrained_4_pos.csv", metrics)