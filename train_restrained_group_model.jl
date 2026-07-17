using CategoricalArrays, DecisionTree
using CSV, DataFrames, DataFrameMacros, Chain, Statistics, RData, JLD2

objs = RData.load("compiled_data_restrained.RData") 
ds = objs["slide_filt"]
##
with_pos = 0

if with_pos == 1
else
    pos_num = 25 #hard coded from Hanzhi code
    ds = ds[:,1:end-pos_num]
end
##
levels(ds.code)
#ds.code = levelcode.(ds.code)

ids = unique(ds.id)

y = ds[:,"code"]
X = Matrix(ds[:,Not(["code", "id"])])

model = RandomForestClassifier(n_trees=500)
DecisionTree.fit!(model,X,y)

save_object("restrained_model.jld2", model)

## For 4 s model
objs = RData.load("compiled_data_restrained_4s.RData") 
ds = objs["slide_filt"]

with_pos = 0

if with_pos == 1
else
    pos_num = 5 #hard coded 
    ds = ds[:,1:end-pos_num]
end

levels(ds.code)

ids = unique(ds.id)

y = ds[:,"code"]
X = Matrix(ds[:,Not(["code", "id"])])

model = RandomForestClassifier(n_trees=250)
DecisionTree.fit!(model,X,y)

save_object("restrained_model_4s.jld2", model)
