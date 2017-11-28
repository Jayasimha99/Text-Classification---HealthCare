## We have train_test lists with training and testing sets from Step 5

#Now, lets build Random Forest model 
rm(list = ls())

#required library
library(h2o)
library(caret)

#loading train_test list for classifying categories and sub categories

load('Fifth_step.dat')

#Unpacking the train and test sets for clasifying categories

x_train_cat = train_test_cat[[1]]
x_test_cat = train_test_cat[[2]]
y_train_cat = train_test_cat[[3]]
y_test_cat = train_test_cat[[4]]
rm(train_test_cat)

#Unpacking the train and test sets for clasifying sub categories

x_train_sub = train_test_sub[[1]]
x_test_sub = train_test_sub[[2]]
y_train_sub = train_test_sub[[3]]
y_test_sub = train_test_sub[[4]]
rm(train_test_sub)

## Create an H2O cloud 
h2o.init(
        nthreads=-1,            ## -1: use all available threads
        max_mem_size = "2G")    ## specify the memory size for the H2O cloud
h2o.removeAll() # Clean slate - just in case the cluster was already running

#check h2o cluster status
h2o.init()

set.seed(123)

#Random Forest for categories and sub categories
p = cbind(x_train_cat,y_train_cat)
q = cbind(x_train_sub,y_train_sub)


# loading data to h2o clusters
h_train_cat = as.h2o(p)
h_train_sub = as.h2o(q)

# creating predictor and target indices
x_cat = 1:ncol(p)-1
y_cat = ncol(p)

#creating predictor and target indices
x_sub = 1:ncol(q)-1
y_sub = ncol(q)

# Building random forest model for categories
rf_model_cat = h2o.randomForest(x=x_cat, y=y_cat, training_frame = h_train_cat, ntrees = 1000)

# Building random forest model for sub categories
rf_model_sub = h2o.randomForest(x=x_sub, y=y_sub, training_frame = h_train_sub, ntrees = 1000)

#saving the model
save(rf_model_cat,rf_model_sub, file = 'RF_model_training.dat')

##In Evaluation, After loading RF_model_training.dat model may throw an error because model must be
##stored in h2o cluster. Saving in h2o cluster is also not working for me.In that case just convert 
## below comments to commands and run.

#set.seed(123)

#a = cbind(x_test_cat,y_test_cat)
#b = cbind(x_test_sub,y_test_sub)

#h_test_cat = as.h2o(a)
#h_test_sub = as.h2o(b)

#pred_cat <- as.data.frame(h2o.predict(rf_model_cat, h_test_cat))
#summary(pred_cat)

#confusionMatrix(pred_cat$predict,y_test_cat)


#pred_sub <- as.data.frame(h2o.predict(rf_model_sub, h_test_sub))
#summary(pred_sub)

#confusionMatrix(pred_sub$predict,y_test_sub)

### shuting down h2o cluster

#h2o.shutdown(prompt = F)






