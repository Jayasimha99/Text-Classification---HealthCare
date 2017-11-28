## We have train_test lists with training and testing sets from Step 5

#Now, lets evaluate Random Forest model 
rm(list = ls())

#required library
library(h2o)
library(caret)

#loading train_test list for classifying categories and sub categories

load('Fifth_step.dat')
load('RF_model_training.dat')

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

a = cbind(x_test_cat,y_test_cat)
b = cbind(x_test_sub,y_test_sub)

#loading testing data to cluster

h_test_cat = as.h2o(a)
h_test_sub = as.h2o(b)

#evaluation of RF for categories
pred_cat <- as.data.frame(h2o.predict(rf_model_cat, h_test_cat)) 
summary(pred_cat)

confusionMatrix(pred_cat$predict,y_test_cat)

#evaluation of RF model for sub categories

pred_sub <- as.data.frame(h2o.predict(rf_model_sub, h_test_sub))
summary(pred_sub)

confusionMatrix(pred_sub$predict,y_test_sub)

#INCASE OF ANY ERROR, Please open RF_model_training.R and uncomment the commands from line 71 to 92
#and run you will get the output

# shuting down h2o cluster
h2o.shutdown(prompt = F)
# Thank you!