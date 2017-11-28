## We have train_test lists with training and testing sets from Step 5

#Now, lets evaluate NaiveBayes model 
rm(list = ls())

#required library
library(e1071)
library(caret)

#loading train_test list for classifying categories and sub categories

load('Fifth_step.dat')
load('NaiveBayes_model_training.dat')

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

#NaiveBayes evaluation for categories
nb_pred_cat = predict(nb_cat,x_test_cat)
summary(nb_pred_cat)

confusionMatrix(nb_pred_cat,y_test_cat)

#NaiveBayes evaluation for sub categories
nb_pred_sub = predict(nb_sub,x_test_sub)
summary(nb_pred_sub)

y = confusionMatrix(nb_pred_sub,y_test_sub)
y$overall
