## Let's build Naive Bayes model
## We have train_test lists with training and testing sets from Step 5
 
rm(list = ls())

#required library
library(e1071)

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

#Naive Bayes for categories
x_cat = cbind(x_train_cat,y_train_cat)

nb_cat = naiveBayes(y_train_cat ~., data = x_cat)
summary(nb_cat)

#Naive Bayes for sub categories
x_sub = cbind(x_train_sub,y_train_sub)

nb_sub = svm(y_train_sub ~., data = x_sub)
summary(nb_sub)

#saving the model
save(nb_cat,nb_sub, file = 'NaiveBayes_model_training.dat')
