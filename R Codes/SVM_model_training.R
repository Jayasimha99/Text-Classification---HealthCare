## We have train_test lists with training and testing sets from Step 5

#Now, lets build SVM model 
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

#SVM model for classifying categories
x_cat = cbind(x_train_cat,y_train_cat)

fit_cat = svm(y_train_cat ~., data = x_cat)
summary(fit_cat)

#SVM model for classifying sub categories
x_sub = cbind(x_train_sub,y_train_sub)

fit_sub = svm(y_train_sub~.,data = x_sub)
summary(fit_sub)


#saving the model
save(fit_cat,fit_sub, file = "SVM_model.dat")
