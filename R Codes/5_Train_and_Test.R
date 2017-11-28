### we are going to form a master dataset and divide it into train and test sets

rm(list = ls())

##initializing setup
libs = c("data.table","plyr","dplyr","caret","tm","SnowballC","ggplot2","wordcloud","caTools")
lapply(libs, require, character.only = TRUE)
rm(libs)

load('First_step.dat')
load('Second_step.dat')
load('Third_step.dat')
load('Fourth_step.dat')

#function to draw sample datasets and test&train sets from the sample (stratified sampling technique)

library(caTools)
sampling <- function(master_data, set_seed = 123, samp.ratio= 0.075, train.ratio= 0.75){
        set.seed(seed = set_seed) #sets the seed
        samp_split = sample.split(master_data[,ncol(master_data)], samp.ratio)
        sample = subset(master_data, samp_split == T) #sample from master
        
        # training and testing 
        smpl = sample.split(sample[,ncol(sample)], train.ratio)
        x_train = subset(sample, smpl == T ) #training set
        x_test  = subset(sample, smpl == F ) #testing set
        y_train = x_train[,ncol(x_train)]
        y_test = x_test[,ncol(x_test)]
        x_train[,ncol(x_train)] = NULL
        x_test[,ncol(x_test)] = NULL
        train_test = list(x_train,x_test,y_train,y_test)
        return(train_test)
}

#Train and Test sets from the sample with seed = 555 to classify categories

train_test_cat = sampling(master_data_cat,555)

#Train and Test sets from the sample with seed = 555 to classify sub categories

train_test_sub = sampling(master_data_sub,555)

save(train_test_cat,train_test_sub, file = 'Fifth_step.dat')
