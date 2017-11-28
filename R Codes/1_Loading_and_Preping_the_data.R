###We are going to load and prep the data in this section.

##Clearing the environment.

rm(list = ls())

##Setting the working directory.

getwd()
setwd("C:/Users/janne/Desktop/Edwisor/HealthCare Classification/Text Classification - HealthCare")
list.files()

###initializing the setup

##loading required libraries
libs = c("data.table","plyr","dplyr","qdap")
lapply(libs, require, character.only = TRUE)
rm(libs)

##set options

options(stringsAsFactors = FALSE)

##reading the data

given_data = fread("TextClassification_Data.csv")
dim(given_data);str(given_data)
summary(given_data)

##Preping the data as needed

#from the str(given_data), we need to convert some columns as factors.

given_data$categories = as.factor(given_data$categories)
given_data$sub_categories = as.factor(given_data$sub_categories)
given_data$previous_appointment = as.factor(given_data$previous_appointment)
str(given_data)

##converting DATA column to text

#the DATA column in dataset is in RTF format. It might be garbage but let's just 
#try to decode the column which might come in handy.

#below operations take more system time for data.table.so converting to just data frame.
given_data = as.data.frame(given_data)

#removing words starting with punctuation or special characters
given_data[,3] = gsub("[[:punct:]]\\w+ *",'',given_data[,3])
given_data[,3] = gsub("[[:punct:]]",'',given_data[,3]) #removing punctuation 
given_data[,3] = gsub("x\\w+ *",'',given_data[,3]) #removing repeated xxx 
#given_data =  given_data[1:100,] #took first 100 rows for testing purpose 
#given_data[,3] = stemmer(given_data[,3]) #played, plays to play 
given_data[,3] = replace_contraction(given_data[,3]) #isn't to is not 
given_data[,3] = replace_abbreviation(given_data[,3],abbreviation = qdapDictionaries::abbreviations,
                                      replace = NULL, ignore.case = TRUE) #Sr. to Senior 

head(given_data$DATA)

#converting back to data.table for faster operations
given_data = data.table(given_data)
class(given_data)

##ignoring fileid, ID variables as they are unique for every patient.
data_req = select(given_data,SUMMARY:previous_appointment)
dim(data_req);rm(given_data) #removing given_data, as we don't need it.

#removing the noice in the dataset
table(data_req$categories);table(data_req$sub_categories);table(data_req$previous_appointment)

#from the above table output, we can see there are multiple factors because of case sence. So removing such noice
data_req$categories =  mapvalues(data_req$categories,from = c("asK_A_DOCTOR",
                        "mISCELLANEOUS","JUNK"),to = c("ASK_A_DOCTOR","MISCELLANEOUS","MISCELLANEOUS"))

data_req$sub_categories = mapvalues(data_req$sub_categories, 
                        from = c("mEDICATION RELATED","JUNK"),to = c("MEDICATION RELATED","OTHERS"))

data_req$previous_appointment = mapvalues(data_req$previous_appointment,
                                from = c("","No","NO","yes","Yes","YES"), to = c(NA,0,0,1,1,1))

table(data_req$categories);table(data_req$sub_categories);table(data_req$previous_appointment)

#any missing values?? Finding any missing values and imputing them
table(is.na(data_req))
sapply(data_req,function(x)sum(is.na(x)))
which(is.na(data_req$previous_appointment))
data_req$previous_appointment[c(14129,16570)] = 0 #imputing the missing values with 0 because >90% are 0
summary(data_req)

#saving the required things for step two.
save(data_req, file = 'First_step.dat')




