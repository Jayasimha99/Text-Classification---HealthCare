### we are going to study the distribution of words using the dataset created 
rm(list = ls())
##initializing setup
libs = c("data.table","plyr","dplyr","caret","tm","SnowballC","ggplot2","wordcloud")
lapply(libs, require, character.only = TRUE)
rm(libs)
##Best way to explore a dataset is to analize the wordclouds.

load('Second_step.dat')
load('First_step.dat')
#forming a dataset with frquencies in descending order
word_freq <- sort(colSums(combined_data), decreasing = T)
word_freq <- data.table(Terms = names(word_freq), frequency = word_freq)
head(word_freq)

# creating word cloud for top 200 words in frequency
#png("WholeData_Wordcloud.png",width = 480,height = 480)
wordcloud(word_freq$Terms, word_freq$frequency,max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()
#most frequent words in the dataset
most_freq <- word_freq[frequency>50000]
most_freq

# Bar graph for words that are more frequent appearing more than 50000 times
#png("Top_5_most_frequent_terms.png")
ggplot(most_freq, aes(Terms, frequency))+
        geom_bar(stat = 'identity', colour = "grey", fill = 'grey')+
        labs(title= 'High frequent Terms')
#dev.off()
#subsetting the combined data category wise and cleaning the corresponding corpuses
categories = data_req$categories
combined_data_cat = cbind(combined_data,categories)

#Subsetting the datsets by category

appoinments = combined_data_cat[(combined_data_cat$categories) == "APPOINTMENTS"]
ask_a_doctor = combined_data_cat[(combined_data_cat$categories) == "ASK_A_DOCTOR"]
miscellaneous = combined_data_cat[(combined_data_cat$categories) == "MISCELLANEOUS"]
lab = combined_data_cat[(combined_data_cat$categories) == "LAB"]
prescription = combined_data_cat[(combined_data_cat$categories) == "PRESCRIPTION"]

#Wordclouds of terms in combined_data, category wise 

#png("Appointments.png")
wordcloud(names(appoinments),numcolwise(sum)(appoinments),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Ask_a_doctor.png")
wordcloud(names(ask_a_doctor),numcolwise(sum)(ask_a_doctor),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Miscellaneous.png")
wordcloud(names(miscellaneous),numcolwise(sum)(miscellaneous),max.words = 200, scale =c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Lap.png")
wordcloud(names(lab),numcolwise(sum)(lab),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Prescription.png")
wordcloud(names(prescription),numcolwise(sum)(prescription),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

rm(appoinments,ask_a_doctor,miscellaneous,lab,prescription)

## Exploring the dataset category wise.

#subsetting the dataset category wise and cleaning the corresponding corpuses

#Subsetting the datsets by category

app_df = data_req[(data_req$categories) == "APPOINTMENTS"]
ask_df = data_req[(data_req$categories) == "ASK_A_DOCTOR"]
mis_df = data_req[(data_req$categories) == "MISCELLANEOUS"]
lab_df = data_req[(data_req$categories) == "LAB"]
pre_df = data_req[(data_req$categories) == "PRESCRIPTION"]

#Cleaning the SUMMARY and DATA columns for each category dataset

appoinments_corpus_SUMMARY = clean_corpus(app_df$SUMMARY)
appoinments_corpus_DATA = clean_corpus(app_df$DATA)
ask_a_doctor_corpus_SUMMARY = clean_corpus(ask_df$SUMMARY)
ask_a_doctor_corpus_DATA = clean_corpus(ask_df$DATA)
miscellaneous_corpus_SUMMARY = clean_corpus(mis_df$SUMMARY)
miscellaneous_corpus_DATA = clean_corpus(mis_df$DATA)
lab_corpus_SUMMARY = clean_corpus(lab_df$SUMMARY)
lab_corpus_DATA = clean_corpus(lab_df$DATA)
prescription_corpus_SUMMARY = clean_corpus(pre_df$SUMMARY)
prescription_corpus_DATA = clean_corpus(pre_df$DATA)

rm(app_df,ask_df,mis_df,lab_df,pre_df)

#generating respective Document Term Matrices for each category

appoinments_dtm_SUMMARY = DocumentTermMatrix(appoinments_corpus_SUMMARY)
appoinments_dtm_SUMMARY = removeSparseTerms(appoinments_dtm_SUMMARY,0.999)
appoinments_dtm_SUMMARY

appoinments_dtm_DATA = DocumentTermMatrix(appoinments_corpus_DATA)
appoinments_dtm_DATA = removeSparseTerms(appoinments_dtm_DATA,0.985)
appoinments_dtm_DATA

ask_a_doctor_dtm_SUMMARY = DocumentTermMatrix(ask_a_doctor_corpus_SUMMARY)
ask_a_doctor_dtm_SUMMARY = removeSparseTerms(ask_a_doctor_dtm_SUMMARY,0.999)
ask_a_doctor_dtm_SUMMARY

ask_a_doctor_dtm_DATA = DocumentTermMatrix(ask_a_doctor_corpus_DATA)
ask_a_doctor_dtm_DATA = removeSparseTerms(ask_a_doctor_dtm_DATA,0.985)
ask_a_doctor_dtm_DATA

miscellaneous_dtm_SUMMARY = DocumentTermMatrix(miscellaneous_corpus_SUMMARY)
miscellaneous_dtm_SUMMARY = removeSparseTerms(miscellaneous_dtm_SUMMARY,0.999)
miscellaneous_dtm_SUMMARY

miscellaneous_dtm_DATA = DocumentTermMatrix(miscellaneous_corpus_DATA)
miscellaneous_dtm_DATA = removeSparseTerms(miscellaneous_dtm_DATA,0.985)
miscellaneous_dtm_DATA

lab_dtm_SUMMARY = DocumentTermMatrix(lab_corpus_SUMMARY)
lab_dtm_SUMMARY = removeSparseTerms(lab_dtm_SUMMARY,0.999)
lab_dtm_SUMMARY

lab_dtm_DATA = DocumentTermMatrix(lab_corpus_DATA)
lab_dtm_DATA = removeSparseTerms(lab_dtm_DATA,0.985)
lab_dtm_DATA

prescription_dtm_SUMMARY = DocumentTermMatrix(prescription_corpus_SUMMARY)
prescription_dtm_SUMMARY = removeSparseTerms(prescription_dtm_SUMMARY,0.999)
prescription_dtm_SUMMARY

prescription_dtm_DATA = DocumentTermMatrix(prescription_corpus_DATA)
prescription_dtm_DATA = removeSparseTerms(prescription_dtm_DATA,0.985)
prescription_dtm_DATA

#Forming datatables with terms and frequencies category wise

app_freq_SUMMARY = sort(colSums(data.table(as.matrix(appoinments_dtm_SUMMARY))), decreasing = T)
app_freq_SUMMARY = data.table(Terms = names(app_freq_SUMMARY), frequency = app_freq_SUMMARY)
rm(appoinments_corpus_SUMMARY,appoinments_dtm_SUMMARY)
head(app_freq_SUMMARY)

ask_freq_SUMMARY = sort(colSums(data.table(as.matrix(ask_a_doctor_dtm_SUMMARY))), decreasing = T)
ask_freq_SUMMARY = data.table(Terms = names(ask_freq_SUMMARY), frequency = ask_freq_SUMMARY)
rm(ask_a_doctor_corpus_SUMMARY,ask_a_doctor_dtm_SUMMARY)
head(ask_freq_SUMMARY)

mis_freq_SUMMARY = sort(colSums(data.table(as.matrix(miscellaneous_dtm_SUMMARY))), decreasing = T)
mis_freq_SUMMARY = data.table(Terms = names(mis_freq_SUMMARY), frequency = mis_freq_SUMMARY)
rm(miscellaneous_corpus_SUMMARY,miscellaneous_dtm_SUMMARY)
head(mis_freq_SUMMARY)

lab_freq_SUMMARY = sort(colSums(data.table(as.matrix(lab_dtm_SUMMARY))), decreasing = T)
lab_freq_SUMMARY = data.table(Terms = names(lab_freq_SUMMARY), frequency = lab_freq_SUMMARY)
rm(lab_corpus_SUMMARY,lab_dtm_SUMMARY)
head(lab_freq_SUMMARY)

pre_freq_SUMMARY = sort(colSums(data.table(as.matrix(prescription_dtm_SUMMARY))), decreasing = T)
pre_freq_SUMMARY = data.table(Terms = names(pre_freq_SUMMARY), frequency = pre_freq_SUMMARY)
rm(prescription_corpus_SUMMARY,prescription_dtm_SUMMARY)
head(pre_freq_SUMMARY)

app_freq_DATA = sort(colSums(data.table(as.matrix(appoinments_dtm_DATA))), decreasing = T)
app_freq_DATA = data.table(Terms = names(app_freq_DATA), frequency = app_freq_DATA)
rm(appoinments_corpus_DATA,appoinments_dtm_DATA)
head(app_freq_SUMMARY)

ask_freq_DATA = sort(colSums(data.table(as.matrix(ask_a_doctor_dtm_DATA))), decreasing = T)
ask_freq_DATA = data.table(Terms = names(ask_freq_DATA), frequency = ask_freq_DATA)
rm(ask_a_doctor_corpus_DATA,ask_a_doctor_dtm_DATA)
head(ask_freq_DATA)

mis_freq_DATA = sort(colSums(data.table(as.matrix(miscellaneous_dtm_DATA))), decreasing = T)
mis_freq_DATA = data.table(Terms = names(mis_freq_DATA), frequency = mis_freq_DATA)
rm(miscellaneous_corpus_DATA,miscellaneous_dtm_DATA)
head(mis_freq_DATA)

lab_freq_DATA = sort(colSums(data.table(as.matrix(lab_dtm_DATA))), decreasing = T)
lab_freq_DATA = data.table(Terms = names(lab_freq_DATA), frequency = lab_freq_DATA)
rm(lab_corpus_DATA,lab_dtm_DATA)
head(lab_freq_DATA)

pre_freq_DATA = sort(colSums(data.table(as.matrix(prescription_dtm_DATA))), decreasing = T)
pre_freq_DATA = data.table(Terms = names(pre_freq_DATA), frequency = pre_freq_DATA)
rm(prescription_corpus_DATA,prescription_dtm_DATA)
head(pre_freq_DATA)


#Combined data for category wise
combined_app_data = rbind(app_freq_DATA,app_freq_SUMMARY)
head(combined_app_data)
combined_ask_data = rbind(ask_freq_DATA,ask_freq_SUMMARY)
head(combined_ask_data)
combined_mis_data = rbind(mis_freq_DATA,mis_freq_SUMMARY)
head(combined_mis_data)
combined_lab_data = rbind(lab_freq_DATA,lab_freq_SUMMARY)
head(combined_lab_data)
combined_pre_data = rbind(pre_freq_DATA,pre_freq_SUMMARY)
head(combined_pre_data)

#Comb_data = rbind(combined_app_data,combined_ask_data,combined_lab_data,combined_mis_data,combined_pre_data)


#wordclouds for categories by their respective data sets

# creating word cloud for top 200 words in frequency

#png("individual_appointment.png")
wordcloud(combined_app_data$Terms, combined_app_data$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("individual_ask_a_doctor.png")
wordcloud(combined_ask_data$Terms, combined_ask_data$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("individual_miscellanious.png")
wordcloud(combined_mis_data$Terms, combined_mis_data$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("individual_lab.png")
wordcloud(combined_lab_data$Terms, combined_lab_data$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2")) 
#dev.off()

#png("individual_prescription.png")
wordcloud(combined_pre_data$Terms, combined_pre_data$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#Finding the common terms present in any two categories from SUMMARY column
common_SUMMARY = app_freq_SUMMARY[app_freq_SUMMARY$Terms %in% ask_freq_SUMMARY$Terms,] %>%
        rbind(app_freq_SUMMARY[app_freq_SUMMARY$Terms %in%  mis_freq_SUMMARY$Terms,]) %>%
        rbind(app_freq_SUMMARY[app_freq_SUMMARY$Terms %in% lab_freq_SUMMARY$Terms,]) %>%
        rbind(app_freq_SUMMARY[app_freq_SUMMARY$Terms %in% pre_freq_SUMMARY$Terms,]) %>%
        rbind(ask_freq_SUMMARY[ask_freq_SUMMARY$Terms %in% mis_freq_SUMMARY$Terms,]) %>%
        rbind(ask_freq_SUMMARY[ask_freq_SUMMARY$Terms %in% lab_freq_SUMMARY$Terms,]) %>%
        rbind(ask_freq_SUMMARY[ask_freq_SUMMARY$Terms %in% pre_freq_SUMMARY$Terms,]) %>%
        rbind(mis_freq_SUMMARY[mis_freq_SUMMARY$Terms %in% lab_freq_SUMMARY$Terms,]) %>%
        rbind(mis_freq_SUMMARY[mis_freq_SUMMARY$Terms %in% pre_freq_SUMMARY$Terms,]) %>%
        rbind(lab_freq_SUMMARY[lab_freq_SUMMARY$Terms %in% pre_freq_SUMMARY$Terms,])

head(common_SUMMARY)

#Finding the common terms present in any two categories from DATA column
common_DATA = app_freq_DATA[app_freq_DATA$Terms %in% ask_freq_DATA$Terms,] %>%
        rbind(app_freq_DATA[app_freq_DATA$Terms %in% mis_freq_DATA$Terms,]) %>%
        rbind(app_freq_DATA[app_freq_DATA$Terms %in% lab_freq_DATA$Terms,]) %>%
        rbind(app_freq_DATA[app_freq_DATA$Terms %in% pre_freq_DATA$Terms,]) %>%
        rbind(ask_freq_DATA[ask_freq_DATA$Terms %in% mis_freq_DATA$Terms,]) %>%
        rbind(ask_freq_DATA[ask_freq_DATA$Terms %in% lab_freq_DATA$Terms,]) %>%
        rbind(ask_freq_DATA[ask_freq_DATA$Terms %in% pre_freq_DATA$Terms,]) %>%
        rbind(mis_freq_DATA[mis_freq_DATA$Terms %in% lab_freq_DATA$Terms,]) %>%
        rbind(mis_freq_DATA[mis_freq_DATA$Terms %in% pre_freq_DATA$Terms,]) %>%
        rbind(lab_freq_DATA[lab_freq_DATA$Terms %in% pre_freq_DATA$Terms,])

head(common_DATA)

common = rbind(common_DATA,common_SUMMARY) #combining both the common terms
#common = common[order(common$frequency,decreasing = TRUE)]
#wordcloud(unique(common$Terms), common$frequency,max.words = 100, scale = c(4,0.75),
        #random.order = F, colors=brewer.pal(8, "Dark2"))
common = common[1:10] #extractin only top 100 most frequent common terms
dim(common)

#png("Common_terms_between_categories.png")
ggplot(common, aes(Terms, frequency))+
        geom_bar(stat = 'identity', colour = "grey", fill = 'grey')+
        labs(title= 'High frequent Common Terms between categories')
#dev.off()

common_unique = unique(common$Terms)
length(common_unique)

#These 100 most frequent words between categories are filtered out in feature engineering

save(common_unique,file = "Third_step.dat")
