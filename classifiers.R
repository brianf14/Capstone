setwd("G:/My Documents/Personal/Masters/Capstone/Code/github")

library(plyr)
library(dplyr) # Use the pipe functionality in data cleanup
library(tm) # text mining library
library(tidytext) # text mining library
library(e1071) # Used for classifiers. Contains tune function
library(caret) # Used for confusion matrix
library(NLP) # Used to create ngram functions
library(pander) # graphics
library(ggplot2) # graphics
library(lubridate) # Dates
library(sentimentr)
library(tidyr)

######################################### SECTION 1: Read in data ###############################
corpus = read.csv("headlines.csv", stringsAsFactors = FALSE) # pos, neg
corpus$Close.to.Open.Return..Rel. = as.numeric(corpus$Close.to.Open.Return..Rel.)
dim(corpus)

# Convert dependent var to factor
corpus$Sentiment.Tag = as.factor(corpus$Sentiment.Tag)

#  Table of Pos vs Neg Headlines
pander(table(corpus$Sentiment.Tag), caption="Neg (-1) and Pos (+1) Count in Raw Data")

# Change the US.Date column from character to Date
corpus$US.Date = dmy(corpus$US.Date)

# Sort the Corpus by date
corpus = corpus[order(corpus$US.Date, decreasing = TRUE), ] 

# Partition Data into training and test
# Training data from 01/01/17 to 30/12/17 (there were no articles on 31/12/17)
end_train_date = which(corpus$US.Date == '2017-12-30', arr.ind=TRUE)
# Test data from 02/01/18 to 31/12/18
start_test_date = which(corpus$US.Date == '2018-01-02', arr.ind=TRUE)
end_test_date = which(corpus$US.Date == '2018-12-31', arr.ind=TRUE)
train_split_index = end_train_date[1]:nrow(corpus)
test_split_index = start_test_date[length(start_test_date)]:end_test_date[1]

# Original Data
trainData_org <- corpus[train_split_index, ]
# Subset the training data to only include strong returns (i.e. those > 0.10% vs market)
trainData_org.strong.pos <- trainData_org[which(trainData_org$Close.to.Open.Return..Rel.>0.0010), ]
trainData_org.strong.neg <- trainData_org[which(trainData_org$Close.to.Open.Return..Rel.< -0.0010), ]
trainData_org = rbind(trainData_org.strong.neg, trainData_org.strong.pos)
# Remove the return data from the training set
trainData_org = trainData_org[, -4]
# test data
testData_org <- corpus[test_split_index, ]
# Remove the return data from the test set
testData_org <- testData_org[ ,-4]

# Dimensions
dim(trainData_org)
dim(testData_org)

##### Proportion of Pos & Neg Cases in Test & Train Data
prop.table(table(trainData_org$Sentiment.Tag))
summary(trainData_org$Sentiment.Tag) # 47% Neg vs 53% Pos

prop.table(table(testData_org$Sentiment.Tag))
summary(testData_org$Sentiment.Tag) # 48% Neg vs 52% Pos


################## TM SVM ####################################################
### Clean the training data
corpus.tm.train <- VCorpus(VectorSource(trainData_org$Headline))

# Remove urls
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
corpus.tm.train.clean <- tm_map(corpus.tm.train, removeURL)
# Remove Twitter tickers. These generally start with $ in the headlines
removeTicker <- content_transformer(function(x) gsub("\\$\\w*", "", x))
corpus.tm.train.clean <- tm_map(corpus.tm.train.clean, removeTicker)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
# Remove /
corpus.tm.train.clean <- tm_map(corpus.tm.train.clean, toSpace, "/")
# Remove @
corpus.tm.train.clean <- tm_map(corpus.tm.train.clean, toSpace, "@")
# Remove \\|
corpus.tm.train.clean <- tm_map(corpus.tm.train.clean, toSpace, "\\|")
# Remove #
corpus.tm.train.clean <- tm_map(corpus.tm.train.clean, toSpace, "#")

# I don't want to exclude negation terms. Remove these from the generic stopword list
bespoke_stopwords = c("not", "no", "nor", "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't",
                      "hadn't", "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't",
                      "mustn't", "can't", "cannot", "couldn't")
# Stopwords = default stopwords less those that are in my list above
stopwords_bf = stopwords(kind = "en")[!stopwords(kind="en")%in%bespoke_stopwords]
# I want to include the following as stopwords
stopwords_bespoke = c("twitter", "seekingalpha", "seekingalpha_fs", "seekingalphafs", "benzinga", "inc")
# Final list of stopswords = default list, less negation terms above, plus my bespoke stopwords
stopwords_final = c(stopwords_bespoke, stopwords_bf)

# Eliminate any remaining punctuation, white space, stopwords, convert to lower case & stem
corpus.tm.train.clean <- corpus.tm.train.clean %>%
        tm_map(content_transformer(tolower)) %>% 
        tm_map(removePunctuation) %>% # this also removes hashtags
        tm_map(removeWords, stopwords_final) %>%
        tm_map(stripWhitespace) %>%
        tm_map(removeNumbers) %>%
        tm_map(stemDocument, language = "english")

# Negation - replace n't, and not with not_ and nor with nor_
str_negate <- content_transformer(function(x) gsub("not ","not_",gsub("n't ","not_",x), gsub("nor", "nor_", x)))
corpus.tm.train.clean <- tm_map(corpus.tm.train.clean, str_negate)

# Create Document Term Matrix of Training Data
dtm.train = DocumentTermMatrix(corpus.tm.train.clean)

#### Clean the test data
corpus.tm.test <- VCorpus(VectorSource(testData_org$Headline))

# Remove urls
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
corpus.tm.test.clean <- tm_map(corpus.tm.test, removeURL)
# Remove tickers from Twitter. These generally start with $ in the headlines
removeTicker <- content_transformer(function(x) gsub("\\$\\w*", "", x))
corpus.tm.test.clean <- tm_map(corpus.tm.test.clean, removeTicker)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
# Remove /
corpus.tm.test.clean <- tm_map(corpus.tm.test.clean, toSpace, "/")
# Remove @
corpus.tm.test.clean <- tm_map(corpus.tm.test.clean, toSpace, "@")
# Remove \\|
corpus.tm.test.clean <- tm_map(corpus.tm.test.clean, toSpace, "\\|")
# Remove #
corpus.tm.test.clean <- tm_map(corpus.tm.test.clean, toSpace, "#")

corpus.tm.test.clean <- corpus.tm.test.clean %>%
        tm_map(content_transformer(tolower)) %>% 
        tm_map(removePunctuation) %>% # this also removes hashtags
        tm_map(removeWords, stopwords_final) %>%
        tm_map(stripWhitespace) %>%
        tm_map(removeNumbers) %>% 
        tm_map(stemDocument, language = "english")

# Negation - replace n't and not with not_ and nor with nor_
str_negate <- content_transformer(function(x) gsub("not ","not_",gsub("n't ","not_",x), gsub("nor", "nor_", x)))
corpus.tm.test.clean <- tm_map(corpus.tm.test.clean, str_negate)

# Create Document Term Matrix of Test Data
dtm.test = DocumentTermMatrix(corpus.tm.test.clean)

################################## Bigrams
### Bigram Tokeniser
BigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

# Create DTM of all bigrams in training set
dtm.bigram <- DocumentTermMatrix(corpus.tm.train.clean, control=list(tokenize = BigramTokenizer))
# Select bigrams that occur more than 2 times
term.freq <- findFreqTerms(dtm.bigram, 2)
# Create a DTM containing the bigrams that occur more than 2 times
dtm.bigram.freq <-DocumentTermMatrix(corpus.tm.train.clean, control=list(tokenize = BigramTokenizer, dictionary=term.freq))

# Dimensions of the DTM that contained all bigrams
dtm_full <- as.data.frame(as.matrix(dtm.bigram))
dim(dtm_full)

# Dimensions of the DTM that has been reduced in size
dtm_df.freq = as.data.frame(as.matrix(dtm.bigram.freq))
dim(dtm_df.freq)

# DF to use for BnS
dtm_df = as.data.frame(as.matrix(dtm.bigram.freq))

############################## BNS ########################################

### BNS calculation
names <- colnames(dtm_df)
all_feature_bns_score <- c()
for(i in 1:(ncol(dtm_df)-1)){
        compute_bns_column <- cbind.data.frame(dtm_df[,i], trainData_org$Sentiment.Tag) # Combine each column with the Sentiment score from the corpus
        # Get BNS score
        pos = length(which(compute_bns_column[, 2] == 1))
        neg = length(which(compute_bns_column[, 2] == -1))
        tp <- length(which(compute_bns_column[,2]==1 & compute_bns_column[,1]==1)) # number of positive cases with the feature
        fp <- length(which(compute_bns_column[,2]==-1 & compute_bns_column[,1]==1)) # number of negative cases with the feature
        tn <- neg - fp # Num of neg articles that didn't contain the feature
        fn <- pos - tp # Num of pos articles that didn't contain the feature
        tpr <- tp/pos
        fpr <- fp/neg
        
        # Per Forman (2008), limit the tpr and fpr to be between 0.0005 and 0.9995. This is because the inverse normal goes to infinity at 0 & 1
        if (tpr <= 0.0005){
                tpr = 0.0005
        }
        if (tpr >= 0.9995){
                tpr = 0.9995
        }
        if (fpr <= 0.0005){
                fpr = 0.0005
        }
        if (fpr >= 0.9995){
                fpr = 0.9995
        }
        bns_score <- abs(qnorm(tpr)-qnorm(fpr))
        feature <- names[i]
        
        feature_bns_score <- cbind.data.frame(feature,tpr, fpr,bns_score)
        
        if(length(all_feature_bns_score) == 0){
                all_feature_bns_score <- feature_bns_score
        }else{
                all_feature_bns_score <- rbind.data.frame(all_feature_bns_score,feature_bns_score)
        }
}

# Quantiles of BNS scores
quantile(all_feature_bns_score$bns_score, c(0.15, 0.25, 0.3, 0.45, 0.55, 0.6, 0.7, 0.8, .85, .86, 0.90, .95)) 

# Dictionary based on BNS by 95th percentile
dict_bns = all_feature_bns_score[ which(all_feature_bns_score$bns_score > quantile(all_feature_bns_score$bns_score, 0.95)), 1]
length(dict_bns)

# Create DTMs of training & test data based on bigrams and the BNS feature set
# Change the weighting parameter in the below to move between binary, TF & TF-IDF weightings
data_train_bns <- DocumentTermMatrix(corpus.tm.train.clean, control = list(tokenize = BigramTokenizer, dictionary=dict_bns, weighting=weightTf)) # creating a DTM of training data based on words in the BNS feature set
data_test_bns <- DocumentTermMatrix(corpus.tm.test.clean, control = list(tokenize = BigramTokenizer, dictionary=dict_bns, weighting = weightTf)) # creating a DTM of test data based on words in the BNS feature set

# Convert these DTMs to DFs for use in the classifier
data_train_bns = as.data.frame(as.matrix(data_train_bns))
data_test_bns = as.data.frame(as.matrix(data_test_bns))

# Remove headlines from the test set that don't match feature set. If the headlines don't match any terms, then the classifier will
# just label them as the dominant class which is not what we want.
drops.headlines.test = c()

for (i in 1:nrow(data_test_bns)){
        if (sum(data_test_bns[i, ]) == 0){
                drops.headlines.test = c(drops.headlines.test, rownames(data_test_bns)[i])
        }
} 

# Convert from character to numeric
drops.headlines.test = as.numeric(drops.headlines.test)
# Remove these headlines from the cleaned training DTM (data_test_bns)
data_test_bns_reduced = data_test_bns[-drops.headlines.test, ]
# Remove these headlines from the original training data 
testData_reduced = testData_org[-drops.headlines.test, ]
dim(testData_reduced)

# add the sentiment score in to the datasets
data_train_nb_bns <- cbind(Sentiment.Tag=factor(trainData_org$Sentiment.Tag), data_train_bns)
data_test_nb_bns <- cbind(Sentiment.Tag=factor(testData_reduced$Sentiment.Tag), data_test_bns_reduced)
data_test_bns.full <- cbind(Sentiment.Tag = factor(testData_org$Sentiment.Tag), data_test_bns)

# Fit SVM using BNS. The cost and gamma parameters have been set based on 10 fold CV using the tune function.
# This is in the section below that is commented out
start.time.svm <- Sys.time()
set.seed(123)
fit_svm_bns <- svm(Sentiment.Tag~., data=data_train_nb_bns, cost = 1.3, gamma = 0.005780347) # a radial kernel is used by default. Scaling is automatic
end.time.svm <- Sys.time()
time.taken.svm <- end.time.svm - start.time.svm
time.taken.svm

# Using tune to train a SVM
# start.time.svm <- Sys.time()
# set.seed(123)
# tune.out = tune(svm, Sentiment.Tag~., data = data_train_nb_bns, kernel = "radial",
#                 ranges = list(cost = c(2^(-4:2)), 
#                               gamma = 2^(-9.5))) 
# end.time.svm <- Sys.time()
# time.taken.svm.tune <- end.time.svm - start.time.svm
# time.taken.svm.tune
# summary(tune.out)  
# 
# # Best model from CV
# bestmod <- tune.out$best.model

# Make predictions on the test set using the best svm model that was trained on the training data
# The test set that we are using does not have the sentiment score included
#pred_tune <- predict(bestmod, data_test_bns_reduced)

# Confustion Matrix SVM Tune
# conf.mat_svm.tune <- confusionMatrix(pred_tune, data_test_nb_bns$Sentiment.Tag)
# conf.mat_svm.tune 

# Predictions of SVM on the test data that does not contain headlines which have been removed
fit_svm_bns.pred <- predict(fit_svm_bns, data_test_bns_reduced) # Testing this on the test dataset without the sentiment tag 

# Confustion Matrix Based on Reduced Test Dataset
conf.mat_svm.bns <- confusionMatrix(fit_svm_bns.pred, data_test_nb_bns$Sentiment.Tag)
conf.mat_svm.bns 

# Combine the predictions with the headline from the test data
model_predictions = cbind.data.frame(fit_svm_bns.pred, testData_reduced)

# Save Model
saveRDS(fit_svm_bns, "fit_svm_bns.rds")

# Save Predictions
saveRDS(model_predictions, "model_predictions.rds")

########################################### ------------ TRIGRAMS ------------------------------ #########################################
# Trigram Tokeniser
TrigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}

# Create DTM of all trigrams in training set
dtm.trigram <- DocumentTermMatrix(corpus.tm.train.clean, control=list(tokenize = TrigramTokenizer))
# Select trigrams that occur more than 2 times
term.freq <- findFreqTerms(dtm.trigram, 2)
# Create a DTM containing trigrams occuring more than 2 times
dtm.trigram.freq <-DocumentTermMatrix(corpus.tm.train.clean, control=list(tokenize = TrigramTokenizer, dictionary=term.freq))

# Dimensions of the DTM that contained all trigrams
dtm_full <- as.data.frame(as.matrix(dtm.trigram))
dim(dtm_full)

# Dimensions of the DTM that has been reduced in size
dtm_df.freq = as.data.frame(as.matrix(dtm.trigram.freq))
dim(dtm_df.freq)

# DF to Use for BNS
dtm_df_tri = as.data.frame(as.matrix(dtm.trigram.freq))

############################## BNS ------------------- TRIGRAMS ########################################
names <- colnames(dtm_df_tri)
all_feature_bns_score <- c()
for(i in 1:(ncol(dtm_df_tri)-1)){
        compute_bns_column <- cbind.data.frame(dtm_df_tri[,i], trainData_org$Sentiment.Tag) # Combine each column with the Sentiment score from the corpus
        # Get BNS score
        pos = length(which(compute_bns_column[, 2] == 1))
        neg = length(which(compute_bns_column[, 2] == -1))
        tp <- length(which(compute_bns_column[,2]==1 & compute_bns_column[,1]==1)) # number of positive cases with the feature
        fp <- length(which(compute_bns_column[,2]==-1 & compute_bns_column[,1]==1)) # number of negative cases with the feature
        tn <- neg - fp # Num of neg articles that didn't contain the feature
        fn <- pos - tp # Num of pos articles that didn't contain the feature
        tpr <- tp/pos
        fpr <- fp/neg
        
        # Per Forman (2008), limit the tpr and fpr to be between 0.0005 and 0.9995. This is because the inverse normal goes to infinity at 0 & 1
        if (tpr <= 0.0005){
                tpr = 0.0005
        }
        if (tpr >= 0.9995){
                tpr = 0.9995
        }
        if (fpr <= 0.0005){
                fpr = 0.0005
        }
        if (fpr >= 0.9995){
                fpr = 0.9995
        }
        bns_score <- abs(qnorm(tpr)-qnorm(fpr))
        feature <- names[i]
        
        feature_bns_score <- cbind.data.frame(feature,tpr, fpr,bns_score)
        
        if(length(all_feature_bns_score) == 0){
                all_feature_bns_score <- feature_bns_score
        }else{
                all_feature_bns_score <- rbind.data.frame(all_feature_bns_score,feature_bns_score)
        }
}

# Quantiles of BNS scores
quantile(all_feature_bns_score$bns_score, c(0.15, 0.25, 0.3, 0.45, 0.55, 0.6, 0.7, 0.8, .85, .86, 0.90, .95)) 

# Feature set based on BNS by percentile
dict_bns = all_feature_bns_score[ which(all_feature_bns_score$bns_score > quantile(all_feature_bns_score$bns_score, 0.95)), 1]
length(dict_bns)

# Create DTMs of training & test data based on trigrams and the BNS feature set
data_train_bns_tri <- DocumentTermMatrix(corpus.tm.train.clean, control = list(tokenize = TrigramTokenizer, dictionary=dict_bns, weighting=weightTf)) # creating a DTM of training data based on words in the BNS feature set
data_test_bns_tri <- DocumentTermMatrix(corpus.tm.test.clean, control = list(tokenize = TrigramTokenizer, dictionary=dict_bns, weighting = weightTf)) # creating a DTM of test data based on words in the BNS feature set

# Convert these DTMs to DFs for use in the classifier
data_train_bns_tri = as.data.frame(as.matrix(data_train_bns_tri))
data_test_bns_tri = as.data.frame(as.matrix(data_test_bns_tri))

# Remove headlines from the test set that don't match feature set
drops.headlines.test = c()

for (i in 1:nrow(data_test_bns_tri)){
        if (sum(data_test_bns_tri[i, ]) == 0){
                drops.headlines.test = c(drops.headlines.test, rownames(data_test_bns_tri)[i])
        }
} 

# Convert from character to numeric
drops.headlines.test = as.numeric(drops.headlines.test)
# Remove these headlines from the cleaned training DTM (data_test_bns)
data_test_bns_tri = data_test_bns_tri[-drops.headlines.test, ]
# Remove these headlines from the original training data 
testData_reduced_tri = testData_org[-drops.headlines.test, ]
dim(testData_reduced_tri)

# add the sentiment score in to the datasets
data_train_nb_bns_tri <- cbind(Sentiment.Tag=factor(trainData_org$Sentiment.Tag), data_train_bns_tri)
data_test_nb_bns_tri <- cbind(Sentiment.Tag=factor(testData_reduced_tri$Sentiment.Tag), data_test_bns_tri)

# Fit SVM using BNS
start.time.svm <- Sys.time()
set.seed(123)
fit_svm_bns_tri <- svm(Sentiment.Tag~., data=data_train_nb_bns_tri, cost = 1.3, gamma = 0.005780347) # a radial kernel is used by default. Scaling is automatic
end.time.svm <- Sys.time()
time.taken.svm <- end.time.svm - start.time.svm
time.taken.svm

# Predictions of SVM on test data
fit_svm_bns.pred_tri <- predict(fit_svm_bns_tri, data_test_bns_tri) # Testing this on the test dataset without the sentiment tag 

# Confustion Matrix Trigram fit
conf.mat_svm.bns_tri <- confusionMatrix(fit_svm_bns.pred_tri, data_test_nb_bns_tri$Sentiment.Tag)
conf.mat_svm.bns_tri 


################################################### --------------- BAG OF WORDS ---------------------- #####################################
# Create DTM of all words in training set
dtm.bow <- DocumentTermMatrix(corpus.tm.train.clean)
# Select words that occur more than 2 times
term.freq <- findFreqTerms(dtm.bow, 2)
# Create a DTM containing unigrams occuring more than 2 times
dtm.bow.freq <-DocumentTermMatrix(corpus.tm.train.clean, control=list(dictionary=term.freq))

# Dimensions of the DTM that contained all unigrams
dtm_full <- as.data.frame(as.matrix(dtm.bow))
dim(dtm_full)

# Dimensions of the DTM that has been reduced in size
dtm_df.freq = as.data.frame(as.matrix(dtm.bow.freq))
dim(dtm_df.freq)

# DF to Use for BNS
dtm_df_bow = as.data.frame(as.matrix(dtm.bow.freq))

############################## BNS FOR BAG OF WORDS ########################################
names <- colnames(dtm_df_bow)
all_feature_bns_score <- c()
for(i in 1:(ncol(dtm_df_bow)-1)){
        compute_bns_column <- cbind.data.frame(dtm_df_bow[,i], trainData_org$Sentiment.Tag) # Combine each column with the Sentiment score from the corpus
        # Get BNS score
        pos = length(which(compute_bns_column[, 2] == 1))
        neg = length(which(compute_bns_column[, 2] == -1))
        tp <- length(which(compute_bns_column[,2]==1 & compute_bns_column[,1]==1)) # number of positive cases with the feature
        fp <- length(which(compute_bns_column[,2]==-1 & compute_bns_column[,1]==1)) # number of negative cases with the feature
        tn <- neg - fp # Num of neg articles that didn't contain the feature
        fn <- pos - tp # Num of pos articles that didn't contain the feature
        tpr <- tp/pos
        fpr <- fp/neg
        
        # Per Forman (2008), limit the tpr and fpr to be between 0.0005 and 0.9995. This is because the inverse normal goes to infinity at 0 & 1
        if (tpr <= 0.0005){
                tpr = 0.0005
        }
        if (tpr >= 0.9995){
                tpr = 0.9995
        }
        if (fpr <= 0.0005){
                fpr = 0.0005
        }
        if (fpr >= 0.9995){
                fpr = 0.9995
        }
        bns_score <- abs(qnorm(tpr)-qnorm(fpr))
        feature <- names[i]
        
        feature_bns_score <- cbind.data.frame(feature,tpr, fpr,bns_score)
        
        if(length(all_feature_bns_score) == 0){
                all_feature_bns_score <- feature_bns_score
        }else{
                all_feature_bns_score <- rbind.data.frame(all_feature_bns_score,feature_bns_score)
        }
}

# Quantiles of BNS scores
quantile(all_feature_bns_score$bns_score, c(0.15, 0.25, 0.3, 0.45, 0.55, 0.6, 0.7, 0.8, .85, .86, 0.90, .95)) 

# Selecting top terms based on BNS percentile
dict_bns = all_feature_bns_score[ which(all_feature_bns_score$bns_score > quantile(all_feature_bns_score$bns_score, 0.95)), 1]
length(dict_bns)

# Create DTMs of training & test data based on BoW and the BNS feature set
data_train_bns_bow <- DocumentTermMatrix(corpus.tm.train.clean, control = list(dictionary=dict_bns, weighting=weightBin)) # creating a DTM of training data based on words in the BNS feature set
data_test_bns_bow <- DocumentTermMatrix(corpus.tm.test.clean, control = list(dictionary=dict_bns, weighting = weightBin)) # creating a DTM of test data based on words in the BNS feature set

# Convert these DTMs to DFs for use in the classifier
data_train_bns_bow = as.data.frame(as.matrix(data_train_bns_bow))
data_test_bns_bow = as.data.frame(as.matrix(data_test_bns_bow))

# Remove headlines from the test set that don't match dictionary terms
drops.headlines.test = c()

for (i in 1:nrow(data_test_bns_bow)){
        if (sum(data_test_bns_bow[i, ]) == 0){
                drops.headlines.test = c(drops.headlines.test, rownames(data_test_bns_bow)[i])
        }
} 

# Convert from character to numeric
drops.headlines.test = as.numeric(drops.headlines.test)
# Remove these headlines from the cleaned training DTM 
data_test_bns_bow = data_test_bns_bow[-drops.headlines.test, ]
# Remove these headlines from the original training data 
testData_reduced_bow = testData_org[-drops.headlines.test, ]
dim(testData_reduced_bow)

# add the sentiment score in to the datasets
data_train_nb_bns_bow <- cbind(Sentiment.Tag=factor(trainData_org$Sentiment.Tag), data_train_bns_bow)
data_test_nb_bns_bow <- cbind(Sentiment.Tag=factor(testData_reduced_bow$Sentiment.Tag), data_test_bns_bow)

# Fit SVM using BNS
start.time.svm <- Sys.time()
set.seed(123)
fit_svm_bns_bow <- svm(Sentiment.Tag~., data=data_train_nb_bns_bow, cost = 1.3, gamma = 0.005780347) # a radial kernel is used by default. Scaling is automatic
end.time.svm <- Sys.time()
time.taken.svm <- end.time.svm - start.time.svm
time.taken.svm

# Predictions of SVM on test data
fit_svm_bns.pred_bow <- predict(fit_svm_bns_bow, data_test_bns_bow) # Testing this on the test dataset without the sentiment tag 

# Confustion Matrix of BoW Fit
conf.mat_svm.bns_bow <- confusionMatrix(fit_svm_bns.pred_bow, data_test_nb_bns_bow$Sentiment.Tag)
conf.mat_svm.bns_bow 

###################################### FREQUENCY - BIGRAMS ---------------------- ############################################
# Create DTM of all bigrams in training set
dtm.bigram <- DocumentTermMatrix(corpus.tm.train.clean, control=list(tokenize = BigramTokenizer))
# Select bigrams that occur more than x amount of times - change the frequency in the below to move between frequencies of 10, 20 and 40
term.freq <- findFreqTerms(dtm.bigram, 10)
length(term.freq)
# Create a DTM containing bigrams occuring more than x times
dtm.bigram.freq <-DocumentTermMatrix(corpus.tm.train.clean, control=list(tokenize = BigramTokenizer, dictionary=term.freq))

# Dimensions of the DTM that contained all bigrams
dtm_full <- as.data.frame(as.matrix(dtm.bigram))
dim(dtm_full)

# Dimensions of the DTM that has been reduced in size
dtm_df.freq = as.data.frame(as.matrix(dtm.bigram.freq))
dim(dtm_df.freq)

# DF to Use for frequency based dictionary
dtm_df = as.data.frame(as.matrix(dtm.bigram.freq))

# Create DTMs of training & test data based on bigrams and the frequency based feature set
data_train_bigram <- DocumentTermMatrix(corpus.tm.train.clean, control = list(tokenize = BigramTokenizer, dictionary= term.freq, weighting=weightBin)) # creating a DTM of training data based on words in the feature set
data_test_bigram <- DocumentTermMatrix(corpus.tm.test.clean, control = list(tokenize = BigramTokenizer, dictionary=term.freq, weighting = weightBin)) # creating a DTM of test data based on words in the feature set

# Convert these DTMs to DFs for use in the classifier
data_train_bigram = as.data.frame(as.matrix(data_train_bigram))
data_test_bigram = as.data.frame(as.matrix(data_test_bigram))

# Remove headlines from the test set that don't match dictionary terms
drops.headlines.test = c()

for (i in 1:nrow(data_test_bigram)){
        if (sum(data_test_bigram[i, ]) == 0){
                drops.headlines.test = c(drops.headlines.test, rownames(data_test_bigram)[i])
        }
} 

# Convert from character to numeric
drops.headlines.test = as.numeric(drops.headlines.test)
# Remove these headlines from the cleaned training DTM 
data_test_bigram = data_test_bigram[-drops.headlines.test, ]
# Remove these headlines from the original training data 
testData_reduced = testData_org[-drops.headlines.test, ]
dim(testData_reduced)

# add the sentiment score in to the datasets
data_train_nb_bigram <- cbind(Sentiment.Tag=factor(trainData_org$Sentiment.Tag), data_train_bigram)
data_test_nb_bigram <- cbind(Sentiment.Tag=factor(testData_reduced$Sentiment.Tag), data_test_bigram)

# Fit SVM using bigram frequency based
start.time.svm <- Sys.time()
set.seed(123)
fit_svm_bigram <- svm(Sentiment.Tag~., data=data_train_nb_bigram, cost = 1.3, gamma = 0.005780347) # a radial kernel is used by default. Scaling is automatic
end.time.svm <- Sys.time()
time.taken.svm <- end.time.svm - start.time.svm
time.taken.svm

# Predictions of SVM on test data
fit_svm_bigram.pred <- predict(fit_svm_bigram, data_test_bigram) # Testing this on the test dataset without the sentiment tag 

# Confustion Matrix Bigram Frequency SVM
conf.mat_svm.bigram <- confusionMatrix(fit_svm_bigram.pred, data_test_nb_bigram$Sentiment.Tag)
conf.mat_svm.bigram

###################################### FREQUENCY - TRIGRAMS ---------------------- ############################################
# Create DTM of all trigrams in training set
dtm.trigram <- DocumentTermMatrix(corpus.tm.train.clean, control=list(tokenize = TrigramTokenizer))
# Select trigrams that occur more than x amount of times. Change the frequency parameter below to move between frequencies of 8, 16 and 30
term.freq <- findFreqTerms(dtm.trigram, 30)
length(term.freq)
# Create a DTM containing trigrams occuring more than x times
dtm.trigram.freq <-DocumentTermMatrix(corpus.tm.train.clean, control=list(tokenize = TrigramTokenizer, dictionary=term.freq))

# Dimensions of the DTM that contained all bigrams
dtm_full <- as.data.frame(as.matrix(dtm.trigram))
dim(dtm_full)

# Dimensions of the DTM that has been reduced in size
dtm_df.freq = as.data.frame(as.matrix(dtm.trigram.freq))
dim(dtm_df.freq)

# DF to Use for frequency based feature set
dtm_df = as.data.frame(as.matrix(dtm.trigram.freq))

# Create DTMs of training & test data based on trigrams and the frequency feature set
data_train_trigram <- DocumentTermMatrix(corpus.tm.train.clean, control = list(tokenize = TrigramTokenizer, dictionary= term.freq, weighting=weightTf)) # creating a DTM of training data based on words in the feature set
data_test_trigram <- DocumentTermMatrix(corpus.tm.test.clean, control = list(tokenize = TrigramTokenizer, dictionary=term.freq, weighting = weightTf)) # creating a DTM of test data based on words in the feature set

# Convert these DTMs to DFs for use in the classifier
data_train_trigram = as.data.frame(as.matrix(data_train_trigram))
data_test_trigram = as.data.frame(as.matrix(data_test_trigram))

# Remove headlines from the test set that don't match dictionary terms
drops.headlines.test = c()

for (i in 1:nrow(data_test_trigram)){
        if (sum(data_test_trigram[i, ]) == 0){
                drops.headlines.test = c(drops.headlines.test, rownames(data_test_trigram)[i])
        }
} 

# Convert from character to numeric
drops.headlines.test = as.numeric(drops.headlines.test)
# Remove these headlines from the cleaned training DTM 
data_test_trigram = data_test_trigram[-drops.headlines.test, ]
# Remove these headlines from the original training data 
testData_reduced = testData_org[-drops.headlines.test, ]
dim(testData_reduced)

# add the sentiment score in to the datasets
data_train_nb_trigram <- cbind(Sentiment.Tag=factor(trainData_org$Sentiment.Tag), data_train_trigram)
data_test_nb_trigram <- cbind(Sentiment.Tag=factor(testData_reduced$Sentiment.Tag), data_test_trigram)

# Fit SVM using frequency based
start.time.svm <- Sys.time()
set.seed(123)
fit_svm_trigram <- svm(Sentiment.Tag~., data=data_train_nb_trigram, cost = 1.3, gamma = 0.005780347) # a radial kernel is used by default. Scaling is automatic
end.time.svm <- Sys.time()
time.taken.svm <- end.time.svm - start.time.svm
time.taken.svm

# Predictions of SVM on test data
fit_svm_trigram.pred <- predict(fit_svm_trigram, data_test_trigram) # Testing this on the test dataset without the sentiment tag 

# Confustion Matrix Trigram Frequency Based SVM
conf.mat_svm.trigram <- confusionMatrix(fit_svm_trigram.pred, data_test_nb_trigram$Sentiment.Tag)
conf.mat_svm.trigram 

###################################### FREQUENCY - BAG OF WORDS ---------------------- ############################################
# Create DTM of all words in training set
dtm.bow <- DocumentTermMatrix(corpus.tm.train.clean)
# Select words that occur more than x amount of times. Change the frequency parameter below to move between 15, 30 and 60
term.freq <- findFreqTerms(dtm.bow, 30)
length(term.freq)
# Create a DTM containing unigrams occuring more than x times
dtm.bow.freq <-DocumentTermMatrix(corpus.tm.train.clean, control=list(dictionary=term.freq))

# Dimensions of the DTM that contained all unigrams
dtm_full <- as.data.frame(as.matrix(dtm.bow))
dim(dtm_full)

# Dimensions of the DTM that has been reduced in size
dtm_df.freq = as.data.frame(as.matrix(dtm.bow.freq))
dim(dtm_df.freq)

# Create DTMs of training & test data based on BoW and the frequency based feature set
data_train_bow <- DocumentTermMatrix(corpus.tm.train.clean, control = list(dictionary=term.freq, weighting=weightBin)) # creating a DTM of training data based on words in the feature set
data_test_bow <- DocumentTermMatrix(corpus.tm.test.clean, control = list(dictionary=term.freq, weighting = weightBin)) # creating a DTM of test data based on words in the feature set

# Convert these DTMs to DFs for use in the classifier
data_train_bow = as.data.frame(as.matrix(data_train_bow))
data_test_bow = as.data.frame(as.matrix(data_test_bow))

# Remove headlines from the test set that don't match dictionary terms
drops.headlines.test = c()

for (i in 1:nrow(data_test_bow)){
        if (sum(data_test_bow[i, ]) == 0){
                drops.headlines.test = c(drops.headlines.test, rownames(data_test_bow)[i])
        }
} 

# Convert from character to numeric
drops.headlines.test = as.numeric(drops.headlines.test)
# Remove these headlines from the cleaned training DTM 
data_test_bow = data_test_bow[-drops.headlines.test, ]
# Remove these headlines from the original training data 
testData_reduced = testData_org[-drops.headlines.test, ]
dim(testData_reduced)

# add the sentiment score in to the datasets
data_train_nb_bow <- cbind(Sentiment.Tag=factor(trainData_org$Sentiment.Tag), data_train_bow)
data_test_nb_bow <- cbind(Sentiment.Tag=factor(testData_reduced$Sentiment.Tag), data_test_bow)

# Fit SVM using frequency based
start.time.svm <- Sys.time()
set.seed(123)
fit_svm_bow <- svm(Sentiment.Tag~., data=data_train_nb_bow, cost = 1.3, gamma = 0.005780347) # a radial kernel is used by default. Scaling is automatic
end.time.svm <- Sys.time()
time.taken.svm <- end.time.svm - start.time.svm
time.taken.svm

# Predictions of SVM on test data
fit_svm_bow.pred <- predict(fit_svm_bow, data_test_bow) # Testing this on the test dataset without the sentiment tag 

# Confustion Matrix for BoW frequency based SVM
conf.mat_svm.bow <- confusionMatrix(fit_svm_bow.pred, data_test_nb_bow$Sentiment.Tag)
conf.mat_svm.bow 

############################## Looking at Most Frequent Term in Training Data ##########################################################

# Most Frequent Terms - BoW
freq_terms.matrix = as.matrix(dtm.bow.freq)
freq_terms <- sort(colSums(freq_terms.matrix), decreasing=TRUE)
head(freq_terms, 10)

# Most Frequent Terms - Bigrams
freq_bigrams.matrix = as.matrix(dtm.bigram.freq)
freq_bigrams <- sort(colSums(freq_bigrams.matrix), decreasing=TRUE)
head(freq_bigrams, 10)

############################ USING NATIVE CLASSIFIER - SENTIMENTR ###########################################
### - Generating binary sentiment score below using SentimentR and the Loughran McDonald dictionary. This will be used when comparing returns.

# Get sentiment score from sentimentR
headlines.sentiment = sentiment_by(testData_org$Headline, polarity_dt = lexicon::hash_sentiment_loughran_mcdonald)
dim(headlines.sentiment)
# Combine the LMcD sentiment with original testData
sentiment.lmcd = cbind(headlines.sentiment, testData_org)
dim(sentiment.lmcd)
# Remove headlines that have a 0 sentiment
sentiment.lmcd = sentiment.lmcd[which(sentiment.lmcd$ave_sentiment!= 0)]
dim(sentiment.lmcd)

# Convert to DF
sentiment.lmcd = as.data.frame(sentiment.lmcd)

# Assign binary sentiment classification
for(i in 1:nrow(sentiment.lmcd)){
        if(sentiment.lmcd[i, 4]> 0){
                sentiment.lmcd$binary.sentiment.lmcd[i] = 1
        }
        else if(sentiment.lmcd[i, 4] < 0){
                sentiment.lmcd$binary.sentiment.lmcd[i] = -1
        }
        
}

saveRDS(sentiment.lmcd, "sentiment.lmcd.rds")