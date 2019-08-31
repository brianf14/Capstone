##### ---- THE FILE NAMED "EDA - FIRST" NEEDS TO BE RUN BE RUNNING THIS FILE.THE EDA FIRST FILE
##### GENERATES A CLEAN CORPUS THAT IS SAVED AND PASSED TO THIS FILE

#setwd("G:/My Documents/Personal/Masters/Capstone/Code/Final")

library(plyr)
library(dplyr) # Use the pipe functionality in data cleanup
library(tm) # text mining library
library(tidytext) # text mining library
library(pander) # graphics
library(ggplot2) # graphs
library(stringr) # str_detect
library(knitr) # summary table in EDA
library(wordcloud)
library(RColorBrewer) # Graph Colours
library(scales) # percent
library(lubridate) # Dates

# Read in corpus
corpus = readRDS("corpus.rds")

# Read in clean corpus
corpus.tm.clean = readRDS("corpus.tm.clean.rds")

# Bigram Tokeniser
BigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

#### BNS
# Create TDM of all bigrams
corpus.clean.tdm <- TermDocumentMatrix(corpus.tm.clean, control=list(tokenize = BigramTokenizer))
term.freq <- findFreqTerms(corpus.clean.tdm, 2)
# Create DTM
dtm.bigram <-DocumentTermMatrix(corpus.tm.clean, control=list(tokenize = BigramTokenizer, dictionary=term.freq))

# Create Matrix from TDM
tdm.bigram <- as.matrix(TermDocumentMatrix(corpus.tm.clean, control=list(tokenize = BigramTokenizer, dictionary=term.freq)))

# Convert to DF to Use for BNS
dtm_df = as.data.frame(as.matrix(dtm.bigram))

# BNS Calc - !!!!! DON'T USE THIS CODE TO TRAIN THE MODEL AS IT IS BASED ON THE FULL DATASET
names <- colnames(dtm_df)
all_feature_bns_score <- c()
for(i in 1:(ncol(dtm_df)-1)){
        compute_bns_column <- cbind.data.frame(dtm_df[,i], corpus$Sentiment.Tag) # Combine each column with the Sentiment score from the corpus
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

# Order by BNS score
frequent_bns = all_feature_bns_score[order(all_feature_bns_score$bns_score, decreasing = TRUE), ] 
frequent_bns$feature <- factor(frequent_bns$feature, levels = frequent_bns$feature[order(frequent_bns$bns_score, decreasing = TRUE)])

# Bigram frequency - After Cleaning
frequent_bigrams <- data.frame(Bigram = rownames(tdm.bigram), 
                               Freq = rowSums(tdm.bigram), 
                               row.names = NULL)

# Horizontal bar plot - BNS - This is based on the full dataset so will be different to the top scoring BNS terms from the training dataset
ggplot(data=frequent_bns[1:20, c(1, 4)], aes(x=feature, y=bns_score)) +
        geom_col() +
        ggtitle("Top BNS Scoring Bigrams - Full Dataset") +
        ylab("BNS Score") +
        xlab("Bigram")+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_flip()





