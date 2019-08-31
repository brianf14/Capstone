##### ---- THE FILE NAMED "EDA - FIRST" NEEDS TO BE RUN BE RUNNING THIS FILE.THE EDA FIRST FILE
##### GENERATES A CLEAN CORPUS THAT IS SAVED AND PASSED TO THIS FILE

setwd("G:/My Documents/Personal/Masters/Capstone/Code/Final")

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

### Word frequency of positive & negative headlines
# Create DTM
headlines.dtm = DocumentTermMatrix(corpus.tm.clean)
# Convert to DF
headlines.df = as.data.frame(as.matrix(headlines.dtm))
# Add in sentiment tag
headlines.df$sentiment.tag = corpus$Sentiment.Tag
# Subset positive headlines
headlines.pos.df = headlines.df[headlines.df$sentiment.tag==1, ]
# Subset negative headlines
headlines.neg.df = headlines.df[headlines.df$sentiment.tag==-1, ]

# Word frequency - After Cleaning - Positive Headlines
frequent_terms_pos <- data.frame(ST = colnames(headlines.pos.df[ ,1:ncol(headlines.pos.df)-1 ]), 
                                 Freq = colSums(headlines.pos.df[ , 1:ncol(headlines.pos.df)-1]))

# Order Positive Words by frequency
frequent_terms_pos = frequent_terms_pos[order(frequent_terms_pos$Freq, decreasing = TRUE), ] 
frequent_terms_pos$ST <- factor(frequent_terms_pos$ST, levels = frequent_terms_pos$ST[order(frequent_terms_pos$Freq, decreasing = TRUE)])

# Horizontal bar plot of Positive Words
ggplot(data=frequent_terms_pos[1:20, ], aes(x=ST, y=Freq)) +
        ggtitle("Word Frequency in Positive Headlines") +
        geom_col() +
        xlab("Word") + 
        ylab("Word Frequency")+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) + 
        coord_flip()

# Word frequency - After Cleaning - Negative Headlines
frequent_terms_neg <- data.frame(ST = colnames(headlines.neg.df[ ,1:ncol(headlines.neg.df)-1 ]), 
                                 Freq = colSums(headlines.neg.df[ , 1:ncol(headlines.neg.df)-1]))

# Order Negative Words by frequency
frequent_terms_neg = frequent_terms_neg[order(frequent_terms_neg$Freq, decreasing = TRUE), ] 
frequent_terms_neg$ST <- factor(frequent_terms_neg$ST, levels = frequent_terms_neg$ST[order(frequent_terms_neg$Freq, decreasing = TRUE)])

# Horizontal bar plot of Negative Words
ggplot(data=frequent_terms_neg[1:20, ], aes(x=ST, y=Freq)) +
        ggtitle("Word Frequency in Negative Headlines") +
        geom_col() +
        xlab("Word") + 
        ylab("Word Frequency")+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_flip()




