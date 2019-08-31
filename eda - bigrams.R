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

#### Bigrams
BigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

# Create TDM of all bigrams
corpus.clean.tdm <- TermDocumentMatrix(corpus.tm.clean, control=list(tokenize = BigramTokenizer))
term.freq <- findFreqTerms(corpus.clean.tdm, 2)
corpus.clean.tdm.freq <- TermDocumentMatrix(corpus.tm.clean, control=list(tokenize = BigramTokenizer, dictionary = term.freq))
corpus.bigrams <- as.matrix(corpus.clean.tdm.freq)

# Bigram frequency - After Cleaning
frequent_bigrams_clean <- data.frame(ST = rownames(corpus.bigrams), 
                                     Freq = rowSums(corpus.bigrams), 
                                     row.names = NULL)

# Order by frequency
frequent_bigrams_clean = frequent_bigrams_clean[order(frequent_bigrams_clean$Freq, decreasing = TRUE), ] 
frequent_bigrams_clean$ST <- factor(frequent_bigrams_clean$ST, levels = frequent_bigrams_clean$ST[order(frequent_bigrams_clean$Freq, decreasing = TRUE)])

# Horizontal bar plot - Bigrams
ggplot(data=frequent_bigrams_clean[1:20, ], aes(x=ST, y=Freq)) +
        ggtitle("Bigram Frequency") + 
        geom_col() +
        ylab("Bigram Frequency") + 
        xlab("Frequency") +
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_flip()

# Wordcloud - Bigrams Clean
wordcloud(words = frequent_bigrams_clean$ST, freq = frequent_bigrams_clean$Freq, max.words = 70, 
          scale=c(1.9,.4), random.order = FALSE, rot.per=0.35)

### Bigrams of Positive and Negative Headlines
# Create DTM
headlines.bigrams = DocumentTermMatrix(corpus.tm.clean, control=list(tokenize = BigramTokenizer, dictionary = term.freq))
# Convert to DF
headlines.bigrams.df = as.data.frame(as.matrix(headlines.bigrams))
# Add in sentiment tag
headlines.bigrams.df$sentiment.tag = corpus$Sentiment.Tag
# Subset positive headlines
headlines.pos.bigrams.df = headlines.bigrams.df[headlines.bigrams.df$sentiment.tag==1, ]
# Subset negative headlines
headlines.neg.bigrams.df = headlines.bigrams.df[headlines.bigrams.df$sentiment.tag==-1, ]

# Bigram frequency - After Cleaning - Positive Headlines
frequent_bigrams_pos <- data.frame(ST = colnames(headlines.pos.bigrams.df[ ,1:ncol(headlines.pos.bigrams.df)-1 ]), 
                                   Freq = colSums(headlines.pos.bigrams.df[ , 1:ncol(headlines.pos.bigrams.df)-1]))

# Order Positive Words by frequency
frequent_bigrams_pos = frequent_bigrams_pos[order(frequent_bigrams_pos$Freq, decreasing = TRUE), ] 
frequent_bigrams_pos$ST <- factor(frequent_bigrams_pos$ST, levels = frequent_bigrams_pos$ST[order(frequent_bigrams_pos$Freq, decreasing = TRUE)])

# Horizontal bar plot - Bigrams - Positive Headlines
ggplot(data=frequent_bigrams_pos[1:20, ], aes(x=ST, y=Freq)) +
        ggtitle("Bigram Frequency in Positive Headlines") + 
        geom_col() +
        xlab("Bigram Frequency") + 
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_flip()

# Bigram frequency - After Cleaning - Negative Headlines
frequent_bigrams_neg <- data.frame(ST = colnames(headlines.neg.bigrams.df[ ,1:ncol(headlines.neg.bigrams.df)-1 ]), 
                                   Freq = colSums(headlines.neg.bigrams.df[ , 1:ncol(headlines.neg.bigrams.df)-1]))

# Order Negative Words by frequency
frequent_bigrams_neg = frequent_bigrams_neg[order(frequent_bigrams_neg$Freq, decreasing = TRUE), ] 
frequent_bigrams_neg$ST <- factor(frequent_bigrams_neg$ST, levels = frequent_bigrams_neg$ST[order(frequent_bigrams_neg$Freq, decreasing = TRUE)])

# Horizontal bar plot - Bigrams - Negative Headlines
ggplot(data=frequent_bigrams_neg[1:20, ], aes(x=ST, y=Freq)) +
        ggtitle("Bigram Frequency in Negative Headlines") + 
        geom_col() +
        xlab("Bigram Frequency") + 
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_flip()
