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

######################################### SECTION 1: Read in data ###############################
corpus = read.csv("headlines.csv", stringsAsFactors = FALSE)

# Convert dependent var to factor
corpus$Sentiment.Tag = as.factor(corpus$Sentiment.Tag)

# Change the Date column from character to Date
corpus$US.Date = dmy(corpus$US.Date)
# Extract Year & Month and Year from US.Date
corpus$Year.Month = format(corpus$US.Date,"%Y-%m")
# Extract Year only
corpus$Year = format(corpus$US.Date,"%Y")

#  Table of Pos vs Neg Headlines
pander(table(corpus$Sentiment.Tag), caption="Pos and Neg Count in Raw Data")

# Add in Article Count column
for(i in 1:nrow(corpus)){
        corpus$article.count[i] = 1
}

# SAve corpus for use in other EDA files
saveRDS(corpus, "corpus.rds")

############################# EDA #########################################

# Plot of positive vs negative headline count
corpus %>% 
        group_by(Sentiment.Tag) %>% 
        summarize(count = n()) %>% 
        mutate(percent = count/sum(count)) %>% 
        ggplot(aes(x=Sentiment.Tag, y = count/sum(count))) +
        geom_col() +
        scale_y_continuous(labels=scales::percent) +
        geom_text(aes(label = paste0(round(100 * percent, 1), "%")), vjust = -0.25)+
        xlab("Sentiment") +
        ylab("Percentage of Headlines") +
        ggtitle("Headline Count by Sentiment") + 
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5))

### Sources
sources.summary = as.data.frame(apply(corpus[3], 2, table))
sources.summary = sources.summary[order(sources.summary$Source, decreasing = TRUE), ,drop=FALSE]
sources.summary$perc = sources.summary$Source/sum(sources.summary$Source)*100

# Create "Other" Category
for(i in 1:nrow(sources.summary)){
        if(sources.summary$perc[i] < 3){
                sources.summary$Category[i] = "Other"
        }
        else{
                sources.summary$Category[i] = rownames(sources.summary)[i]
        }
}

#Horizontal bar plot of Source
sources.labels = c("EDGAR", "Twitter", "Street\nAccount", "Business Wire", "PR Newswire\nUS",
                   "FactSet Call\nStreet Transcripts", "FactSet Institutional\nOwnership News", "Other")
sources.plot <-ggplot(data=sources.summary, aes(x=reorder(Category,-perc), y=perc)) +
        geom_col(position = 'dodge') + 
        geom_text(aes(label = scales::percent(perc/100)),position = position_dodge(width = .9),    # move to center of bars
                  vjust = -0.5,    # nudge above top of bar
                  size = 3) +
        scale_x_discrete(labels= sources.labels) +
        geom_bar(stat="identity") +
        xlab("News Source") +
        ylab("Percentage of Headlines") +
        ggtitle("Headlines by Source") +
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5))
sources.plot

# Plot of positive vs negative headline count
corpus %>% 
        group_by(Identifier) %>% 
        summarize(count = n()) %>% 
        ggplot(aes(x=reorder(Identifier,-count), y = count)) +
        geom_col() +
        xlab("Company Ticker") +
        ylab("Number of Headlines") +
        ggtitle("Headline Count by Company") + 
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_flip()

# Bar Chart of article count by Year & Month
months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
           "Oct", "Nov", "Dec")

# 2017
corpus[which(corpus$Year == "2017"), ] %>% 
        group_by(Year.Month) %>% 
        summarize(count = n()) %>% 
        ggplot(aes(x=Year.Month, y = count, group = 1)) +
        scale_x_discrete(labels= months) +
        geom_col() +
        xlab("Month") +
        ylab("Number of Headlines") +
        ggtitle("Headline Count by Month (2017)") + 
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_line()

# 2018
corpus[which(corpus$Year == "2018"), ] %>% 
        group_by(Year.Month) %>% 
        summarize(count = n()) %>% 
        ggplot(aes(x=Year.Month, y = count, group = 1)) +
        scale_x_discrete(labels= months) +
        geom_col() +
        xlab("Month") +
        ylab("Number of Headlines") +
        ggtitle("Headline Count by Month (2018)") + 
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_line()

# Create corpus
corpus.tm.full <- VCorpus(VectorSource(corpus$Headline))
# Convert to TDM
corpus.tm.full <- as.matrix(TermDocumentMatrix(corpus.tm.full))

# Word frequency - Before Cleaning
frequent_terms <- data.frame(ST = rownames(corpus.tm.full), 
                             Freq = rowSums(corpus.tm.full), 
                             row.names = NULL)

# Order by frequency
frequent_terms = frequent_terms[order(frequent_terms$Freq, decreasing = TRUE), ] 
frequent_terms$ST <- factor(frequent_terms$ST, levels = frequent_terms$ST[order(frequent_terms$Freq, decreasing = TRUE)])

# Horizontal bar plot
ggplot(data=frequent_terms[1:20, ], aes(x=ST, y=Freq)) +
        geom_col() +
        ggtitle("Word Frequency - Uncleaned Data") +
        ylab("Frequency") + 
        xlab("Word") +
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) +        
        coord_flip()

# Wordcloud - Dirty data
wordcloud(words = frequent_terms$ST, freq = frequent_terms$Freq, max.words = 100, scale=c(2.5,.4))

### Now Clean the Data
#### Clean the test data
corpus.tm.clean <- VCorpus(VectorSource(corpus$Headline))

# Remove urls
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
corpus.tm.clean <- tm_map(corpus.tm.clean, removeURL)
# Remove tickers. These generally start with $ in the headlines
removeTicker <- content_transformer(function(x) gsub("\\$\\w*", "", x))
corpus.tm.clean <- tm_map(corpus.tm.clean, removeTicker)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
# Remove /
corpus.tm.clean <- tm_map(corpus.tm.clean, toSpace, "/")
# Remove @
corpus.tm.clean <- tm_map(corpus.tm.clean, toSpace, "@")
# Remove \\|
corpus.tm.clean <- tm_map(corpus.tm.clean, toSpace, "\\|")
# Remove #
corpus.tm.clean <- tm_map(corpus.tm.clean, toSpace, "#")

# I don't want to exclude negation terms
bespoke_stopwords = c("not", "no", "nor", "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't",
                      "hadn't", "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't",
                      "mustn't", "can't", "cannot", "couldn't")
# Stopwords = default stopwords less those that are in my list above
stopwords_bf = stopwords(kind = "en")[!stopwords(kind="en")%in%bespoke_stopwords]
# I want to include the following as stopwords
stopwords_bespoke = c("twitter", "seekingalpha", "seekingalpha_fs", "seekingalphafs", "benzinga", "inc")
# Final list of stopswords = default list, less negation terms above, plus my bespoke stopwords
stopwords_final = c(stopwords_bespoke, stopwords_bf)

# Remove any remaining punctuation, convert to lower case, etc.
corpus.tm.clean <- corpus.tm.clean %>%
        tm_map(content_transformer(tolower)) %>% 
        tm_map(removePunctuation) %>% # this also removes hashtags
        tm_map(removeWords, stopwords_final) %>%
        tm_map(stripWhitespace) %>%
        tm_map(removeNumbers) %>%
        tm_map(stemDocument, language = "english")

# Negation - replace n't, and not with not_ and nor with nor_
str_negate <- content_transformer(function(x) gsub("not ","not_",gsub("n't ","not_",x), gsub("nor", "nor_", x)))
corpus.tm.clean <- tm_map(corpus.tm.clean, str_negate)

# Save clean corpus for use in other EDA files
saveRDS(corpus.tm.clean, "corpus.tm.clean.rds")

# Convert to Matrix via TDM
corpus.tm.clean.tdm <- as.matrix(TermDocumentMatrix(corpus.tm.clean))

# Word frequency - After Cleaning
frequent_terms_clean <- data.frame(ST = rownames(corpus.tm.clean.tdm), 
                                   Freq = rowSums(corpus.tm.clean.tdm), 
                                   row.names = NULL)

# Order by frequency
frequent_terms_clean = frequent_terms_clean[order(frequent_terms_clean$Freq, decreasing = TRUE), ] 
frequent_terms_clean$ST <- factor(frequent_terms_clean$ST, levels = frequent_terms_clean$ST[order(frequent_terms_clean$Freq, decreasing = TRUE)])

# Horizontal bar plot - clean words
ggplot(data=frequent_terms_clean[1:20, ], aes(x=ST, y=Freq)) +
        ggtitle("Word Frequency - Cleaned Data") +
        geom_col() +
        xlab("Word") + 
        ylab("Frequency")+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) + 
        coord_flip()

