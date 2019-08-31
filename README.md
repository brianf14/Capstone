# Capstone

This repository contains code and csv files in relation to the Capstone project. File descriptions are below.

R Files:  
classifiers.R:  
Contains code for training and testing a SVM classifier using a range of feature extraction, selection and representation methods. The best performing model is saved and used in the return_comparison file. Sentiment scores from SentimentR are also calculated.  

return_comparison.R:  
Takes the predictions of the best performing classifier from the classifiers.R file. Compares returns between stocks and the market following days of positive and negative sentiment. Runs regressions with sentiment score as independent variable and stock return as dependent variable. Calculates a sentiment vs stock return index.  

EDA  
The EDA analysis is split into 4 scripts. The eda-first file needs to be run before the others as this generates a clean training file that is passed to the other scripts. File descriptions:    
eda - first.R: This generates graphs of uncleaned data, cleans the data and then generates graphs of the cleaned data. Focuses on unigrams  
eda - word frequency.R. Generates graphs of the frequency of unigrams in positive vs negative headlines  
eda - bigrams.R. EDA for bigrams  
eda - bns.R. EDA for BNS terms  

CSV Files:  
headlines.csv:  
Dataset of headlines on which sentiment anaylsis is extracted.   
Data description:  
Headline - raw news headline  
Identifier - stock ticker  
Source - news source  
Time - GMT time of news publication  
Date - GMT date of news publication  
FDS Language - Language of news headline  
FDS Subject - News subject  
Add Date - GMT Date that headline was added to FactSet  
Add Time - GMT Time that headline was added to FactSet  
Document Type  
Time Zone - US Time Zone when article published  
US Date - US date that headline was added to FactSet. May differ from GMT date due to time zone differences  
US Time - US EST time of news publication  
Close to Open Return (Rel) - Relative close to open return versus the S&P 500  
Sentiment Tag - 1 indicates positive sentiment. -1 indicates negative sentiment  

close_to_open_abs_returns_25:  
Returns for the 25 stocks and the S&P500 from close to open  

openT1_to_closeT1_abs_return_25:  
Returns for the 25 stocks and the S&P500 from open T+1 to close T+1  

openT1_to_closeT2_abs_return_25:  
Returns for the 25 stocks and the S&P500 from open T+1 to close T+2  

openT1_to_closeT5_abs_return_25:  
Returns for the 25 stocks and the S&P500 from open T+1 to close T+5  

Closing_Prices_25:  
Closing prices for the 25 stocks for 2017 and 2018 
