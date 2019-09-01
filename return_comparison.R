setwd("G:/My Documents/Personal/Masters/Capstone/Code/Final")

# Libraries
library(dplyr)
library(ggplot2) # Charts
library(lubridate) # Date converter
library(caret) # Used for confusion matrix

# Read in data
# Close to Open T1 ABSOLUTE Returns
close_open = read.csv("close_to_open_abs_returns_25.csv", stringsAsFactors = FALSE) # For close to open returns, the return shown for a date is from the close
# of the previous date to the open of that date. For example, the
# return marked as 03/01 is the return from close 02/01 to open 03/01. This ensures that we are comparing the correct sentiment date to return date

# Open T1 to Close T1 ABSOLUTE Returns
open_closeT1 = read.csv("openT1_to_closeT1_abs_return_25.csv", stringsAsFactors = FALSE) # For open to close returns, the return shown for a date is from
# the open of that date. For example, for open_closeT1, the return 
# shown for 03/01 is the return from open 03/01 to close 03/01

# Open T1 to Close T2 ABSOLUTE Returns
open_closeT2 = read.csv("openT1_to_closeT2_abs_return_25.csv", stringsAsFactors = FALSE) # For open to close returns, the return shown for a date is from
# the open of that date. For example, for open_closeT2, the return 
# shown for 03/01 is the return from open 03/01 to close 04/01

open_closeT5 = read.csv("openT1_to_closeT5_abs_return_25.csv", stringsAsFactors = FALSE) # For open to close returns, the return shown for a date is from
# the open of that date. For example, for open_closeT5, the return 
# shown for 03/01 is the return from open 03/01 to close 07/01

# Closing Prices - For use in creating price vs sentiment index
prices = read.csv("Closing_Prices_25.csv", stringsAsFactors = FALSE)

# Convert close_open return data to numeric
close_open[ , 2:27] = sapply(close_open[ ,2:27], function(x) as.numeric(x))
open_closeT1[ , 2:27] = sapply(open_closeT1[ ,2:27], function(x) as.numeric(x))
open_closeT2[ , 2:27] = sapply(open_closeT2[ ,2:27], function(x) as.numeric(x))
open_closeT5[ , 2:27] = sapply(open_closeT5[ ,2:27], function(x) as.numeric(x))

# Convert NAs to zeros
close_open[is.na(close_open)] = 0
open_closeT1[is.na(open_closeT1)] = 0
open_closeT2[is.na(open_closeT2)] = 0
open_closeT5[is.na(open_closeT5)] = 0

# Change the Date column from character to Date
close_open$Date = dmy(close_open$Date)
open_closeT1$Date = dmy(open_closeT1$Date)
open_closeT2$Date = dmy(open_closeT2$Date)
open_closeT5$Date = dmy(open_closeT5$Date)
prices$Date = dmy(prices$Date)

# Cut the data to match the same period as sentiment
start_date = "2018-01-02"
end_date = "2018-12-31"
# Find row related to start & end date - Close to Open Returns
start_date_row = which(close_open$Date == start_date, arr.ind=TRUE)
end_date_row = which(close_open$Date == end_date, arr.ind=TRUE)

close_open = close_open[start_date_row:end_date_row, ]

# Find row related to start date - Open to CloseT+1 Returns
start_date_row_co = which(open_closeT1$Date == start_date, arr.ind=TRUE)
end_date_row_co = which(open_closeT1$Date == end_date, arr.ind=TRUE)

open_closeT1 = open_closeT1[start_date_row_co:end_date_row_co, ]

# Find row related to start date - Open to CloseT+2 Returns
start_date_row_co = which(open_closeT2$Date == start_date, arr.ind=TRUE)
end_date_row_co = which(open_closeT2$Date == end_date, arr.ind=TRUE)

open_closeT2 = open_closeT2[start_date_row_co:end_date_row_co, ]

# Find row related to start date - Open to CloseT+5 Returns
start_date_row_co = which(open_closeT5$Date == start_date, arr.ind=TRUE)
end_date_row_co = which(open_closeT5$Date == end_date, arr.ind=TRUE)

open_closeT5 = open_closeT5[start_date_row_co:end_date_row_co, ]

# Cut the prices data to match the same period as sentiment
start_date = "2018-01-02"
end_date = "2018-12-31"
# Find row related to start & end date - Close to Close Returns
start_date_row = which(prices$Date == start_date, arr.ind=TRUE)
end_date_row = which(prices$Date == end_date, arr.ind=TRUE)

prices = prices[start_date_row:end_date_row, ]

######## Load model predictions
model_predictions = readRDS("model_predictions.rds")
# Change col names
colnames(model_predictions)[1] = "Model.Prediction"
colnames(model_predictions)[15] = "Actual.Sentiment"

# Convert factors to numeric
model_predictions$Model.Prediction = as.numeric(model_predictions$Model.Prediction)
model_predictions$Actual.Sentiment = as.numeric(model_predictions$Actual.Sentiment)

# The 1s are converted to 2s and -1s to 1s so we need to change these back
# Convert 1s back to -1s
model_predictions$Model.Prediction[model_predictions$Model.Prediction==1] <- -1
model_predictions$Actual.Sentiment[model_predictions$Actual.Sentiment==1] <- -1
# Coonvert 2s back to 1s
model_predictions$Model.Prediction[model_predictions$Model.Prediction==2] <- 1
model_predictions$Actual.Sentiment[model_predictions$Actual.Sentiment==2] <- 1

# Add day of week
model_predictions$day = weekdays(as.Date(model_predictions$US.Date))

# Move Saturday to Monday
for(i in 1:(nrow(model_predictions))){
        if (model_predictions[i, 16] == "Saturday"){
                model_predictions[i, 12] = model_predictions[i, 12] + 2 # Move Saturday's date and time to pre open Monday
                model_predictions[i, 13] = "06:00"
        }
}

# Move Sunday scores to the next day
for(i in 1:(nrow(model_predictions))){
        if (model_predictions[i, 16] == "Sunday"){
                model_predictions[i, 12] = model_predictions[i, 12] + 1 # Move Saturday's date and time to pre open Monday
                model_predictions[i, 13] = "06:00"
        }
}

# Change the day of the week again based on the new dates
model_predictions$day = weekdays(as.Date(model_predictions$US.Date))

# Include an abs count of articles
model_predictions$article.count = abs(model_predictions$Model.Prediction)

# Change the US.Time to a time using lubridate
model_predictions$US.Time = hm(model_predictions$US.Time)

# If an articles is published after 16:00 (market close) then the relevant date for comparing returns
# is from the open of the next day
# Change the date depending on article publication time
for(i in 1:(nrow(model_predictions))){
        if (model_predictions[i, 13] > hm("16:00")){
                model_predictions[i, 12] = model_predictions[i, 12] + 1 #
                
        }
}

# Check days again as Friday's might have moved to Saturdays
model_predictions$day = weekdays(as.Date(model_predictions$US.Date))

# Move any new Saturdays to Monday
for(i in 1:(nrow(model_predictions))){
        if (model_predictions[i, 16] == "Saturday"){
                model_predictions[i, 12] = model_predictions[i, 12] + 2 # Move Saturday's date and time to pre open Monday
                model_predictions[i, 13] = hm("06:00")
        }
}

# Check days again as Friday's might have moved to Saturdays
model_predictions$day = weekdays(as.Date(model_predictions$US.Date))

########################################### ----------------------------  INDEX LEVEL ------------------------------------------###########################
####### Group sentiment count by date - ALL STOCKS TAKEN AS AN INDEX !!!!!!!
# # Predicted sentiment
sentiment.count = aggregate(model_predictions$Model.Prediction, by=list(Category=model_predictions$US.Date), FUN=sum)
# # Actual sentiment
actual.sentiment.count = aggregate(model_predictions$Actual.Sentiment, by=list(Category=model_predictions$US.Date), FUN=sum)
# # Change column names
colnames(sentiment.count) = c("Date", "Pred.Sentiment.Count")
colnames(actual.sentiment.count) = c("Date", "Actual.Sentiment.Count")
# # Include a count of total articles
total.articles.sent = aggregate(model_predictions$article.count, by=list(Category=model_predictions$US.Date), FUN=sum)
colnames(total.articles.sent) = c("Date", "Total.Articles")

# Order by Date
sentiment.count = sentiment.count[order(sentiment.count$Date),]
actual.sentiment.count = actual.sentiment.count[order(actual.sentiment.count$Date),]
total.articles.sent = total.articles.sent[order(total.articles.sent$Date),]

# Combine predicted & actual sentiments into one DF
sentiment = cbind(sentiment.count, actual.sentiment.count, total.articles.sent)
# Remove the additional date column
sentiment = sentiment[ , -c(3,5)]

# Create column that shows overall daily sentiment for actual sentiment
for (i in 1:nrow(sentiment)){
        if(sentiment[i, 3] >= 1){
                sentiment$actual.sentiment.daily[i] = 1
        }
        else if(sentiment[i, 3] == 0){
                sentiment$actual.sentiment.daily[i] = 0
        }
        else if(sentiment[i, 3] <= -1){
                sentiment$actual.sentiment.daily[i] = -1
        }
}

# Create column that shows overall daily sentiment for predicted sentiment
for (i in 1:nrow(sentiment)){
        if(sentiment[i, 2] >= 1){
                sentiment$pred.sentiment.daily[i] = 1
        }
        else if(sentiment[i, 2] == 0){
                sentiment$pred.sentiment.daily[i] = 0
        }
        else if(sentiment[i, 2] <= -1){
                sentiment$pred.sentiment.daily[i] = -1
        }
}

# Create column that shows daily continuous sentiment score for predictions
for (i in 1:nrow(sentiment)){
        sentiment$sentiment.score.daily[i] = sentiment$Pred.Sentiment.Count[i] / sentiment$Total.Articles[i]
}

### Calculate average CLOSE to OPEN return for all 25 stocks
return.avg.co = close_open[ ,3:27]
return.avg.co$average.return = apply(return.avg.co, 1, FUN = mean)
return.avg.co$Date = close_open$Date
# Rearrange Date column to first column
return.avg.co = return.avg.co[ ,c(27, 1:26)]
# Insert S&P500 returns
return.avg.co$SP500 = close_open$SP500
# Difference between average return on 25 stocks & S&P500
return.avg.co$return.differential = return.avg.co$average.return - return.avg.co$SP500

# Merge the CLOSE to OPEN returns DF with the sentiment DF based on the sentiment dates
merged_df.co = merge(sentiment, return.avg.co[ ,c(1, 27:29)], by = "Date")

# Check if differences in CLOSE to OPEN returns are statistically significant
# Positive Sentiment
t.test.all.pos = t.test(merged_df.co[which(merged_df.co$pred.sentiment.daily == 1), ]$average.return, 
                        merged_df.co[which(merged_df.co$pred.sentiment.daily == 1), ]$SP500, alternative = "greater")

# Negative
t.test.all.neg = t.test(merged_df.co[which(merged_df.co$pred.sentiment.daily == -1), ]$average.return, 
                        merged_df.co[which(merged_df.co$pred.sentiment.daily == -1), ]$SP500, alternative = "less")

### Calculate average OPEN to CLOSE T+1 return for all 25 stocks
return.avg.open_closeT1 = open_closeT1[ ,3:27]
return.avg.open_closeT1$average.return = apply(return.avg.open_closeT1, 1, FUN = mean)
return.avg.open_closeT1$Date = open_closeT1$Date
# Rearrange Date column to first column
return.avg.open_closeT1 = return.avg.open_closeT1[ ,c(27, 1:26)]
# Insert S&P500 returns
return.avg.open_closeT1$SP500 = open_closeT1$SP500
# Difference between average return on 25 stocks & S&P500
return.avg.open_closeT1$return.differential = return.avg.open_closeT1$average.return - return.avg.open_closeT1$SP500

# Merge the OPEN to CLOSE T1 returns DF with the sentiment DF based on the sentiment dates
merged_df.open_closeT1 = merge(sentiment, return.avg.open_closeT1[ ,c(1, 27:29)], by = "Date")

# Check if differences in OPEN to CLOSE T+1 returns are statistically significant
# Positive Sentiment
t.test.all.pos.T1 = t.test(merged_df.open_closeT1[which(merged_df.open_closeT1$pred.sentiment.daily == 1), ]$average.return, 
                           merged_df.open_closeT1[which(merged_df.open_closeT1$pred.sentiment.daily == 1), ]$SP500, alternative = "greater")

# Negative
t.test.all.neg.T1 = t.test(merged_df.open_closeT1[which(merged_df.open_closeT1$pred.sentiment.daily == -1), ]$average.return, 
                           merged_df.open_closeT1[which(merged_df.open_closeT1$pred.sentiment.daily == -1), ]$SP500, alternative = "less")


### Calculate average OPEN to CLOSE T+2 return for all 25 stocks
return.avg.open_closeT2 = open_closeT2[ ,3:27]
return.avg.open_closeT2$average.return = apply(return.avg.open_closeT2, 1, FUN = mean)
return.avg.open_closeT2$Date = open_closeT2$Date
# Rearrange Date column to first column
return.avg.open_closeT2 = return.avg.open_closeT2[ ,c(27, 1:26)]
# Insert S&P500 returns
return.avg.open_closeT2$SP500 = open_closeT2$SP500
# Difference between average return on 25 stocks & S&P500
return.avg.open_closeT2$return.differential = return.avg.open_closeT2$average.return - return.avg.open_closeT2$SP500

# Merge the OPEN to CLOSE T+2 returns DF with the sentiment DF based on the sentiment dates
merged_df.open_closeT2 = merge(sentiment, return.avg.open_closeT2[ ,c(1, 27:29)], by = "Date")

# Check if differences in OPEN to CLOSE T+2 returns are statistically significant
# Positive Sentiment
t.test.all.pos.T2 = t.test(merged_df.open_closeT2[which(merged_df.open_closeT2$pred.sentiment.daily == 1), ]$average.return, 
                           merged_df.open_closeT2[which(merged_df.open_closeT2$pred.sentiment.daily == 1), ]$SP500, alternative = "greater")

# Negative
t.test.all.neg.T2 = t.test(merged_df.open_closeT2[which(merged_df.open_closeT2$pred.sentiment.daily == -1), ]$average.return, 
                           merged_df.open_closeT2[which(merged_df.open_closeT2$pred.sentiment.daily == -1), ]$SP500, alternative = "less")


### Calculate average OPEN to CLOSE T+5 return for all 25 stocks
return.avg.open_closeT5 = open_closeT5[ ,3:27]
return.avg.open_closeT5$average.return = apply(return.avg.open_closeT5, 1, FUN = mean)
return.avg.open_closeT5$Date = open_closeT5$Date
# Rearrange Date column to first column
return.avg.open_closeT5 = return.avg.open_closeT5[ ,c(27, 1:26)]
# Insert S&P500 returns
return.avg.open_closeT5$SP500 = open_closeT5$SP500
# Difference between average return on 25 stocks & S&P500
return.avg.open_closeT5$return.differential = return.avg.open_closeT5$average.return - return.avg.open_closeT5$SP500

# Merge the OPEN to CLOSE T+5 returns DF with the sentiment DF based on the sentiment dates
merged_df.open_closeT5 = merge(sentiment, return.avg.open_closeT5[ ,c(1, 27:29)], by = "Date")

# Check if differences in OPEN to CLOSE T+5 returns are statistically significant
# Positive Sentiment
t.test.all.pos.T5 = t.test(merged_df.open_closeT5[which(merged_df.open_closeT5$pred.sentiment.daily == 1), ]$average.return, 
                           merged_df.open_closeT5[which(merged_df.open_closeT5$pred.sentiment.daily == 1), ]$SP500, alternative = "greater")

# Negative
t.test.all.neg.T5 = t.test(merged_df.open_closeT5[which(merged_df.open_closeT5$pred.sentiment.daily == -1), ]$average.return, 
                           merged_df.open_closeT5[which(merged_df.open_closeT5$pred.sentiment.daily == -1), ]$SP500, alternative = "less")

###################################  REGRESSION OF SENTIMENT SCORE ON AVERAGE RETURNS
# CLOSE TO OPEN T+1
reg.co = lm(average.return ~ sentiment.score.daily, data = merged_df.co)
reg.co
summary(reg.co)

# OPEN TO CLOSE T+1
reg.closeT1 = lm(average.return ~ sentiment.score.daily, data = merged_df.open_closeT1)
reg.closeT1
summary(reg.closeT1)

# OPEN TO CLOSE T+2
reg.closeT2 = lm(average.return ~ sentiment.score.daily, data = merged_df.open_closeT2)
reg.closeT2
summary(reg.closeT2)

# OPEN TO CLOSE T+5
reg.closeT5 = lm(average.return ~ sentiment.score.daily, data = merged_df.open_closeT5)
reg.closeT5
summary(reg.closeT5)

################# LOOKING AT INDIVIDUAL PREDICTIONS 
# Tickers
tickers = c("MSFT-US", "AAPL-US", "AMZN-US", "BRK.B-US", "FB-US", "JNJ-US", "GOOGL-US", "XOM-US",
            "UNH-US", "PFE-US", "V-US", "VZ-US","PG-US", "T-US", "INTC-US", "CVX-US", "SBAC-US", "ROK-US",
            "CLX-US", "FE-US", "MAT-US", "UAA-US", "NFX-US", "COTY-US", "NWS-US")

# Subset model predictions for stock specific headlines
for(i in tickers){
        nam <- paste("model_predictions.", i, sep = "")
        assign(nam, model_predictions[which(model_predictions$Identifier == i), ])
}

# Aggregate by Identifier and date and sum predicted sentiment and article count
agg.df = aggregate(cbind(model_predictions$Model.Prediction, model_predictions$article.count) ~ Identifier + US.Date, data = model_predictions, sum)
colnames(agg.df)[2:4] = c("Date","pred.sentiment.count", "total.articles")

# Include daily sentiment indicator
for (i in 1:nrow(agg.df)){
        if(agg.df[i, 3] >= 1){
                agg.df$pred.sentiment.daily[i] = 1
        }
        else if(agg.df[i, 3] <= -1){
                agg.df$pred.sentiment.daily[i] = -1
        }
        else{
                agg.df$pred.sentiment.daily[i] = 0
        }
}

# Create column that shows daily sentiment score for predictions
for (i in 1:nrow(agg.df)){
        agg.df$sentiment.score.daily[i] = agg.df$pred.sentiment.count[i] / agg.df$total.articles[i]
}

# Take out the individual stocks
for(i in tickers){
        nam <- paste("sentiment.", i, sep = "")
        assign(nam, agg.df[which(agg.df$Identifier == i), ])
}

# Calculate CLOSE TO OPEN return differential for each stock vs S&P
for(i in 3:27){
        for(j in 1:nrow(close_open)){
                close_open[j, i+25] = close_open[j, i] - close_open[j, 2]
        }
        
}

# Column names - CLOSE TO OPEN T1
colnames(close_open)[28:52] = c("MSFT-US.rel.return.co", "AAPL-US.rel.return.co", "AMZN-US.rel.return.co", "BRK.B-US.rel.return.co", "FB-US.rel.return.co", 
                                "JNJ-US.rel.return.co", "GOOGL-US.rel.return.co", "XOM-US.rel.return.co","UNH-US.rel.return.co", 
                                "PFE-US.rel.return.co", "V-US.rel.return.co", "VZ-US.rel.return.co","PG-US.rel.return.co", 
                                "T-US.rel.return.co", "INTC-US.rel.return.co", "CVX-US.rel.return.co", 
                                "SBAC-US.rel.return.co", "ROK-US.rel.return.co","CLX-US.rel.return.co", "FE-US.rel.return.co", 
                                "MAT-US.rel.return.co", "UAA-US.rel.return.co", "NFX-US.rel.return.co", "COTY-US.rel.return.co", "NWS-US.rel.return.co")

# Calculate OPEN TO CLOSE T+1 return differential for each stock vs S&P
for(i in 3:27){
        for(j in 1:nrow(open_closeT1)){
                open_closeT1[j, i+25] = open_closeT1[j, i] - open_closeT1[j, 2]
        }
        
}

# Column names - OPEN TO CLOSET+1
colnames(open_closeT1)[28:52] = c("MSFT-US.rel.return.t1", "AAPL-US.rel.return.t1", "AMZN-US.rel.return.t1", "BRK.B-US.rel.return.t1", "FB-US.rel.return.t1", 
                                  "JNJ-US.rel.return.t1", "GOOGL-US.rel.return.t1", "XOM-US.rel.return.t1","UNH-US.rel.return.t1", 
                                  "PFE-US.rel.return.t1", "V-US.rel.return.t1", "VZ-US.rel.return.t1","PG-US.rel.return.t1", 
                                  "T-US.rel.return.t1", "INTC-US.rel.return.t1", "CVX-US.rel.return.t1", 
                                  "SBAC-US.rel.return.t1", "ROK-US.rel.return.t1","CLX-US.rel.return.t1", "FE-US.rel.return.t1", 
                                  "MAT-US.rel.return.t1", "UAA-US.rel.return.t1", "NFX-US.rel.return.t1", "COTY-US.rel.return.t1", "NWS-US.rel.return.t1")


# Calculate OPEN TO CLOSE T+2 return differential for each stock vs S&P
for(i in 3:27){
        for(j in 1:nrow(open_closeT2)){
                open_closeT2[j, i+25] = open_closeT2[j, i] - open_closeT2[j, 2]
        }
        
}

# Column names - OPEN TO CLOSE T+2
colnames(open_closeT2)[28:52] = c("MSFT-US.rel.return.t2", "AAPL-US.rel.return.t2", "AMZN-US.rel.return.t2", "BRK.B-US.rel.return.t2", "FB-US.rel.return.t2", 
                                  "JNJ-US.rel.return.t2", "GOOGL-US.rel.return.t2", "XOM-US.rel.return.t2","UNH-US.rel.return.t2", 
                                  "PFE-US.rel.return.t2", "V-US.rel.return.t2", "VZ-US.rel.return.t2","PG-US.rel.return.t2", 
                                  "T-US.rel.return.t2", "INTC-US.rel.return.t2", "CVX-US.rel.return.t2", 
                                  "SBAC-US.rel.return.t2", "ROK-US.rel.return.t2","CLX-US.rel.return.t2", "FE-US.rel.return.t2", 
                                  "MAT-US.rel.return.t2", "UAA-US.rel.return.t2", "NFX-US.rel.return.t2", "COTY-US.rel.return.t2", "NWS-US.rel.return.t2")


# Calculate OPEN TO CLOSE T+5 return differential for each stock vs S&P
for(i in 3:27){
        for(j in 1:nrow(open_closeT5)){
                open_closeT5[j, i+25] = open_closeT5[j, i] - open_closeT5[j, 2]
        }
        
}

# Column names - OPEN TO CLOSET+5
colnames(open_closeT5)[28:52] = c("MSFT-US.rel.return.T5", "AAPL-US.rel.return.T5", "AMZN-US.rel.return.T5", "BRK.B-US.rel.return.T5", "FB-US.rel.return.T5", 
                                  "JNJ-US.rel.return.T5", "GOOGL-US.rel.return.T5", "XOM-US.rel.return.T5","UNH-US.rel.return.T5", 
                                  "PFE-US.rel.return.T5", "V-US.rel.return.T5", "VZ-US.rel.return.T5","PG-US.rel.return.T5", 
                                  "T-US.rel.return.T5", "INTC-US.rel.return.T5", "CVX-US.rel.return.T5", 
                                  "SBAC-US.rel.return.T5", "ROK-US.rel.return.T5","CLX-US.rel.return.T5", "FE-US.rel.return.T5", 
                                  "MAT-US.rel.return.T5", "UAA-US.rel.return.T5", "NFX-US.rel.return.T5", "COTY-US.rel.return.T5", "NWS-US.rel.return.T5")

# Take out Individual Close to Open Returns
for(i in 3:27){
        nam <- paste("returns.co.", i, sep = "")
        assign(nam, close_open[ , c(1,2, i, i+25)])
}

# Take out Individual OPEN to CLOSE T+1 Returns
for(i in 3:27){
        nam <- paste("returns.closeT1", i, sep = "")
        assign(nam, open_closeT1[ , c(1,2, i, i+25)])
}

# Take out Individual OPEN to CLOSE T+2 Returns
for(i in 3:27){
        nam <- paste("returns.closeT2", i, sep = "")
        assign(nam, open_closeT2[ , c(1,2, i, i+25)])
}

# Take out Individual OPEN to CLOSE T+5 Returns
for(i in 3:27){
        nam <- paste("returns.closeT5", i, sep = "")
        assign(nam, open_closeT5[ , c(1,2, i, i+25)])
}

# Merge the CLOSE to OPEN returns DFs with the sentiment DFs based on the sentiment dates
merged_df.co.msft = merge(`sentiment.MSFT-US`, returns.co.3, by = "Date")
merged_df.co.aapl = merge(`sentiment.AAPL-US`, returns.co.4, by = "Date")
merged_df.co.amzn = merge(`sentiment.AMZN-US`, returns.co.5, by = "Date")
merged_df.co.brk = merge(`sentiment.BRK.B-US`, returns.co.6, by = "Date")
merged_df.co.fb = merge(`sentiment.FB-US`, returns.co.7, by = "Date")
merged_df.co.jnj = merge(`sentiment.JNJ-US`, returns.co.8, by = "Date")
merged_df.co.googl = merge(`sentiment.GOOGL-US`, returns.co.9, by = "Date")
merged_df.co.xom = merge(`sentiment.XOM-US`, returns.co.10, by = "Date")
merged_df.co.unh = merge(`sentiment.UNH-US`, returns.co.11, by = "Date")
merged_df.co.pfe = merge(`sentiment.PFE-US`, returns.co.12, by = "Date")
merged_df.co.v = merge(`sentiment.V-US`, returns.co.13, by = "Date")
merged_df.co.vz = merge(`sentiment.VZ-US`, returns.co.14, by = "Date")
merged_df.co.pg = merge(`sentiment.PG-US`, returns.co.15, by = "Date")
merged_df.co.t = merge(`sentiment.T-US`, returns.co.16, by = "Date")
merged_df.co.intc = merge(`sentiment.INTC-US`, returns.co.17, by = "Date")
merged_df.co.cvx = merge(`sentiment.CVX-US`, returns.co.18, by = "Date")
merged_df.co.sbac = merge(`sentiment.SBAC-US`, returns.co.19, by = "Date")
merged_df.co.rok = merge(`sentiment.ROK-US`, returns.co.20, by = "Date")
merged_df.co.clx = merge(`sentiment.CLX-US`, returns.co.21, by = "Date")
merged_df.co.fe = merge(`sentiment.FE-US`, returns.co.22, by = "Date")
merged_df.co.mat = merge(`sentiment.MAT-US`, returns.co.23, by = "Date")
merged_df.co.uaa = merge(`sentiment.UAA-US`, returns.co.24, by = "Date")
merged_df.co.nfx = merge(`sentiment.NFX-US`, returns.co.25, by = "Date")
merged_df.co.coty = merge(`sentiment.COTY-US`, returns.co.26, by = "Date")
merged_df.co.nws = merge(`sentiment.NWS-US`, returns.co.27, by = "Date")

# Merge the OPEN to CLOSE T+1 returns DFs with the sentiment DFs based on the sentiment dates
merged_df.t1.msft = merge(`sentiment.MSFT-US`, returns.closeT13, by = "Date")
merged_df.t1.aapl = merge(`sentiment.AAPL-US`, returns.closeT14, by = "Date")
merged_df.t1.amzn = merge(`sentiment.AMZN-US`, returns.closeT15, by = "Date")
merged_df.t1.brk = merge(`sentiment.BRK.B-US`, returns.closeT16, by = "Date")
merged_df.t1.fb = merge(`sentiment.FB-US`, returns.closeT17, by = "Date")
merged_df.t1.jnj = merge(`sentiment.JNJ-US`, returns.closeT18, by = "Date")
merged_df.t1.googl = merge(`sentiment.GOOGL-US`, returns.closeT19, by = "Date")
merged_df.t1.xom = merge(`sentiment.XOM-US`, returns.closeT110, by = "Date")
merged_df.t1.unh = merge(`sentiment.UNH-US`, returns.closeT111, by = "Date")
merged_df.t1.pfe = merge(`sentiment.PFE-US`, returns.closeT112, by = "Date")
merged_df.t1.v = merge(`sentiment.V-US`, returns.closeT113, by = "Date")
merged_df.t1.vz = merge(`sentiment.VZ-US`, returns.closeT114, by = "Date")
merged_df.t1.pg = merge(`sentiment.PG-US`, returns.closeT115, by = "Date")
merged_df.t1.t = merge(`sentiment.T-US`, returns.closeT116, by = "Date")
merged_df.t1.intc = merge(`sentiment.INTC-US`, returns.closeT117, by = "Date")
merged_df.t1.cvx = merge(`sentiment.CVX-US`, returns.closeT118, by = "Date")
merged_df.t1.sbac = merge(`sentiment.SBAC-US`, returns.closeT119, by = "Date")
merged_df.t1.rok = merge(`sentiment.ROK-US`, returns.closeT120, by = "Date")
merged_df.t1.clx = merge(`sentiment.CLX-US`, returns.closeT121, by = "Date")
merged_df.t1.fe = merge(`sentiment.FE-US`, returns.closeT122, by = "Date")
merged_df.t1.mat = merge(`sentiment.MAT-US`, returns.closeT123, by = "Date")
merged_df.t1.uaa = merge(`sentiment.UAA-US`, returns.closeT124, by = "Date")
merged_df.t1.nfx = merge(`sentiment.NFX-US`, returns.closeT125, by = "Date")
merged_df.t1.coty = merge(`sentiment.COTY-US`, returns.closeT126, by = "Date")
merged_df.t1.nws = merge(`sentiment.NWS-US`, returns.closeT127, by = "Date")

# Merge the OPEN to CLOSE T+2 returns DFs with the sentiment DFs based on the sentiment dates
merged_df.t2.msft = merge(`sentiment.MSFT-US`, returns.closeT23, by = "Date")
merged_df.t2.aapl = merge(`sentiment.AAPL-US`, returns.closeT24, by = "Date")
merged_df.t2.amzn = merge(`sentiment.AMZN-US`, returns.closeT25, by = "Date")
merged_df.t2.brk = merge(`sentiment.BRK.B-US`, returns.closeT26, by = "Date")
merged_df.t2.fb = merge(`sentiment.FB-US`, returns.closeT27, by = "Date")
merged_df.t2.jnj = merge(`sentiment.JNJ-US`, returns.closeT28, by = "Date")
merged_df.t2.googl = merge(`sentiment.GOOGL-US`, returns.closeT29, by = "Date")
merged_df.t2.xom = merge(`sentiment.XOM-US`, returns.closeT210, by = "Date")
merged_df.t2.unh = merge(`sentiment.UNH-US`, returns.closeT211, by = "Date")
merged_df.t2.pfe = merge(`sentiment.PFE-US`, returns.closeT212, by = "Date")
merged_df.t2.v = merge(`sentiment.V-US`, returns.closeT213, by = "Date")
merged_df.t2.vz = merge(`sentiment.VZ-US`, returns.closeT214, by = "Date")
merged_df.t2.pg = merge(`sentiment.PG-US`, returns.closeT215, by = "Date")
merged_df.t2.t = merge(`sentiment.T-US`, returns.closeT216, by = "Date")
merged_df.t2.intc = merge(`sentiment.INTC-US`, returns.closeT217, by = "Date")
merged_df.t2.cvx = merge(`sentiment.CVX-US`, returns.closeT218, by = "Date")
merged_df.t2.sbac = merge(`sentiment.SBAC-US`, returns.closeT219, by = "Date")
merged_df.t2.rok = merge(`sentiment.ROK-US`, returns.closeT220, by = "Date")
merged_df.t2.clx = merge(`sentiment.CLX-US`, returns.closeT221, by = "Date")
merged_df.t2.fe = merge(`sentiment.FE-US`, returns.closeT222, by = "Date")
merged_df.t2.mat = merge(`sentiment.MAT-US`, returns.closeT223, by = "Date")
merged_df.t2.uaa = merge(`sentiment.UAA-US`, returns.closeT224, by = "Date")
merged_df.t2.nfx = merge(`sentiment.NFX-US`, returns.closeT225, by = "Date")
merged_df.t2.coty = merge(`sentiment.COTY-US`, returns.closeT226, by = "Date")
merged_df.t2.nws = merge(`sentiment.NWS-US`, returns.closeT227, by = "Date")

# Merge the OPEN to CLOSE T+5 returns DFs with the sentiment DFs based on the sentiment dates
merged_df.T5.msft = merge(`sentiment.MSFT-US`, returns.closeT53, by = "Date")
merged_df.T5.aapl = merge(`sentiment.AAPL-US`, returns.closeT54, by = "Date")
merged_df.T5.amzn = merge(`sentiment.AMZN-US`, returns.closeT55, by = "Date")
merged_df.T5.brk = merge(`sentiment.BRK.B-US`, returns.closeT56, by = "Date")
merged_df.T5.fb = merge(`sentiment.FB-US`, returns.closeT57, by = "Date")
merged_df.T5.jnj = merge(`sentiment.JNJ-US`, returns.closeT58, by = "Date")
merged_df.T5.googl = merge(`sentiment.GOOGL-US`, returns.closeT59, by = "Date")
merged_df.T5.xom = merge(`sentiment.XOM-US`, returns.closeT510, by = "Date")
merged_df.T5.unh = merge(`sentiment.UNH-US`, returns.closeT511, by = "Date")
merged_df.T5.pfe = merge(`sentiment.PFE-US`, returns.closeT512, by = "Date")
merged_df.T5.v = merge(`sentiment.V-US`, returns.closeT513, by = "Date")
merged_df.T5.vz = merge(`sentiment.VZ-US`, returns.closeT514, by = "Date")
merged_df.T5.pg = merge(`sentiment.PG-US`, returns.closeT515, by = "Date")
merged_df.T5.t = merge(`sentiment.T-US`, returns.closeT516, by = "Date")
merged_df.T5.intc = merge(`sentiment.INTC-US`, returns.closeT517, by = "Date")
merged_df.T5.cvx = merge(`sentiment.CVX-US`, returns.closeT518, by = "Date")
merged_df.T5.sbac = merge(`sentiment.SBAC-US`, returns.closeT519, by = "Date")
merged_df.T5.rok = merge(`sentiment.ROK-US`, returns.closeT520, by = "Date")
merged_df.T5.clx = merge(`sentiment.CLX-US`, returns.closeT521, by = "Date")
merged_df.T5.fe = merge(`sentiment.FE-US`, returns.closeT522, by = "Date")
merged_df.T5.mat = merge(`sentiment.MAT-US`, returns.closeT523, by = "Date")
merged_df.T5.uaa = merge(`sentiment.UAA-US`, returns.closeT524, by = "Date")
merged_df.T5.nfx = merge(`sentiment.NFX-US`, returns.closeT525, by = "Date")
merged_df.T5.coty = merge(`sentiment.COTY-US`, returns.closeT526, by = "Date")
merged_df.T5.nws = merge(`sentiment.NWS-US`, returns.closeT527, by = "Date")

# DFs of CLOSE to OPEN T+1
my_dfs = list(merged_df.co.msft, merged_df.co.aapl, merged_df.co.amzn, merged_df.co.brk,
              merged_df.co.fb, merged_df.co.jnj, merged_df.co.googl, merged_df.co.xom,
              merged_df.co.unh, merged_df.co.pfe, merged_df.co.v, merged_df.co.vz,
              merged_df.co.pg, merged_df.co.t, merged_df.co.intc, merged_df.co.cvx,
              merged_df.co.sbac, merged_df.co.rok, merged_df.co.clx, merged_df.co.fe,
              merged_df.co.mat, merged_df.co.uaa, merged_df.co.nfx, merged_df.co.coty,
              merged_df.co.nws)

# Function to calculate t-stats for positive returns
my_fun_tvalue.pos = function(x){
        t.test(x[which(x[,5] == 1), ][,8], 
               x[which(x[ ,5] == 1), ][,7], alternative = "greater")
        
}

# Function to calculate t-stats for negative returns
my_fun_tvalue.neg = function(x){
        t.test(x[which(x[,5] == -1), ][,8], 
               x[which(x[ ,5] == -1), ][,7], alternative = "less")
        
}

# Pass the list of CLOSE TO OPEN dataframes to the function for positive returns
a = my_dfs %>%
        lapply(my_fun_tvalue.pos)

# Pass the list of CLOSE TO OPEN dataframes to the function for negative returns
b = my_dfs %>%
        lapply(my_fun_tvalue.neg)


# Unwrap the listed CLOSE TO OPEN DFs for positive return t-values
for(i in 1:25){
        nam = paste("new_df.pos", i, sep = "")
        assign(nam, unlist(a[i])[1:10])
}

# Unwrap the listed CLOSE TO OPEN DFs for negative return t-values
for(i in 1:25){
        nam = paste("new_df.neg", i, sep = "")
        assign(nam, unlist(b[i])[1:10])
}


# Combine negative CLOSE TO OPEN into one DF
t_close_open.neg.all = cbind(new_df.neg1, new_df.neg2, new_df.neg3, new_df.neg4, new_df.neg5, new_df.neg6,
                             new_df.neg7, new_df.neg8, new_df.neg9, new_df.neg10, new_df.neg11, new_df.neg12,
                             new_df.neg13, new_df.neg14, new_df.neg15, new_df.neg16, new_df.neg17, new_df.neg18,
                             new_df.neg19, new_df.neg20, new_df.neg21, new_df.neg22, new_df.neg23, new_df.neg24,
                             new_df.neg25)

# Change col names
colnames(t_close_open.neg.all) = tickers

# Combine positive CLOSE TO OPEN into one DF
t_close_open.pos.all = cbind(new_df.pos1, new_df.pos2, new_df.pos3, new_df.pos4, new_df.pos5, new_df.pos6,
                             new_df.pos7, new_df.pos8, new_df.pos9, new_df.pos10, new_df.pos11, new_df.pos12,
                             new_df.pos13, new_df.pos14, new_df.pos15, new_df.pos16, new_df.pos17, new_df.pos18,
                             new_df.pos19, new_df.pos20, new_df.pos21, new_df.pos22, new_df.pos23, new_df.pos24,
                             new_df.pos25)

# Change col names
colnames(t_close_open.pos.all) = tickers

# List of OPEN To CLOSE T+1 DFs
my_dfs_open_closeT1 = list(merged_df.t1.msft, merged_df.t1.aapl, merged_df.t1.amzn, merged_df.t1.brk,
                           merged_df.t1.fb, merged_df.t1.jnj, merged_df.t1.googl, merged_df.t1.xom,
                           merged_df.t1.unh, merged_df.t1.pfe, merged_df.t1.v, merged_df.t1.vz,
                           merged_df.t1.pg, merged_df.t1.t, merged_df.t1.intc, merged_df.t1.cvx,
                           merged_df.t1.sbac, merged_df.t1.rok, merged_df.t1.clx, merged_df.t1.fe,
                           merged_df.t1.mat, merged_df.t1.uaa, merged_df.t1.nfx, merged_df.t1.coty,
                           merged_df.t1.nws)

# Pass the list of OPEN TO CLOSE T+1 dataframes to the function for positive returns
t_open_closeT1.pos = my_dfs_open_closeT1 %>%
        lapply(my_fun_tvalue.pos)

# Pass the list of OPEN TO CLOSE T+1 dataframes to the function for negative returns
t_open_closeT1.neg = my_dfs_open_closeT1 %>%
        lapply(my_fun_tvalue.neg)

# Unwrap the listed OPEN TO CLOSE T+1 DFs for positive return t-values
for(i in 1:25){
        nam = paste("t_open_closeT1.pos", i, sep = "")
        assign(nam, unlist(t_open_closeT1.pos[i])[1:10])
}

# Unwrap the listed OPEN TO CLOSE T+1 DFs for negative return t-values
for(i in 1:25){
        nam = paste("t_open_closeT1.neg", i, sep = "")
        assign(nam, unlist(t_open_closeT1.neg[i])[1:10])
}


# Combine negative OPEN TO CLOSE T+1 into one DF
t_open_closeT1.neg.all = cbind(t_open_closeT1.neg1, t_open_closeT1.neg2, t_open_closeT1.neg3, t_open_closeT1.neg4, t_open_closeT1.neg5, t_open_closeT1.neg6,
                               t_open_closeT1.neg7, t_open_closeT1.neg8, t_open_closeT1.neg9, t_open_closeT1.neg10, t_open_closeT1.neg11, t_open_closeT1.neg12,
                               t_open_closeT1.neg13, t_open_closeT1.neg14, t_open_closeT1.neg15, t_open_closeT1.neg16, t_open_closeT1.neg17, t_open_closeT1.neg18,
                               t_open_closeT1.neg19, t_open_closeT1.neg20, t_open_closeT1.neg21, t_open_closeT1.neg22, t_open_closeT1.neg23, t_open_closeT1.neg24,
                               t_open_closeT1.neg25)

# Change col names
colnames(t_open_closeT1.neg.all) = tickers

# Combine positive OPEN TO CLOSE T1 into one DF
t_open_closeT1.pos.all = cbind(t_open_closeT1.pos1, t_open_closeT1.pos2, t_open_closeT1.pos3, t_open_closeT1.pos4, t_open_closeT1.pos5, t_open_closeT1.pos6,
                               t_open_closeT1.pos7, t_open_closeT1.pos8, t_open_closeT1.pos9, t_open_closeT1.pos10, t_open_closeT1.pos11, t_open_closeT1.pos12,
                               t_open_closeT1.pos13, t_open_closeT1.pos14, t_open_closeT1.pos15, t_open_closeT1.pos16, t_open_closeT1.pos17, t_open_closeT1.pos18,
                               t_open_closeT1.pos19, t_open_closeT1.pos20, t_open_closeT1.pos21, t_open_closeT1.pos22, t_open_closeT1.pos23, t_open_closeT1.pos24,
                               t_open_closeT1.pos25)

# Change col names
colnames(t_open_closeT1.pos.all) = tickers


# List of OPEN To CLOSE T+2 DFs
my_dfs_open_closeT2 = list(merged_df.t2.msft, merged_df.t2.aapl, merged_df.t2.amzn, merged_df.t2.brk,
                           merged_df.t2.fb, merged_df.t2.jnj, merged_df.t2.googl, merged_df.t2.xom,
                           merged_df.t2.unh, merged_df.t2.pfe, merged_df.t2.v, merged_df.t2.vz,
                           merged_df.t2.pg, merged_df.t2.t, merged_df.t2.intc, merged_df.t2.cvx,
                           merged_df.t2.sbac, merged_df.t2.rok, merged_df.t2.clx, merged_df.t2.fe,
                           merged_df.t2.mat, merged_df.t2.uaa, merged_df.t2.nfx, merged_df.t2.coty,
                           merged_df.t2.nws)

# Pass the list of OPEN TO CLOSE T+2 dataframes to the function for positive returns
t_open_closeT2.pos = my_dfs_open_closeT2 %>%
        lapply(my_fun_tvalue.pos)

# Pass the list of OPEN TO CLOSE T+2 dataframes to the function for negative returns
t_open_closeT2.neg = my_dfs_open_closeT2 %>%
        lapply(my_fun_tvalue.neg)

# Unwrap the listed OPEN TO CLOSE T+2 DFs for positive return t-values
for(i in 1:25){
        nam = paste("t_open_closeT2.pos", i, sep = "")
        assign(nam, unlist(t_open_closeT2.pos[i])[1:10])
}

# Unwrap the listed OPEN TO CLOSE T+2 DFs for negative return t-values
for(i in 1:25){
        nam = paste("t_open_closeT2.neg", i, sep = "")
        assign(nam, unlist(t_open_closeT2.neg[i])[1:10])
}


# Combine negative OPEN TO CLOSE T+2 into one DF
t_open_closeT2.neg.all = cbind(t_open_closeT2.neg1, t_open_closeT2.neg2, t_open_closeT2.neg3, t_open_closeT2.neg4, t_open_closeT2.neg5, t_open_closeT2.neg6,
                               t_open_closeT2.neg7, t_open_closeT2.neg8, t_open_closeT2.neg9, t_open_closeT2.neg10, t_open_closeT2.neg11, t_open_closeT2.neg12,
                               t_open_closeT2.neg13, t_open_closeT2.neg14, t_open_closeT2.neg15, t_open_closeT2.neg16, t_open_closeT2.neg17, t_open_closeT2.neg18,
                               t_open_closeT2.neg19, t_open_closeT2.neg20, t_open_closeT2.neg21, t_open_closeT2.neg22, t_open_closeT2.neg23, t_open_closeT2.neg24,
                               t_open_closeT2.neg25)

# Change col names
colnames(t_open_closeT2.neg.all) = tickers

# Combine positive OPEN TO CLOSE T+2 into one DF
t_open_closeT2.pos.all = cbind(t_open_closeT2.pos1, t_open_closeT2.pos2, t_open_closeT2.pos3, t_open_closeT2.pos4, t_open_closeT2.pos5, t_open_closeT2.pos6,
                               t_open_closeT2.pos7, t_open_closeT2.pos8, t_open_closeT2.pos9, t_open_closeT2.pos10, t_open_closeT2.pos11, t_open_closeT2.pos12,
                               t_open_closeT2.pos13, t_open_closeT2.pos14, t_open_closeT2.pos15, t_open_closeT2.pos16, t_open_closeT2.pos17, t_open_closeT2.pos18,
                               t_open_closeT2.pos19, t_open_closeT2.pos20, t_open_closeT2.pos21, t_open_closeT2.pos22, t_open_closeT2.pos23, t_open_closeT2.pos24,
                               t_open_closeT2.pos25)

# Change col names
colnames(t_open_closeT2.pos.all) = tickers

### List of OPEN To CLOSE T+5 DFs
my_dfs_open_closeT5 = list(merged_df.T5.msft, merged_df.T5.aapl, merged_df.T5.amzn, merged_df.T5.brk,
                           merged_df.T5.fb, merged_df.T5.jnj, merged_df.T5.googl, merged_df.T5.xom,
                           merged_df.T5.unh, merged_df.T5.pfe, merged_df.T5.v, merged_df.T5.vz,
                           merged_df.T5.pg, merged_df.T5.t, merged_df.T5.intc, merged_df.T5.cvx,
                           merged_df.T5.sbac, merged_df.T5.rok, merged_df.T5.clx, merged_df.T5.fe,
                           merged_df.T5.mat, merged_df.T5.uaa, merged_df.T5.nfx, merged_df.T5.coty,
                           merged_df.T5.nws)

# Pass the list of OPEN TO CLOSE T+5 dataframes to the function for positive returns
t_open_closeT5.pos = my_dfs_open_closeT5 %>%
        lapply(my_fun_tvalue.pos)

# Pass the list of OPEN TO CLOSE T+5 dataframes to the function for negative returns
t_open_closeT5.neg = my_dfs_open_closeT5 %>%
        lapply(my_fun_tvalue.neg)

# Unwrap the listed OPEN TO CLOSE T+5 DFs for positive return t-values
for(i in 1:25){
        nam = paste("t_open_closeT5.pos", i, sep = "")
        assign(nam, unlist(t_open_closeT5.pos[i])[1:10])
}

# Unwrap the listed OPEN TO CLOSE T+5 DFs for negative return t-values
for(i in 1:25){
        nam = paste("t_open_closeT5.neg", i, sep = "")
        assign(nam, unlist(t_open_closeT5.neg[i])[1:10])
}

# Combine negative OPEN TO CLOSE T+5 into one DF
t_open_closeT5.neg.all = cbind(t_open_closeT5.neg1, t_open_closeT5.neg2, t_open_closeT5.neg3, t_open_closeT5.neg4, t_open_closeT5.neg5, t_open_closeT5.neg6,
                               t_open_closeT5.neg7, t_open_closeT5.neg8, t_open_closeT5.neg9, t_open_closeT5.neg10, t_open_closeT5.neg11, t_open_closeT5.neg12,
                               t_open_closeT5.neg13, t_open_closeT5.neg14, t_open_closeT5.neg15, t_open_closeT5.neg16, t_open_closeT5.neg17, t_open_closeT5.neg18,
                               t_open_closeT5.neg19, t_open_closeT5.neg20, t_open_closeT5.neg21, t_open_closeT5.neg22, t_open_closeT5.neg23, t_open_closeT5.neg24,
                               t_open_closeT5.neg25)

colnames(t_open_closeT5.neg.all) = tickers

# Combine positive OPEN TO CLOSE T+5 into one DF
t_open_closeT5.pos.all = cbind(t_open_closeT5.pos1, t_open_closeT5.pos2, t_open_closeT5.pos3, t_open_closeT5.pos4, t_open_closeT5.pos5, t_open_closeT5.pos6,
                               t_open_closeT5.pos7, t_open_closeT5.pos8, t_open_closeT5.pos9, t_open_closeT5.pos10, t_open_closeT5.pos11, t_open_closeT5.pos12,
                               t_open_closeT5.pos13, t_open_closeT5.pos14, t_open_closeT5.pos15, t_open_closeT5.pos16, t_open_closeT5.pos17, t_open_closeT5.pos18,
                               t_open_closeT5.pos19, t_open_closeT5.pos20, t_open_closeT5.pos21, t_open_closeT5.pos22, t_open_closeT5.pos23, t_open_closeT5.pos24,
                               t_open_closeT5.pos25)

# Change col names
colnames(t_open_closeT5.pos.all) = tickers

################ REGRESSION ##############################################################################
# Function to generate regressions for CLOSE To OPEN T+1 RETURNS
fits.co <- lapply( my_dfs, function(x) {
        lm( formula = x[,8] ~ sentiment.score.daily, data = x )
} )

# Extract Regression summaries for CLOSE to OPEN T+1
for(i in 1:25){
        print(summary(fits.co[[i]]))
}

# Function to generate regressions for OPEN To CLOSE T+1 RETURNS
fits.open_closeT1 <- lapply( my_dfs_open_closeT1, function(x) {
        lm( formula = x[,8] ~ sentiment.score.daily, data = x )
} )

# Extract Regression summaries for OPEN T+1 to CLOSE T+1
for(i in 1:25){
        print(summary(fits.open_closeT1[[i]]))
}

# Function to generate regressions for OPEN To CLOSE T+2 RETURNS
fits.open_closeT2 <- lapply( my_dfs_open_closeT2, function(x) {
        lm( formula = x[,8] ~ sentiment.score.daily, data = x )
} )

# Extract Regression summaries for OPEN T+1 to CLOSE T+2
for(i in 1:25){
        print(summary(fits.open_closeT2[[i]]))
}

# Function to generate regressions for OPEN To CLOSE T+5 RETURNS
fits.open_closeT5 <- lapply( my_dfs_open_closeT5, function(x) {
        lm( formula = x[,8] ~ sentiment.score.daily, data = x )
} )

# Extract Regression summaries for OPEN T+1 to CLOSE T+5
for(i in 1:25){
        print(summary(fits.open_closeT5[[i]]))
}


########################################## SENTIMENTR CLASSIFICATIONS #####################################################

######## Load model predictions
lmcd_predictions = readRDS("sentiment.lmcd.rds")
# Change col names
colnames(lmcd_predictions)[18] = "Actual.Sentiment.Tag"

# Convert factors to numeric
lmcd_predictions$Actual.Sentiment.Tag = as.numeric(lmcd_predictions$Actual.Sentiment.Tag)

# The 1s are converted to 2s and -1s to 1s so we need to change these back
# Convert 1s back to -1s
lmcd_predictions$Actual.Sentiment.Tag[lmcd_predictions$Actual.Sentiment.Tag ==1] <- -1
# Coonvert 2s back to 1s
lmcd_predictions$Actual.Sentiment.Tag[lmcd_predictions$Actual.Sentiment.Tag==2] <- 1

# Add day of week
lmcd_predictions$day = weekdays(as.Date(lmcd_predictions$US.Date))

# Move Saturday to Monday
for(i in 1:(nrow(lmcd_predictions))){
        if (lmcd_predictions[i, 20] == "Saturday"){
                lmcd_predictions[i, 15] = lmcd_predictions[i, 15] + 2 # Move Saturday's date and time to pre open Monday
                lmcd_predictions[i, 16] = "06:00"
        }
}

# Move Sunday scores to the next day
for(i in 1:(nrow(lmcd_predictions))){
        if (lmcd_predictions[i, 20] == "Sunday"){
                lmcd_predictions[i, 15] = lmcd_predictions[i, 15] + 1 # Move Saturday's date and time to pre open Monday
                lmcd_predictions[i, 16] = "06:00"
        }
}

# Change the day of the week again based on the new dates
lmcd_predictions$day = weekdays(as.Date(lmcd_predictions$US.Date))

# Include an abs count of articles
lmcd_predictions$article.count = abs(lmcd_predictions$binary.sentiment.lmcd)

# Change the US.Time to a time using lubridate
lmcd_predictions$US.Time = hm(lmcd_predictions$US.Time)

# If an articles is published bafter 16:00 (market close) we move the date forward by 1 day to align it with the return data.
# From the return data, close T to open T+1 is marked with the date T+1. Open T+1 to Close T+1/T+2/T+5 is also marked with the date T+1.
# The sentiment date for articles released after the close on day T is moved forward to date T+1 to align with the appropriate return data.

# Change the date depending on article publication time
for(i in 1:(nrow(lmcd_predictions))){
        if (lmcd_predictions[i, 16] > hm("16:00")){
                lmcd_predictions[i, 15] = lmcd_predictions[i, 15] + 1 
                
        }
}

# Check days again as Fridays might have moved to Saturdays
lmcd_predictions$day = weekdays(as.Date(lmcd_predictions$US.Date))

# Move any new Saturdays to Monday
for(i in 1:(nrow(lmcd_predictions))){
        if (lmcd_predictions[i, 20] == "Saturday"){
                lmcd_predictions[i, 15] = lmcd_predictions[i, 15] + 2 # Move Sunday's date and time to pre open Monday
                lmcd_predictions[i, 16] = hm("06:00")
        }
}

# Check days again as Monday's might have moved to Sundays
lmcd_predictions$day = weekdays(as.Date(lmcd_predictions$US.Date))

##################################################### -----------------  INDEX LEVEL ANALYSIS: LMCD ----------------------- ##############################

# Aggregate by date
# Predicted sentiment
lmcd_predictions.grouped = aggregate(cbind(lmcd_predictions$binary.sentiment.lmcd, lmcd_predictions$ave_sentiment), by=list(Category=lmcd_predictions$US.Date), FUN=sum)

# # Change column names
colnames(lmcd_predictions.grouped) = c("Date", "Pred.Sentiment.Count", "SentimentR.score")
# # Include a count of total articles
total.articles.lmcd = aggregate(lmcd_predictions$article.count, by=list(Category=lmcd_predictions$US.Date), FUN=sum)
colnames(total.articles.lmcd) = c("Date", "Total.Articles")

# # Order by Date
lmcd_predictions.grouped = lmcd_predictions.grouped[order(lmcd_predictions.grouped$Date),]
total.articles.lmcd = total.articles.lmcd[order(total.articles.lmcd$Date),]

# # Create column that shows overall daily sentiment for predictions (binary)
for (i in 1:nrow(lmcd_predictions.grouped)){
        if(lmcd_predictions.grouped[i, 2] >= 1){
                lmcd_predictions.grouped$pred.sentiment.daily[i] = 1
        }
        else if(lmcd_predictions.grouped[i, 2] == 0){
                lmcd_predictions.grouped$pred.sentiment.daily[i] = 0
        }
        else if(lmcd_predictions.grouped[i, 2] <= -1){
                lmcd_predictions.grouped$pred.sentiment.daily[i] = -1
        }
}

# Merge the CLOSE to OPEN returns DF with the sentiment DF based on the sentiment dates
merged_df.co.lmcd = merge(lmcd_predictions.grouped, return.avg.co[ ,c(1, 27:29)], by = "Date")

# Check if differences in CLOSE to OPEN returns are statistically significant
# Positive Sentiment
t.test.all.pos.lmcd = t.test(merged_df.co.lmcd[which(merged_df.co.lmcd$pred.sentiment.daily == 1), ]$average.return, 
                             merged_df.co.lmcd[which(merged_df.co.lmcd$pred.sentiment.daily == 1), ]$SP500, alternative = "greater")

# Negative
t.test.all.neg.lmcd = t.test(merged_df.co.lmcd[which(merged_df.co.lmcd$pred.sentiment.daily == -1), ]$average.return, 
                             merged_df.co.lmcd[which(merged_df.co.lmcd$pred.sentiment.daily == -1), ]$SP500, alternative = "less")


# Merge the OPEN to CLOSE T+1 returns DF with the lmcd_predictions.grouped DF based on the lmcd_predictions.grouped dates
merged_df.open_closeT1.lmcd = merge(lmcd_predictions.grouped, return.avg.open_closeT1[ ,c(1, 27:29)], by = "Date")

# Check if differences in OPEN to CLOSE T+1 returns are statistically significant
# Positive Sentiment
t.test.all.pos.T1.lmcd = t.test(merged_df.open_closeT1.lmcd[which(merged_df.open_closeT1.lmcd$pred.sentiment.daily == 1), ]$average.return, 
                                merged_df.open_closeT1.lmcd[which(merged_df.open_closeT1.lmcd$pred.sentiment.daily == 1), ]$SP500, alternative = "greater")

# Negative
t.test.all.neg.T1.lmcd = t.test(merged_df.open_closeT1.lmcd[which(merged_df.open_closeT1.lmcd$pred.sentiment.daily == -1), ]$average.return, 
                                merged_df.open_closeT1.lmcd[which(merged_df.open_closeT1.lmcd$pred.sentiment.daily == -1), ]$SP500, alternative = "less")


# Merge the OPEN to CLOSE T+2 returns DF with the sentiment DF based on the sentiment dates
merged_df.open_closeT2.lmcd = merge(lmcd_predictions.grouped, return.avg.open_closeT2[ ,c(1, 27:29)], by = "Date")

# Check if differences in OPEN to CLOSE T+2 returns are statistically significant
# Positive Sentiment
t.test.all.pos.T2.lmcd = t.test(merged_df.open_closeT2.lmcd[which(merged_df.open_closeT2.lmcd$pred.sentiment.daily == 1), ]$average.return, 
                                merged_df.open_closeT2.lmcd[which(merged_df.open_closeT2.lmcd$pred.sentiment.daily == 1), ]$SP500, alternative = "greater")

# Negative
t.test.all.neg.T2.lmcd = t.test(merged_df.open_closeT2.lmcd[which(merged_df.open_closeT2.lmcd$pred.sentiment.daily == -1), ]$average.return, 
                                merged_df.open_closeT2.lmcd[which(merged_df.open_closeT2.lmcd$pred.sentiment.daily == -1), ]$SP500, alternative = "less")


# Merge the OPEN to CLOSE T+5 returns DF with the sentiment DF based on the sentiment dates
merged_df.open_closeT5.lmcd = merge(lmcd_predictions.grouped, return.avg.open_closeT5[ ,c(1, 27:29)], by = "Date")

# Check if differences in OPEN to CLOSE T+5 returns are statistically significant
# Positive Sentiment
t.test.all.pos.T5.lmcd = t.test(merged_df.open_closeT5.lmcd[which(merged_df.open_closeT5.lmcd$pred.sentiment.daily == 1), ]$average.return, 
                                merged_df.open_closeT5.lmcd[which(merged_df.open_closeT5.lmcd$pred.sentiment.daily == 1), ]$SP500, alternative = "greater")

# Negative
t.test.all.neg.T5.lmcd = t.test(merged_df.open_closeT5.lmcd[which(merged_df.open_closeT5.lmcd$pred.sentiment.daily == -1), ]$average.return, 
                                merged_df.open_closeT5.lmcd[which(merged_df.open_closeT5.lmcd$pred.sentiment.daily == -1), ]$SP500, alternative = "less")


########################################## INDEX LEVEL REGRESSION: SENTIMENTR #########################################################
# CLOSE TO OPEN T+1
reg.co.lmcd = lm(average.return ~ SentimentR.score, data = merged_df.co.lmcd)
reg.co.lmcd
summary(reg.co.lmcd)

# OPEN TO CLOSE T+1
reg.closeT1.lmcd = lm(average.return ~ SentimentR.score, data = merged_df.open_closeT1.lmcd)
reg.closeT1.lmcd
summary(reg.closeT1.lmcd)

# OPEN TO CLOSE T+2
reg.closeT2.lmcd = lm(average.return ~ SentimentR.score, data = merged_df.open_closeT2.lmcd)
reg.closeT2.lmcd
summary(reg.closeT2.lmcd)

# OPEN TO CLOSE T+5
reg.closeT5.lmcd = lm(average.return ~ SentimentR.score, data = merged_df.open_closeT5.lmcd)
reg.closeT5.lmcd
summary(reg.closeT5.lmcd)


######################################################--------------- INDIVIDUAL STOCK ANALYSIS: LMCD -----------------------------########################

# Subset lmcd predictions for stock specific headlines
for(i in tickers){
        nam <- paste("lmcd_predictions.", i, sep = "")
        assign(nam, lmcd_predictions[which(lmcd_predictions$Identifier == i), ])
}

# Aggregate by Identifier and date and sum predicted sentiment and article count
agg.df.lmcd = aggregate(cbind(lmcd_predictions$binary.sentiment.lmcd, lmcd_predictions$ave_sentiment, lmcd_predictions$article.count) ~ Identifier + US.Date, data = lmcd_predictions, sum)
colnames(agg.df.lmcd)[2:5] = c("Date","pred.sentiment.count", "SentimentR.score", "total.articles")

# Include daily sentiment indicator
for (i in 1:nrow(agg.df.lmcd)){
        if(agg.df.lmcd[i, 3] >= 1){
                agg.df.lmcd$pred.sentiment.daily[i] = 1
        }
        else if(agg.df.lmcd[i, 3] <= -1){
                agg.df.lmcd$pred.sentiment.daily[i] = -1
        }
        else{
                agg.df.lmcd$pred.sentiment.daily[i] = 0
        }
}

# Take out the individual stocks
for(i in tickers){
        nam <- paste("sentiment.lmcd.", i, sep = "")
        assign(nam, agg.df.lmcd[which(agg.df.lmcd$Identifier == i), ])
}

#####
# Merge the CLOSE to OPEN T1 returns DFs with the sentiment DFs based on the sentiment dates
merged_df.co.lmcd.msft = merge(`sentiment.lmcd.MSFT-US`, returns.co.3, by = "Date")
merged_df.co.lmcd.aapl = merge(`sentiment.lmcd.AAPL-US`, returns.co.4, by = "Date")
merged_df.co.lmcd.amzn = merge(`sentiment.lmcd.AMZN-US`, returns.co.5, by = "Date")
merged_df.co.lmcd.brk = merge(`sentiment.lmcd.BRK.B-US`, returns.co.6, by = "Date")
merged_df.co.lmcd.fb = merge(`sentiment.lmcd.FB-US`, returns.co.7, by = "Date")
merged_df.co.lmcd.jnj = merge(`sentiment.lmcd.JNJ-US`, returns.co.8, by = "Date")
merged_df.co.lmcd.googl = merge(`sentiment.lmcd.GOOGL-US`, returns.co.9, by = "Date")
merged_df.co.lmcd.xom = merge(`sentiment.lmcd.XOM-US`, returns.co.10, by = "Date")
merged_df.co.lmcd.unh = merge(`sentiment.lmcd.UNH-US`, returns.co.11, by = "Date")
merged_df.co.lmcd.pfe = merge(`sentiment.lmcd.PFE-US`, returns.co.12, by = "Date")
merged_df.co.lmcd.v = merge(`sentiment.lmcd.V-US`, returns.co.13, by = "Date")
merged_df.co.lmcd.vz = merge(`sentiment.lmcd.VZ-US`, returns.co.14, by = "Date")
merged_df.co.lmcd.pg = merge(`sentiment.lmcd.PG-US`, returns.co.15, by = "Date")
merged_df.co.lmcd.t = merge(`sentiment.lmcd.T-US`, returns.co.16, by = "Date")
merged_df.co.lmcd.intc = merge(`sentiment.lmcd.INTC-US`, returns.co.17, by = "Date")
merged_df.co.lmcd.cvx = merge(`sentiment.lmcd.CVX-US`, returns.co.18, by = "Date")
merged_df.co.lmcd.sbac = merge(`sentiment.lmcd.SBAC-US`, returns.co.19, by = "Date")
merged_df.co.lmcd.rok = merge(`sentiment.lmcd.ROK-US`, returns.co.20, by = "Date")
merged_df.co.lmcd.clx = merge(`sentiment.lmcd.CLX-US`, returns.co.21, by = "Date")
merged_df.co.lmcd.fe = merge(`sentiment.lmcd.FE-US`, returns.co.22, by = "Date")
merged_df.co.lmcd.mat = merge(`sentiment.lmcd.MAT-US`, returns.co.23, by = "Date")
merged_df.co.lmcd.uaa = merge(`sentiment.lmcd.UAA-US`, returns.co.24, by = "Date")
merged_df.co.lmcd.nfx = merge(`sentiment.lmcd.NFX-US`, returns.co.25, by = "Date")
merged_df.co.lmcd.coty = merge(`sentiment.lmcd.COTY-US`, returns.co.26, by = "Date")
merged_df.co.lmcd.nws = merge(`sentiment.lmcd.NWS-US`, returns.co.27, by = "Date")

# Merge the OPEN to CLOSE T+1 returns DFs with the sentiment DFs based on the sentiment dates
merged_df.t1.lmcd.msft = merge(`sentiment.lmcd.MSFT-US`, returns.closeT13, by = "Date")
merged_df.t1.lmcd.aapl = merge(`sentiment.lmcd.AAPL-US`, returns.closeT14, by = "Date")
merged_df.t1.lmcd.amzn = merge(`sentiment.lmcd.AMZN-US`, returns.closeT15, by = "Date")
merged_df.t1.lmcd.brk = merge(`sentiment.lmcd.BRK.B-US`, returns.closeT16, by = "Date")
merged_df.t1.lmcd.fb = merge(`sentiment.lmcd.FB-US`, returns.closeT17, by = "Date")
merged_df.t1.lmcd.jnj = merge(`sentiment.lmcd.JNJ-US`, returns.closeT18, by = "Date")
merged_df.t1.lmcd.googl = merge(`sentiment.lmcd.GOOGL-US`, returns.closeT19, by = "Date")
merged_df.t1.lmcd.xom = merge(`sentiment.lmcd.XOM-US`, returns.closeT110, by = "Date")
merged_df.t1.lmcd.unh = merge(`sentiment.lmcd.UNH-US`, returns.closeT111, by = "Date")
merged_df.t1.lmcd.pfe = merge(`sentiment.lmcd.PFE-US`, returns.closeT112, by = "Date")
merged_df.t1.lmcd.v = merge(`sentiment.lmcd.V-US`, returns.closeT113, by = "Date")
merged_df.t1.lmcd.vz = merge(`sentiment.lmcd.VZ-US`, returns.closeT114, by = "Date")
merged_df.t1.lmcd.pg = merge(`sentiment.lmcd.PG-US`, returns.closeT115, by = "Date")
merged_df.t1.lmcd.t = merge(`sentiment.lmcd.T-US`, returns.closeT116, by = "Date")
merged_df.t1.lmcd.intc = merge(`sentiment.lmcd.INTC-US`, returns.closeT117, by = "Date")
merged_df.t1.lmcd.cvx = merge(`sentiment.lmcd.CVX-US`, returns.closeT118, by = "Date")
merged_df.t1.lmcd.sbac = merge(`sentiment.lmcd.SBAC-US`, returns.closeT119, by = "Date")
merged_df.t1.lmcd.rok = merge(`sentiment.lmcd.ROK-US`, returns.closeT120, by = "Date")
merged_df.t1.lmcd.clx = merge(`sentiment.lmcd.CLX-US`, returns.closeT121, by = "Date")
merged_df.t1.lmcd.fe = merge(`sentiment.lmcd.FE-US`, returns.closeT122, by = "Date")
merged_df.t1.lmcd.mat = merge(`sentiment.lmcd.MAT-US`, returns.closeT123, by = "Date")
merged_df.t1.lmcd.uaa = merge(`sentiment.lmcd.UAA-US`, returns.closeT124, by = "Date")
merged_df.t1.lmcd.nfx = merge(`sentiment.lmcd.NFX-US`, returns.closeT125, by = "Date")
merged_df.t1.lmcd.coty = merge(`sentiment.lmcd.COTY-US`, returns.closeT126, by = "Date")
merged_df.t1.lmcd.nws = merge(`sentiment.lmcd.NWS-US`, returns.closeT127, by = "Date")


# Merge the OPEN to CLOSE T+2 returns DFs with the sentiment DFs based on the sentiment dates
merged_df.t2.lmcd.msft = merge(`sentiment.lmcd.MSFT-US`, returns.closeT23, by = "Date")
merged_df.t2.lmcd.aapl = merge(`sentiment.lmcd.AAPL-US`, returns.closeT24, by = "Date")
merged_df.t2.lmcd.amzn = merge(`sentiment.lmcd.AMZN-US`, returns.closeT25, by = "Date")
merged_df.t2.lmcd.brk = merge(`sentiment.lmcd.BRK.B-US`, returns.closeT26, by = "Date")
merged_df.t2.lmcd.fb = merge(`sentiment.lmcd.FB-US`, returns.closeT27, by = "Date")
merged_df.t2.lmcd.jnj = merge(`sentiment.lmcd.JNJ-US`, returns.closeT28, by = "Date")
merged_df.t2.lmcd.googl = merge(`sentiment.lmcd.GOOGL-US`, returns.closeT29, by = "Date")
merged_df.t2.lmcd.xom = merge(`sentiment.lmcd.XOM-US`, returns.closeT210, by = "Date")
merged_df.t2.lmcd.unh = merge(`sentiment.lmcd.UNH-US`, returns.closeT211, by = "Date")
merged_df.t2.lmcd.pfe = merge(`sentiment.lmcd.PFE-US`, returns.closeT212, by = "Date")
merged_df.t2.lmcd.v = merge(`sentiment.lmcd.V-US`, returns.closeT213, by = "Date")
merged_df.t2.lmcd.vz = merge(`sentiment.lmcd.VZ-US`, returns.closeT214, by = "Date")
merged_df.t2.lmcd.pg = merge(`sentiment.lmcd.PG-US`, returns.closeT215, by = "Date")
merged_df.t2.lmcd.t = merge(`sentiment.lmcd.T-US`, returns.closeT216, by = "Date")
merged_df.t2.lmcd.intc = merge(`sentiment.lmcd.INTC-US`, returns.closeT217, by = "Date")
merged_df.t2.lmcd.cvx = merge(`sentiment.lmcd.CVX-US`, returns.closeT218, by = "Date")
merged_df.t2.lmcd.sbac = merge(`sentiment.lmcd.SBAC-US`, returns.closeT219, by = "Date")
merged_df.t2.lmcd.rok = merge(`sentiment.lmcd.ROK-US`, returns.closeT220, by = "Date")
merged_df.t2.lmcd.clx = merge(`sentiment.lmcd.CLX-US`, returns.closeT221, by = "Date")
merged_df.t2.lmcd.fe = merge(`sentiment.lmcd.FE-US`, returns.closeT222, by = "Date")
merged_df.t2.lmcd.mat = merge(`sentiment.lmcd.MAT-US`, returns.closeT223, by = "Date")
merged_df.t2.lmcd.uaa = merge(`sentiment.lmcd.UAA-US`, returns.closeT224, by = "Date")
merged_df.t2.lmcd.nfx = merge(`sentiment.lmcd.NFX-US`, returns.closeT225, by = "Date")
merged_df.t2.lmcd.coty = merge(`sentiment.lmcd.COTY-US`, returns.closeT226, by = "Date")
merged_df.t2.lmcd.nws = merge(`sentiment.lmcd.NWS-US`, returns.closeT227, by = "Date")

# Merge the OPEN to CLOSE T+5 returns DFs with the sentiment DFs based on the sentiment dates
merged_df.T5.lmcd.msft = merge(`sentiment.lmcd.MSFT-US`, returns.closeT53, by = "Date")
merged_df.T5.lmcd.aapl = merge(`sentiment.lmcd.AAPL-US`, returns.closeT54, by = "Date")
merged_df.T5.lmcd.amzn = merge(`sentiment.lmcd.AMZN-US`, returns.closeT55, by = "Date")
merged_df.T5.lmcd.brk = merge(`sentiment.lmcd.BRK.B-US`, returns.closeT56, by = "Date")
merged_df.T5.lmcd.fb = merge(`sentiment.lmcd.FB-US`, returns.closeT57, by = "Date")
merged_df.T5.lmcd.jnj = merge(`sentiment.lmcd.JNJ-US`, returns.closeT58, by = "Date")
merged_df.T5.lmcd.googl = merge(`sentiment.lmcd.GOOGL-US`, returns.closeT59, by = "Date")
merged_df.T5.lmcd.xom = merge(`sentiment.lmcd.XOM-US`, returns.closeT510, by = "Date")
merged_df.T5.lmcd.unh = merge(`sentiment.lmcd.UNH-US`, returns.closeT511, by = "Date")
merged_df.T5.lmcd.pfe = merge(`sentiment.lmcd.PFE-US`, returns.closeT512, by = "Date")
merged_df.T5.lmcd.v = merge(`sentiment.lmcd.V-US`, returns.closeT513, by = "Date")
merged_df.T5.lmcd.vz = merge(`sentiment.lmcd.VZ-US`, returns.closeT514, by = "Date")
merged_df.T5.lmcd.pg = merge(`sentiment.lmcd.PG-US`, returns.closeT515, by = "Date")
merged_df.T5.lmcd.t = merge(`sentiment.lmcd.T-US`, returns.closeT516, by = "Date")
merged_df.T5.lmcd.intc = merge(`sentiment.lmcd.INTC-US`, returns.closeT517, by = "Date")
merged_df.T5.lmcd.cvx = merge(`sentiment.lmcd.CVX-US`, returns.closeT518, by = "Date")
merged_df.T5.lmcd.sbac = merge(`sentiment.lmcd.SBAC-US`, returns.closeT519, by = "Date")
merged_df.T5.lmcd.rok = merge(`sentiment.lmcd.ROK-US`, returns.closeT520, by = "Date")
merged_df.T5.lmcd.clx = merge(`sentiment.lmcd.CLX-US`, returns.closeT521, by = "Date")
merged_df.T5.lmcd.fe = merge(`sentiment.lmcd.FE-US`, returns.closeT522, by = "Date")
merged_df.T5.lmcd.mat = merge(`sentiment.lmcd.MAT-US`, returns.closeT523, by = "Date")
merged_df.T5.lmcd.uaa = merge(`sentiment.lmcd.UAA-US`, returns.closeT524, by = "Date")
merged_df.T5.lmcd.nfx = merge(`sentiment.lmcd.NFX-US`, returns.closeT525, by = "Date")
merged_df.T5.lmcd.coty = merge(`sentiment.lmcd.COTY-US`, returns.closeT526, by = "Date")
merged_df.T5.lmcd.nws = merge(`sentiment.lmcd.NWS-US`, returns.closeT527, by = "Date")

# DFs of CLOSE to OPEN 
my_dfs.lmcd = list(merged_df.co.lmcd.msft, merged_df.co.lmcd.aapl, merged_df.co.lmcd.amzn, merged_df.co.lmcd.brk,
                   merged_df.co.lmcd.fb, merged_df.co.lmcd.jnj, merged_df.co.lmcd.googl, merged_df.co.lmcd.xom,
                   merged_df.co.lmcd.unh, merged_df.co.lmcd.pfe, merged_df.co.lmcd.v, merged_df.co.lmcd.vz,
                   merged_df.co.lmcd.pg, merged_df.co.lmcd.t, merged_df.co.lmcd.intc, merged_df.co.lmcd.cvx,
                   merged_df.co.lmcd.sbac, merged_df.co.lmcd.rok, merged_df.co.lmcd.clx, merged_df.co.lmcd.fe,
                   merged_df.co.lmcd.mat, merged_df.co.lmcd.uaa, merged_df.co.lmcd.nfx, merged_df.co.lmcd.coty,
                   merged_df.co.lmcd.nws)


# Function to calculate t-stats for positive returns (is slightly different to previous function above due to different columns)
my_fun_tvalue.pos.lmcd = function(x){
        t.test(x[which(x[,6] == 1), ][,8], 
               x[which(x[ ,6] == 1), ][,7], alternative = "greater")
        
}

# Function to calculate t-stats for negative returns (is slightly different to previous function above due to different columns)
my_fun_tvalue.neg.lmcd = function(x){
        t.test(x[which(x[,6] == -1), ][,8], 
               x[which(x[ ,6] == -1), ][,7], alternative = "less")
        
}

# Pass the list of CLOSE TO OPEN dataframes to the function for positive returns
lmcd.co.pos = my_dfs.lmcd %>%
        lapply(my_fun_tvalue.pos.lmcd)

# Pass the list of CLOSE TO OPEN dataframes to the function for negative returns
lmcd.co.neg = my_dfs.lmcd %>%
        lapply(my_fun_tvalue.neg.lmcd)

# Unwrap the listed CLOSE TO OPEN DFs for positive return t-values
for(i in 1:25){
        nam = paste("new_df.lmcd.pos", i, sep = "")
        assign(nam, unlist(lmcd.co.pos[i])[1:10])
}

# Unwrap the listed CLOSE TO OPEN DFs for negative return t-values
for(i in 1:25){
        nam = paste("new_df.lmcd.neg", i, sep = "")
        assign(nam, unlist(lmcd.co.neg[i])[1:10])
}

# Combine negative CLOSE TO OPEN T+1 into one DF
t_close_open.neg.lmcd.all = cbind(new_df.lmcd.neg1, new_df.lmcd.neg2, new_df.lmcd.neg3, new_df.lmcd.neg4, new_df.lmcd.neg5, new_df.lmcd.neg6,
                                  new_df.lmcd.neg7, new_df.lmcd.neg8, new_df.lmcd.neg9, new_df.lmcd.neg10, new_df.lmcd.neg11, new_df.lmcd.neg12,
                                  new_df.lmcd.neg13, new_df.lmcd.neg14, new_df.lmcd.neg15, new_df.lmcd.neg16, new_df.lmcd.neg17, new_df.lmcd.neg18,
                                  new_df.lmcd.neg19, new_df.lmcd.neg20, new_df.lmcd.neg21, new_df.lmcd.neg22, new_df.lmcd.neg23, new_df.lmcd.neg24,
                                  new_df.lmcd.neg25)

# Change col names
colnames(t_close_open.neg.lmcd.all) = tickers

# Combine positive CLOSE TO OPEN T+1 into one DF
t_close_open.pos.lmcd.all = cbind(new_df.lmcd.pos1, new_df.lmcd.pos2, new_df.lmcd.pos3, new_df.lmcd.pos4, new_df.lmcd.pos5, new_df.lmcd.pos6,
                                  new_df.lmcd.pos7, new_df.lmcd.pos8, new_df.lmcd.pos9, new_df.lmcd.pos10, new_df.lmcd.pos11, new_df.lmcd.pos12,
                                  new_df.lmcd.pos13, new_df.lmcd.pos14, new_df.lmcd.pos15, new_df.lmcd.pos16, new_df.lmcd.pos17, new_df.lmcd.pos18,
                                  new_df.lmcd.pos19, new_df.lmcd.pos20, new_df.lmcd.pos21, new_df.lmcd.pos22, new_df.lmcd.pos23, new_df.lmcd.pos24,
                                  new_df.lmcd.pos25)

# Change col names
colnames(t_close_open.pos.lmcd.all) = tickers

################ List of OPEN To CLOSE T+1 DFs
my_dfs_open_closeT1.lmcd = list(merged_df.t1.lmcd.msft, merged_df.t1.lmcd.aapl, merged_df.t1.lmcd.amzn, merged_df.t1.lmcd.brk,
                                merged_df.t1.lmcd.fb, merged_df.t1.lmcd.jnj, merged_df.t1.lmcd.googl, merged_df.t1.lmcd.xom,
                                merged_df.t1.lmcd.unh, merged_df.t1.lmcd.pfe, merged_df.t1.lmcd.v, merged_df.t1.lmcd.vz,
                                merged_df.t1.lmcd.pg, merged_df.t1.lmcd.t, merged_df.t1.lmcd.intc, merged_df.t1.lmcd.cvx,
                                merged_df.t1.lmcd.sbac, merged_df.t1.lmcd.rok, merged_df.t1.lmcd.clx, merged_df.t1.lmcd.fe,
                                merged_df.t1.lmcd.mat, merged_df.t1.lmcd.uaa, merged_df.t1.lmcd.nfx, merged_df.t1.lmcd.coty,
                                merged_df.t1.lmcd.nws)

# Pass the list of OPEN TO CLOSE T1 dataframes to the function for positive returns
t_open_closeT1.lmcd.pos = my_dfs_open_closeT1.lmcd %>%
        lapply(my_fun_tvalue.pos.lmcd)

# Pass the list of OPEN TO CLOSE T1 dataframes to the function for negative returns
t_open_closeT1.lmcd.neg = my_dfs_open_closeT1.lmcd %>%
        lapply(my_fun_tvalue.neg.lmcd)

# Unwrap the listed OPEN TO CLOSE T+1 DFs for positive return t-values
for(i in 1:25){
        nam = paste("t_open_closeT1.lmcd.pos", i, sep = "")
        assign(nam, unlist(t_open_closeT1.lmcd.pos[i])[1:10])
}

# Unwrap the listed OPEN TO CLOSE T1 DFs for negative return t-values
for(i in 1:25){
        nam = paste("t_open_closeT1.lmcd.neg", i, sep = "")
        assign(nam, unlist(t_open_closeT1.lmcd.neg[i])[1:10])
}


# Combine negative OPEN TO CLOSE T1 into one DF
t_open_closeT1.neg.lmcd.all = cbind(t_open_closeT1.lmcd.neg1, t_open_closeT1.lmcd.neg2, t_open_closeT1.lmcd.neg3, t_open_closeT1.lmcd.neg4, t_open_closeT1.lmcd.neg5, t_open_closeT1.lmcd.neg6,
                                    t_open_closeT1.lmcd.neg7, t_open_closeT1.lmcd.neg8, t_open_closeT1.lmcd.neg9, t_open_closeT1.lmcd.neg10, t_open_closeT1.lmcd.neg11, t_open_closeT1.lmcd.neg12,
                                    t_open_closeT1.lmcd.neg13, t_open_closeT1.lmcd.neg14, t_open_closeT1.lmcd.neg15, t_open_closeT1.lmcd.neg16, t_open_closeT1.lmcd.neg17, t_open_closeT1.lmcd.neg18,
                                    t_open_closeT1.lmcd.neg19, t_open_closeT1.lmcd.neg20, t_open_closeT1.lmcd.neg21, t_open_closeT1.lmcd.neg22, t_open_closeT1.lmcd.neg23, t_open_closeT1.lmcd.neg24,
                                    t_open_closeT1.lmcd.neg25)

# Change col names
colnames(t_open_closeT1.neg.lmcd.all) = tickers

# Combine positive OPEN TO CLOSE T1 into one DF
t_open_closeT1.pos.lmcd.all = cbind(t_open_closeT1.lmcd.pos1, t_open_closeT1.lmcd.pos2, t_open_closeT1.lmcd.pos3, t_open_closeT1.lmcd.pos4, t_open_closeT1.lmcd.pos5, t_open_closeT1.lmcd.pos6,
                                    t_open_closeT1.lmcd.pos7, t_open_closeT1.lmcd.pos8, t_open_closeT1.lmcd.pos9, t_open_closeT1.lmcd.pos10, t_open_closeT1.lmcd.pos11, t_open_closeT1.lmcd.pos12,
                                    t_open_closeT1.lmcd.pos13, t_open_closeT1.lmcd.pos14, t_open_closeT1.lmcd.pos15, t_open_closeT1.lmcd.pos16, t_open_closeT1.lmcd.pos17, t_open_closeT1.lmcd.pos18,
                                    t_open_closeT1.lmcd.pos19, t_open_closeT1.lmcd.pos20, t_open_closeT1.lmcd.pos21, t_open_closeT1.lmcd.pos22, t_open_closeT1.lmcd.pos23, t_open_closeT1.lmcd.pos24,
                                    t_open_closeT1.lmcd.pos25)

# Change col names
colnames(t_open_closeT1.pos.lmcd.all) = tickers

################ List of OPEN To CLOSE T+2 DFs
my_dfs_open_closeT2.lmcd = list(merged_df.t2.lmcd.msft, merged_df.t2.lmcd.aapl, merged_df.t2.lmcd.amzn, merged_df.t2.lmcd.brk,
                                merged_df.t2.lmcd.fb, merged_df.t2.lmcd.jnj, merged_df.t2.lmcd.googl, merged_df.t2.lmcd.xom,
                                merged_df.t2.lmcd.unh, merged_df.t2.lmcd.pfe, merged_df.t2.lmcd.v, merged_df.t2.lmcd.vz,
                                merged_df.t2.lmcd.pg, merged_df.t2.lmcd.t, merged_df.t2.lmcd.intc, merged_df.t2.lmcd.cvx,
                                merged_df.t2.lmcd.sbac, merged_df.t2.lmcd.rok, merged_df.t2.lmcd.clx, merged_df.t2.lmcd.fe,
                                merged_df.t2.lmcd.mat, merged_df.t2.lmcd.uaa, merged_df.t2.lmcd.nfx, merged_df.t2.lmcd.coty,
                                merged_df.t2.lmcd.nws)

# Pass the list of OPEN TO CLOSE T+2 dataframes to the function for positive returns
t_open_closeT2.lmcd.pos = my_dfs_open_closeT2.lmcd %>%
        lapply(my_fun_tvalue.pos.lmcd)

# Pass the list of OPEN TO CLOSE T+2 dataframes to the function for negative returns
t_open_closeT2.lmcd.neg = my_dfs_open_closeT2.lmcd %>%
        lapply(my_fun_tvalue.neg.lmcd)


# Unwrap the listed OPEN TO CLOSE T+2 DFs for positive return t-values
for(i in 1:25){
        nam = paste("t_open_closeT2.lmcd.pos", i, sep = "")
        assign(nam, unlist(t_open_closeT2.lmcd.pos[i])[1:10])
}

# Unwrap the listed OPEN TO CLOSE T+2 DFs for negative return t-values
for(i in 1:25){
        nam = paste("t_open_closeT2.lmcd.neg", i, sep = "")
        assign(nam, unlist(t_open_closeT2.lmcd.neg[i])[1:10])
}

# Combine negative OPEN TO CLOSE T+2 into one DF
t_open_closeT2.neg.lmcd.all = cbind(t_open_closeT2.lmcd.neg1, t_open_closeT2.lmcd.neg2, t_open_closeT2.lmcd.neg3, t_open_closeT2.lmcd.neg4, t_open_closeT2.lmcd.neg5, t_open_closeT2.lmcd.neg6,
                                    t_open_closeT2.lmcd.neg7, t_open_closeT2.lmcd.neg8, t_open_closeT2.lmcd.neg9, t_open_closeT2.lmcd.neg10, t_open_closeT2.lmcd.neg11, t_open_closeT2.lmcd.neg12,
                                    t_open_closeT2.lmcd.neg13, t_open_closeT2.lmcd.neg14, t_open_closeT2.lmcd.neg15, t_open_closeT2.lmcd.neg16, t_open_closeT2.lmcd.neg17, t_open_closeT2.lmcd.neg18,
                                    t_open_closeT2.lmcd.neg19, t_open_closeT2.lmcd.neg20, t_open_closeT2.lmcd.neg21, t_open_closeT2.lmcd.neg22, t_open_closeT2.lmcd.neg23, t_open_closeT2.lmcd.neg24,
                                    t_open_closeT2.lmcd.neg25)

# Change col names
colnames(t_open_closeT2.neg.lmcd.all) = tickers

# Combine positive OPEN TO CLOSE T+2 into one DF
t_open_closeT2.pos.lmcd.all = cbind(t_open_closeT2.lmcd.pos1, t_open_closeT2.lmcd.pos2, t_open_closeT2.lmcd.pos3, t_open_closeT2.lmcd.pos4, t_open_closeT2.lmcd.pos5, t_open_closeT2.lmcd.pos6,
                                    t_open_closeT2.lmcd.pos7, t_open_closeT2.lmcd.pos8, t_open_closeT2.lmcd.pos9, t_open_closeT2.lmcd.pos10, t_open_closeT2.lmcd.pos11, t_open_closeT2.lmcd.pos12,
                                    t_open_closeT2.lmcd.pos13, t_open_closeT2.lmcd.pos14, t_open_closeT2.lmcd.pos15, t_open_closeT2.lmcd.pos16, t_open_closeT2.lmcd.pos17, t_open_closeT2.lmcd.pos18,
                                    t_open_closeT2.lmcd.pos19, t_open_closeT2.lmcd.pos20, t_open_closeT2.lmcd.pos21, t_open_closeT2.lmcd.pos22, t_open_closeT2.lmcd.pos23, t_open_closeT2.lmcd.pos24,
                                    t_open_closeT2.lmcd.pos25)

# Change col names
colnames(t_open_closeT2.pos.lmcd.all) = tickers


### List of OPEN To CLOSE T+5 DFs
my_dfs_open_closeT5.lmcd = list(merged_df.T5.lmcd.msft, merged_df.T5.lmcd.aapl, merged_df.T5.lmcd.amzn, merged_df.T5.lmcd.brk,
                                merged_df.T5.lmcd.fb, merged_df.T5.lmcd.jnj, merged_df.T5.lmcd.googl, merged_df.T5.lmcd.xom,
                                merged_df.T5.lmcd.unh, merged_df.T5.lmcd.pfe, merged_df.T5.lmcd.v, merged_df.T5.lmcd.vz,
                                merged_df.T5.lmcd.pg, merged_df.T5.lmcd.t, merged_df.T5.lmcd.intc, merged_df.T5.lmcd.cvx,
                                merged_df.T5.lmcd.sbac, merged_df.T5.lmcd.rok, merged_df.T5.lmcd.clx, merged_df.T5.lmcd.fe,
                                merged_df.T5.lmcd.mat, merged_df.T5.lmcd.uaa, merged_df.T5.lmcd.nfx, merged_df.T5.lmcd.coty,
                                merged_df.T5.lmcd.nws)

# Pass the list of OPEN TO CLOSE T+5 dataframes to the function for positive returns
t_open_closeT5.lmcd.pos = my_dfs_open_closeT5.lmcd %>%
        lapply(my_fun_tvalue.pos.lmcd)

# Pass the list of OPEN TO CLOSE T+5 dataframes to the function for negative returns
t_open_closeT5.lmcd.neg = my_dfs_open_closeT5.lmcd %>%
        lapply(my_fun_tvalue.neg.lmcd)

# Unwrap the listed OPEN TO CLOSE T+5 DFs for positive return t-values
for(i in 1:25){
        nam = paste("t_open_closeT5.lmcd.pos", i, sep = "")
        assign(nam, unlist(t_open_closeT5.lmcd.pos[i])[1:10])
}

# Unwrap the listed OPEN TO CLOSE T+5 DFs for negative return t-values
for(i in 1:25){
        nam = paste("t_open_closeT5.lmcd.neg", i, sep = "")
        assign(nam, unlist(t_open_closeT5.lmcd.neg[i])[1:10])
}

# Combine negative OPEN TO CLOSE T+5 into one DF
t_open_closeT5.neg.lmcd.all = cbind(t_open_closeT5.lmcd.neg1, t_open_closeT5.lmcd.neg2, t_open_closeT5.lmcd.neg3, t_open_closeT5.lmcd.neg4, t_open_closeT5.lmcd.neg5, t_open_closeT5.lmcd.neg6,
                                    t_open_closeT5.lmcd.neg7, t_open_closeT5.lmcd.neg8, t_open_closeT5.lmcd.neg9, t_open_closeT5.lmcd.neg10, t_open_closeT5.lmcd.neg11, t_open_closeT5.lmcd.neg12,
                                    t_open_closeT5.lmcd.neg13, t_open_closeT5.lmcd.neg14, t_open_closeT5.lmcd.neg15, t_open_closeT5.lmcd.neg16, t_open_closeT5.lmcd.neg17, t_open_closeT5.lmcd.neg18,
                                    t_open_closeT5.lmcd.neg19, t_open_closeT5.lmcd.neg20, t_open_closeT5.lmcd.neg21, t_open_closeT5.lmcd.neg22, t_open_closeT5.lmcd.neg23, t_open_closeT5.lmcd.neg24,
                                    t_open_closeT5.lmcd.neg25)

colnames(t_open_closeT5.neg.lmcd.all) = tickers

# Combine positive OPEN TO CLOSE T+5 into one DF
t_open_closeT5.pos.lmcd.all = cbind(t_open_closeT5.lmcd.pos1, t_open_closeT5.lmcd.pos2, t_open_closeT5.lmcd.pos3, t_open_closeT5.lmcd.pos4, t_open_closeT5.lmcd.pos5, t_open_closeT5.lmcd.pos6,
                                    t_open_closeT5.lmcd.pos7, t_open_closeT5.lmcd.pos8, t_open_closeT5.lmcd.pos9, t_open_closeT5.lmcd.pos10, t_open_closeT5.lmcd.pos11, t_open_closeT5.lmcd.pos12,
                                    t_open_closeT5.lmcd.pos13, t_open_closeT5.lmcd.pos14, t_open_closeT5.lmcd.pos15, t_open_closeT5.lmcd.pos16, t_open_closeT5.lmcd.pos17, t_open_closeT5.lmcd.pos18,
                                    t_open_closeT5.lmcd.pos19, t_open_closeT5.lmcd.pos20, t_open_closeT5.lmcd.pos21, t_open_closeT5.lmcd.pos22, t_open_closeT5.lmcd.pos23, t_open_closeT5.lmcd.pos24,
                                    t_open_closeT5.lmcd.pos25)

# Change col names
colnames(t_open_closeT5.pos.lmcd.all) = tickers

################ SENTIMENTR REGRESSION ##############################################################################
# Function to generate regressions for CLOSE To OPEN T+1 RETURNS
# This is regressing the sentiment score against the stocks return (not the relative return)
fits.co.lmcd <- lapply(my_dfs.lmcd, function(x) {
        lm( formula = x[,8] ~ SentimentR.score, data = x )
} )

# # Extract Regression summaries for CLOSE to OPEN T+1
for(i in 1:25){
        print(summary(fits.co.lmcd[[i]]))
}

# # Function to generate regressions for OPEN To CLOSE T+1 RETURNS
fits.open_closeT1.lmcd <- lapply( my_dfs_open_closeT1.lmcd, function(x) {
        lm( formula = x[,8] ~ SentimentR.score, data = x )
})
# 
# Extract Regression summaries for OPEN T+1 to CLOSE T+1
for(i in 1:25){
        print(summary(fits.open_closeT1.lmcd[[i]]))
}

# Function to generate regressions for OPEN To CLOSE T+2 RETURNS
fits.open_closeT2.lmcd <- lapply(my_dfs_open_closeT2.lmcd, function(x) {
        lm( formula = x[,8] ~ SentimentR.score, data = x )
})

# Extract Regression summaries for OPEN T+1 to CLOSE T+2
for(i in 1:25){
        print(summary(fits.open_closeT2.lmcd[[i]]))
}

# Function to generate regressions for OPEN To CLOSE T+5 RETURNS
fits.open_closeT5.lmcd <- lapply(my_dfs_open_closeT5.lmcd, function(x) {
        lm(formula = x[,8] ~ SentimentR.score, data = x )
})
 
# Extract Regression summaries for OPEN T+1 to CLOSE T+5
for(i in 1:25){
        print(summary(fits.open_closeT5.lmcd[[i]]))
}

########################################### INDEX CREATION ##########################################################
#### Calculate CLOSE to CLOSE returns
# Function to calc CLOSE to CLOSE returns
func_rets <- function(x){ (x - lag(x))/lag(x)}

# Calc CLOSE to CLOSE Returns
returns = mutate_if(prices, is.numeric, func_rets)

# Convert NAs to 0
returns[is.na(returns)] = 0

# Average daily return return 
returns$average.return = rowMeans(returns[ , 2:ncol(returns)])

# Cumulative returns
returns$cum = cumprod(returns$average.return+1)-1

# Create Price Series
returns$index.level = 100
returns$index.level = returns$index.level * (1+returns$cum)

# Cumulative sum of sentiment count
sentiment.index.df = sentiment
sentiment.index.df$cum.sum.pred = cumsum(sentiment.index.df$Pred.Sentiment.Count)
sentiment.index.df$cum.sum.actual = cumsum(sentiment.index.df$Actual.Sentiment.Count)

# Sentiment Index Level
sentiment.index.df$pred.daily.score = sentiment.index.df$Pred.Sentiment.Count / sentiment.index.df$Total.Articles
sentiment.index.df$actual.daily.score = sentiment.index.df$Actual.Sentiment.Count / sentiment.index.df$Total.Articles

# Predicted Sentiment Index
sentiment.index.df$pred.cum.score = cumsum(sentiment.index.df$pred.daily.score)
sentiment.index.df$pred.index = 100
sentiment.index.df$pred.index = sentiment.index.df$pred.index + sentiment.index.df$pred.cum.score

# Actual Sentiment Index
sentiment.index.df$actual.cum.score = cumsum(sentiment.index.df$actual.daily.score)
sentiment.index.df$actual.index = 100
sentiment.index.df$actual.index = sentiment.index.df$actual.index + sentiment.index.df$actual.cum.score

# De-mean the indexes
sentiment.index.df$demeaned.pred = sentiment.index.df$pred.index - mean(sentiment.index.df$pred.index)
returns$demeaned.index = returns$index.level - mean(returns$index.level)

# Normalised indexes
sentiment.index.df$norm.pred = sentiment.index.df$demeaned.pred / sd(sentiment.index.df$pred.index)
returns$norm.index = returns$demeaned.index / sd(returns$index.level)

# Subset returns DF for index levels and Dates
returns.trim = returns[ , c(1, 28, 29, 30, 31)]

# Subset the sentiment DF for pred index levels and Dates
sentiment.trim = sentiment.index.df[ , c(1, 10, 13, 14, 15,17)]

# Merge the trimmed returns DF with the sentiment DF based on the sentiment dates
merged_df = merge(sentiment.trim, returns.trim, by = "Date")

##### Graph of Sentiment vs Return Index
plot(returns$Date, returns$norm.index, type = "l", xlab = "Date", ylab = "Index Level", col = "black", lty = 2)
par(new = TRUE)
plot(sentiment.index.df$Date, sentiment.index.df$norm.pred, yaxt = "n", xaxt="n", col = "black", type = "l", ylab = "", 
     xlab = "")
legend("bottomleft", c("Return Index", "Sentiment Index"), col = c("black", "black"), lty=c(2, 1),
       cex = 0.75)
title("Sentiment Index vs Return Index")

### Correlation between the normalised sentiment and stock indexes
# Cor on full timeframe
corr_indexes_full = cor(merged_df$norm.pred, merged_df$norm.index)
corr_indexes_full # -0.08

# Cor on bear market: End Sept to End Dec
corr_indexes_bear = cor(merged_df[191:255, 6], merged_df[191:255, 10])
corr_indexes_bear # 0.77

# Cor Jan - end April 
corr_indexes_h1 = cor(merged_df[1:84, 6], merged_df[1:84, 10])
corr_indexes_h1 # 0.46

# Cor May - End July
corr_indexes_mayjul = cor(merged_df[85:149, 6], merged_df[85:149, 10])
corr_indexes_mayjul # -0.50

# Start July to End sept
corr_indexes_julsep = cor(merged_df[129:191, 6], merged_df[129:191, 10])
corr_indexes_julsep 

# Cor Jan - end June
corr_indexes_janjun = cor(merged_df[1:128, 2], merged_df[1:128, 6])
corr_indexes_janjun
