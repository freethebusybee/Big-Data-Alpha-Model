install.packages('quantmod')
install.packages('PerformanceAnalytics')
library(quantmod)
library(PerformanceAnalytics)
library(dplyr)
library(lubridate)


sym.vec <- c("NVDA", "AAPL",'BTC-USD','NFLX')
getSymbols(sym.vec, from = "2019-01-01", to = "2023-12-31")


for(sym in sym.vec) {
  sym_var <- gsub("-", "_", sym)
  
  stock <- get(sym)[, paste0(sym, ".Adjusted"), drop = FALSE]
  
  stock.logret <- diff(log(stock))
  stock.logret[1] <- 0
  
  assign(paste0(sym_var, ".logret"), stock.logret)
  
  stock.monthly <- to.monthly(stock, indexAt = "lastof", OHLC = FALSE)
  stock.monthly.logret <- diff(log(stock.monthly))
  
  assign(paste0(sym_var, "_monthly_logret"), stock.monthly.logret)
}

#Log plots

plot(NVDA.logret, main = "NVDA Daily Logarithmic Returns") 

plot(NVDA_monthly_logret, main = "NVDA Monthly Logarithmic Returns") 

plot(NFLX.logret, main = "NFLX Daily Logarithmic Returns") 

plot(NFLX_monthly_logret, main = "NFLX Monthly Logarithmic Returns") 

plot(AAPL.logret, main = "AAPL Daily Logarithmic Returns") 

plot(AAPL_monthly_logret, main = "AAPL Monthly Logarithmic Returns") 

plot(BTC_USD.logret, main = "BTC-USD Daily Logarithmic Returns")
plot(BTC_USD_monthly_logret, main = "BTC-USD Monthly Logarithmic Returns")



#Sentiment Data
install.packages("RedditExtractoR")
library(RedditExtractoR)

#Apple Reddit
apple_thread_urls <- find_thread_urls(keywords = "Apple", sort_by = "top",subreddit='Business',period = "all")
apple_thread_urls$date_utc <- as.Date(apple_thread_urls$date_utc)
apple_reddit <- subset(apple_thread_urls, date_utc > as.Date("2019-01-01"))
apple_reddit <- apple_reddit[, c("date_utc", "title", "comments")]

#Netflix Reddit
ntflx_thread_urls <- find_thread_urls(keywords = "Netflix", sort_by = "top",subreddit='Business',period = "all")
ntflx_thread_urls$date_utc <- as.Date(ntflx_thread_urls$date_utc)
ntflx_reddit <- subset(ntflx_thread_urls, date_utc > as.Date("2019-01-01"))
ntflx_reddit <- ntflx_reddit[, c("date_utc", "title", "comments")]

#Nvidia Reddit
nvdia_thread_urls <- find_thread_urls(keywords = "NVDA", sort_by = "top",subreddit='Business',period = "all")
nvdia_thread_urls$date_utc <- as.Date(nvdia_thread_urls$date_utc)
nvdia_reddit <- subset(nvdia_thread_urls, date_utc > as.Date("2019-01-01"))
nvdia_reddit <- nvdia_reddit[, c("date_utc", "title", "comments")]

#BTC Reddit
btc_thread_urls <- find_thread_urls(keywords = "Bitcoin", sort_by = "top",subreddit='Business',period = "all")
btc_thread_urls$date_utc <- as.Date(btc_thread_urls$date_utc)
btc_reddit <- subset(btc_thread_urls, date_utc > as.Date("2019-01-01"))
btc_reddit <- btc_reddit[, c("date_utc", "title", "comments")]


library(data.table) 

#Twitter
bitcoin_twitter_tweet<- fread('/Users/haleemashahzad/Desktop/Datasets/bitcoin_twitter_tweet.csv')
netflix_twitter_tweet<- fread('/Users/haleemashahzad/Desktop/Datasets/netflix_twitter.csv')
apple_twitter_tweet<- fread('/Users/haleemashahzad/Desktop/Datasets/AAPL_filtered.csv')
nvdia_twitter_tweet<- fread('/Users/haleemashahzad/Desktop/Datasets/Nvidia_tweets.csv')

#Trading data 
trading_df <- fread('/Users/haleemashahzad/Desktop/Datasets/monthly_averages.csv')

#US Economy Data 


# Companies Income statements



install.packages("syuzhet")
library(syuzhet)
install.packages("ggplot2")
library(ggplot2)
install.packages('dplyr')
library(dplyr)


#bitcoin_reddit
mySentiment_BTC_reddit <- get_nrc_sentiment(btc_reddit$title)

long_mySentiment_btc_reddit <- melt(mySentiment_BTC_reddit, variable.name = "Sentiment", value.name = "Count")

sentimentTotals_btc_reddit <- aggregate(Count ~ Sentiment, data = long_mySentiment_btc_reddit, sum)

ggplot(data = sentimentTotals_btc_reddit, aes(x = Sentiment, y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Sentiment", y = "Total Count", title = "BTC Sentiment Score_reddit") +
  coord_flip()

#bitcoin_twitter
mySentiment_BTC_tweet <- get_nrc_sentiment(bitcoin_twitter_tweet$text)
long_mySentiment_btc_twitter <- melt(mySentiment_BTC_tweet, variable.name = "Sentiment", value.name = "Count")
sentimentTotals_btc_twitter <- aggregate(Count ~ Sentiment, data = long_mySentiment_btc_twitter, sum)
ggplot(data = sentimentTotals_btc_twitter, aes(x = Sentiment, y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Sentiment", y = "Total Count", title = "BTC Sentiment Score_twitter") +
  coord_flip()

#bitcoin Returns

trading_df$MonthYear <- as.Date(paste0(trading_df$Month.Year, "-01"))

ggplot(trading_df, aes(x = MonthYear)) +
  geom_line(aes(y = trading_df$`BTC-USD.Adjusted_AVG`, colour = "Adjusted Price")) +
  geom_line(aes(y = trading_df$`BTC-USD.TotalVolume_AVG` / max(trading_df$`BTC-USD.TotalVolume_AVG`) * max(trading_df$`BTC-USD.Adjusted_AVG`), colour = "Total Volume")) +
  labs(title = "BTC-USD Monthly Trading Data", x = "Date", y = "Value") +
  scale_colour_manual("", values = c("Adjusted Price" = "blue", "Total Volume" = "red")) +
  theme_minimal()



#apple reddit 
mySentiment_apple <- get_nrc_sentiment(apple_reddit$title)

long_mySentiment_apple <- melt(mySentiment_apple, variable.name = "Sentiment", value.name = "Count")

sentimentTotals_apple<- aggregate(Count ~ Sentiment, data = long_mySentiment_apple, sum)

ggplot(data = sentimentTotals_apple, aes(x = Sentiment, y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Sentiment", y = "Total Count", title = "Apple Sentiment Score") +
  coord_flip()

#apple twitter 
ggplot(apple_twitter_tweet, aes(x = date, y = twitter_volume)) +
  geom_col(fill = "blue") + # geom_col is used for bar plots
  labs(title = "Apple_Monthly Tweet Counts",
       x = "Date",
       y = "Tweet Count") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + # Customizing the date breaks and labels
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Plot for Apple (AAPL)
ggplot(trading_df, aes(x = MonthYear)) +
  geom_line(aes(y = AAPL.Adjusted_AVG, colour = "Adjusted Price")) +
  geom_line(aes(y = AAPL.TotalVolume_AVG / max(AAPL.TotalVolume_AVG) * max(AAPL.Adjusted_AVG), colour = "Total Volume")) +
  labs(title = "AAPL Monthly Trading Data", x = "Date", y = "Value") +
  scale_colour_manual("", values = c("Adjusted Price" = "blue", "Total Volume" = "red")) +
  theme_minimal()


#netflix reddit
mySentiment_ntflx <- get_nrc_sentiment(ntflx_reddit$title)
long_mySentiment_ntflx <- melt(mySentiment_ntflx, variable.name = "Sentiment", value.name = "Count")

sentimentTotals_ntflx<- aggregate(Count ~ Sentiment, data = long_mySentiment_ntflx, sum)

ggplot(data = sentimentTotals_ntflx, aes(x = Sentiment, y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Sentiment", y = "Total Count", title = "Netflix Sentiment Score") +
  coord_flip()

#netflix twitter 
ggplot(netflix_twitter_tweet, aes(x = month_year, y = twt_count)) +
  geom_col(fill = "blue") + # geom_col is used for bar plots
  labs(title = "Netflix_Monthly Tweet Counts",
       x = "Date",
       y = "Tweet Count") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + # Customizing the date breaks and labels
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Plot for Netflix (NFLX)
ggplot(trading_df, aes(x = MonthYear)) +
  geom_line(aes(y = NFLX.Adjusted_AVG, colour = "Adjusted Price")) +
  geom_line(aes(y = NFLX.TotalVolume_AVG / max(NFLX.TotalVolume_AVG) * max(NFLX.Adjusted_AVG), colour = "Total Volume")) +
  labs(title = "NFLX Monthly Trading Data", x = "Date", y = "Value") +
  scale_colour_manual("", values = c("Adjusted Price" = "blue", "Total Volume" = "red")) +
  theme_minimal()


#nvidia reddit 
mySentiment_Nvdia <- get_nrc_sentiment(nvdia_reddit$title)
long_mySentiment_Nvdia <- melt(mySentiment_Nvdia, variable.name = "Sentiment", value.name = "Count")

sentimentTotals_Nvdia<- aggregate(Count ~ Sentiment, data = long_mySentiment_Nvdia, sum)

ggplot(data = sentimentTotals_Nvdia, aes(x = Sentiment, y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Sentiment", y = "Total Count", title = "Nvidia Sentiment Score") +
  coord_flip()

#nvidia twitter
mySentiment_Nvdia_tweet <- get_nrc_sentiment(nvdia_twitter_tweet$Text)
long_mySentiment_Nvdia_twitter <- melt(mySentiment_Nvdia_tweet, variable.name = "Sentiment", value.name = "Count")
sentimentTotals_Nvidia_twitter <- aggregate(Count ~ Sentiment, data = long_mySentiment_Nvdia_twitter, sum)
ggplot(data = sentimentTotals_Nvidia_twitter, aes(x = Sentiment, y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Sentiment", y = "Total Count", title = "Nvidia Sentiment Score_twitter") +
  coord_flip()

# Plot for NVDA (NVDA)
ggplot(trading_df, aes(x = MonthYear)) +
  geom_line(aes(y = NVDA.Adjusted_AVG, colour = "Adjusted Price")) +
  geom_line(aes(y = NVDA.TotalVolume_AVG / max(NVDA.TotalVolume_AVG) * max(NVDA.Adjusted_AVG), colour = "Total Volume")) +
  labs(title = "NVDA Monthly Trading Data", x = "Date", y = "Value") +
  scale_colour_manual("", values = c("Adjusted Price" = "blue", "Total Volume" = "red")) +
  theme_minimal()

#Break 

install.packages("reshape2")
library(reshape2)

Reddit_combined_df <- rbind(apple_thread_urls, ntflx_thread_urls, Nvdia_thread_urls, BTC_thread_urls)
Reddit_URl_df <- Reddit_combined_df$url
Comments_apple_reddit <- get_thread_content(Reddit_URl_df)

install.packages('scales','reshape2','NLP','openNLP')
library(scales); library(reshape2)
library(NLP); library(openNLP)

negative_words <- readLines("/Users/haleemashahzad/Desktop/negative-words.txt")
positive_words <- readLines("/Users/haleemashahzad/Desktop/positive-words.txt")

mySentiment <- get_nrc_sentiment(apple_titles_df$Title)
long_mySentiment <- melt(mySentiment, variable.name = "Sentiment", value.name = "Count")
sentimentTotals <- aggregate(Count ~ Sentiment, data = long_mySentiment, sum)


