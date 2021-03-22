install.packages("twitteR") #install package
library(twitteR) #load package

consumer_key <- #consumer_key 
consumer_secret <- #consumer_secret
access_token <- #access_token
access_secret <- #access_secret
#keys not displayed due to privacy reasons

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

virus <- searchTwitter('#wendlergate', n = 200, since = '2020-09-01', retryOnRateLimit = 1e3)
virus_df = twListToDF(virus)