install.packages("twitteR") #install package
library(twitteR) #load package

consumer_key <- "HnONg8EcrUfKPWEf7i8RIK9Km" 
consumer_secret <- "rf1vermzBhNz08encQwqh7xnuwLcBhNbp6qBU3oEQcgM1byTfg" 
access_token <- "1319973292262985728-qADGWHmGbK7SMPb2uYhcomGK8oVGUx"
access_secret <- "Xp39swNAzwmPLAeILPZrqefQQY0lNmK2R1Ng82itMaGEG"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

virus <- searchTwitter('#wendlergate', n = 200, since = '2020-09-01', retryOnRateLimit = 1e3)
virus_df = twListToDF(virus)