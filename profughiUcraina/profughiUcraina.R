library(rtweet)
library(stringr)

tweet_df <- search_tweets("#accoglienzaucraina sono from:viminale", 
                          include_rts = FALSE)

temp <- as.data.frame(tweet_df[1:5])
temp$created_at <- as.character(temp$created_at)



#load previous version
if(file.exists("tweet_df_hist.csv")){
  hist <- read.csv("tweet_df_hist.csv", colClasses = "character")
  temp <- temp[!(temp$status_id %in% hist$status_id),]
  hist <- rbind(temp, hist)
}else{
  hist <- temp
}

n <- nrow(temp)

if(n>0){
 
  temp$text <- gsub("\\.", "", temp$text)
  
  res <- data.frame(matrix(NA, nrow = n, ncol = 5))
  colnames(res) <- c("Data", "Totale", "Uomini", "Donne", "Minori")  
  
  for(i in 1:n){
    res$Data[i]   <- temp$created_at[i]
    res$Totale[i] <- as.numeric(str_extract(temp$text[i],"[0-9]+(?= le)"))
    res$Donne[i]  <- as.numeric(str_extract(temp$text[i],"[0-9]+(?= donne)"))
    res$Uomini[i] <- as.numeric(str_extract(temp$text[i],"[0-9]+(?= uomini)"))
    res$Minori[i] <- as.numeric(str_extract(temp$text[i],"[0-9]+(?= minori)"))
  }
  
  res$Data <- as.character(as.Date(res$Data))
  
  if(file.exists("outputProfughi.csv")){
    hist_res <- read.csv("outputProfughi.csv")
    res <- rbind(res, hist_res)
  }
 
  write.csv(res, file = "outputProfughi.csv", row.names = F)
  write.csv(hist, file = "tweet_df_hist.csv", row.names = F)
}


  