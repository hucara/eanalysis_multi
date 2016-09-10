# ----------------------------------------
# Emotion analisis - Master DATCOM 2016
# 
# ----------------------------------------
# Useful functions for data retrieving,
# formatting and storing from twitter.
# ----------------------------------------

#' Build Tweets data frame structure. We use internal twitteR functions. And take care
#' of the tweets with url in them and without. twListToDF gave problems with this.
#'
#' \code{tweetsToDF} creates a data frame structure to store all the tweets retrieved
#' 
#' @param twitteR status type list with the tweets information
#' @return None
#' @example tweetsToDF(tweets)
#' @export
tweetsToDF <- function(tweetList)
{
  if(length(tweetList) <= 0)
  {
    message("TweetsToDF: tweetList has 0 rows")
    return()
  }
    
  if(require(twitteR) && require(plyr))
  {
    df <- adply(.data = tweetList, .margins = 1, .progress = progress_text(char = "."), .fun = function(i)
    {
      if(dim(i$urls)[1] > 0)
      {
        t <- i$`toDataFrame#twitterObj`()
      }
      else
      {
        t <- i$toDataFrame()
        t$urls.url <- NA
        t$urls.expanded_url <- NA
        t$urls.display_url <- NA
        t$urls.start_index <- NA
        t$urls.stop_index <- NA
      }

      return(t)
    })
    
    df$X1 <- NULL
  }
  return(df)
}

#' Returns a list with the hashtags for each element of the dataframe
#'
#' \code{gettHashtags} returns a list with the hashtags for each element of the dataframe
#' 
#' @param array with the texts to process
#' @return list with the hashtags for each element of texts
#' @example gettHashtags(texts)
#' @export
getHashtags <- function(texts)
{
  if(require(plyr) && require(stringr) && length(texts)>0)
  {
    hashtags <- aaply(.data = texts, .margins = 1, .progress = progress_text(char = "."), .fun = function(i)
    {
      toString(unlist(str_extract_all(i, "#\\S+")))
    })

    return(hashtags)
  }
}

getEmojis <- function(texts)
{
  if(require(plyr) && require(stringr) && length(texts)>0)
  {
    emojis <- aaply(.data = texts, .margins = 1, .progress = progress_text(char = "."), .fun = function(i)
    {
      i <- iconv(i, "UTF-8", "UTF-8", sub="byte")
      #i <- unlist(str_extract_all(i, pattern='<\\w+?>'))
      i <- unlist(str_extract_all(i, pattern='<U\\+....>|<ed>.*?<ed>.*?>.*?>'))
      i <- paste(i, collapse=',')
    })
    
    return(emojis)
  }
}


getEmojiKeywords <- function(emojiCodes, emojiDict)
{
  if(require(plyr) && require(stringr) && length(emojiCodes)>0)
  {
    emojis <- aaply(.data = emojiCodes, .margins = 1, .progress = progress_text(char = "."), .fun = function(i)
    {
      elist <- strsplit(i, ",")
      words <- ""
      for(j in length(elist))
      {
        w <- emojiDict[emojiDict$R.encoding == elist[j],"Description"]
        words <- paste(words, w, collapse=",")
      }
      return(words)
    })
    return(emojis)
  }
}

getBalancedDataset <- function(tweets, num)
{
  if(require(dplyr))
  {
    num <- as.integer(num)
    
    happyNum <- nrow(tweets[tweets$class == "happy",])
    sadNum <- nrow(tweets[tweets$class == "sad",])
    disgustedNum <- nrow(tweets[tweets$class == "disgusted",])
    angryNum <- nrow(tweets[tweets$class == "angry",])
    surprisedNum <- nrow(tweets[tweets$class == "surprised",])
    afraidNum <- nrow(tweets[tweets$class == "afraid",])
    
    shappy <- sample_n(tweets[tweets$class == "happy",], if_else(num > happyNum, happyNum, num)) 
    ssad <- sample_n(tweets[tweets$class == "sad",], if_else(num > sadNum, sadNum, num))
    sdisgusted <- sample_n(tweets[tweets$class == "disgusted",], if_else(num > disgustedNum, disgustedNum, num))
    sangry <- sample_n(tweets[tweets$class == "angry",], if_else(num > angryNum, angryNum, num))
    ssurprised <- sample_n(tweets[tweets$class == "surprised",], if_else(num > surprisedNum, surprisedNum, num))
    safraid <- sample_n(tweets[tweets$class == "afraid",], if_else(num > afraidNum, afraidNum, num))
    
    stweets <- rbind(shappy, ssad, sdisgusted, sangry, ssurprised, safraid)
    gc()
    return(stweets)
  }
}