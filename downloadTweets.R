
# Download classifying and cleaning tweets

rm(list = ls())
source("twitterAuthorization.R")
source("dataRetriever.R")
source("preprocessing.R")
source("corpusBuilding.R")
source("emojiRetriever.R")
num <- 10000
size <- "big"

emojiDict <- readEmojiDictionary("./data/emoji_dictionary.csv")
emoteDict <- readEmoticonDictionary("./data/emote_dictionary.csv")

downloadTweets <- function(num, search, class, emojiDict = NULL, emoteDict = NULL)
{
  tweets <- data.frame()
  for(i in 1:length(search))
  {
    message("\nSearch: ", search[i])
    s <- searchTwitter(search[i], n=num, lang = "en")
    message("Transformation...")
    s <- tweetsToDF(s)
    
    if(!is.null(emoteDict))
    {
      message("Dealing with ASCII emotes...")
      emoteWord <- getEmoticonsWord(s$text, emoteDict)
      s$text <- paste(s$text, emoteWord, sep = " ")
    }
    
    message("Parsing hashtags...")
    s$hashtags <- getHashtags(s$text)
    message("Parsin emojis...")
    s$emojis <- getEmojis(s$text)
    
    if(!is.null(emojiDict))
    {
      message("Adding emoji keywords to text...")
      emoKWs <- getEmojiKeywords(s$emojis, emojiDict)
      s$text <- paste(s$text, emoKWs)
    }
    
    message("Cleaning...")
    s$ctext <- cleanTexts(s$text)
    s$chtext <- cleanTexts(s$text, keepHashtags = FALSE)
    s$class <- as.factor(class)
    tweets <- rbind(tweets, s)
  }
  message("Done! Tweets: ", dim(tweets)[1], " variables: ", dim(tweets)[2])
  
  gc()
  return(tweets)
}

#happy
happySyn <- c("#happy", "#joy", "#bliss", "#happiness")
happy <- data.frame()
repeat
{
  h <- downloadTweets(num, happySyn, "happy", emojiDict, emoteDict)
  happy <- rbind(happy, h)
  happy <- unique(happy)
  
  message("Current #happy unique tweets: ", nrow(happy))
  if(nrow(happy)*length(happySyn) < num)
  {
    Sys.sleep(180)
  }
  else
  {
    rm(h)
    break;
  }
}

#sad
sadSyn <- c("#sad","#unhappy","#depressed", "#bitter", "#heartbroken", "#dismay")
sad <- data.frame()
repeat
{
  s <- downloadTweets(num, sadSyn, "sad", emojiDict, emoteDict)
  sad <- rbind(sad, s)
  sad <- unique(sad)
  
  message("Current #sad unique tweets: ", nrow(sad))
  if(nrow(sad)*length(sadSyn) < num)
  {
    Sys.sleep(180)
  }
  else
  {
    rm(s)
    break;
  }
}

#surprised
surprisedSyn <- c("#surprised","#shocked","#amazed", "#astonished", "#omg")
surprised <- data.frame()
repeat
{
  su <- downloadTweets(num, surprisedSyn, "surprised", emojiDict, emoteDict)
  surprised <- rbind(surprised, su)
  surprised <- unique(surprised)

  message("Current #surprised unique tweets: ", nrow(surprised))
  if(nrow(surprised)*length(surprisedSyn) < num)
  {
    Sys.sleep(180)
  }
  else
  {
    rm(su)
    break;
  }
}

#afraid
afraidSyn <- c("#afraid","#scared","#worried", "#fear", "#angst", "#horror")
afraid <- data.frame()
repeat
{
  af <- downloadTweets(num, afraidSyn, "afraid", emojiDict, emoteDict)
  afraid <- rbind(afraid, af)
  afraid <- unique(afraid)
  
  message("Current #afraid unique tweets: ", nrow(afraid))
  if(nrow(afraid)*length(afraidSyn) < num)
  {
    Sys.sleep(180)
  }
  else
  {
    rm(af)
    break;
  }
}

#angry
angrySyn <- c("#angry","#mad","#furious", "#annoyed", "#rage", "#jealous", "#jelly", "#frustrated", "#outrage", "#grumpy", "#anger")
angry <- data.frame()
repeat
{
  an <- downloadTweets(num, angrySyn, "angry", emojiDict, emoteDict)
  angry <- rbind(angry, an)
  angry <- unique(angry)
  
  message("Current #angry unique tweets: ", nrow(angry))
  if(nrow(angry)*length(angrySyn) < num)
  {
    Sys.sleep(180)
  }
  else
  {
    rm(an)
    break;
  }
}

#disgusted
disgustedSyn <- c("#disgusted","#sickened","#offended", "#sick", "#weary", "#wtf")
disgusted <- data.frame()
repeat
{
  di <- downloadTweets(num, disgustedSyn, "disgusted", emojiDict, emoteDict)
  disgusted <- rbind(disgusted, di)
  disgusted <- unique(disgusted)
  
  message("Current #disgusted unique tweets: ", nrow(disgusted))
  if(nrow(disgusted)*length(disgustedSyn) < num)
  {
    Sys.sleep(180)
  }
  else
  {
    rm(di)
    break;
  }
}

tweets <- rbind(happy, sad, surprised, afraid, angry, disgusted)
write.csv(tweets, "./data/big_tweets.csv")

write.csv(happy, "./data/big_happy.csv")
write.csv(sad, "./data/big_sad.csv")
write.csv(surprised, "./data/big_surprised.csv")
write.csv(afraid, "./data/big_afraid.csv")
write.csv(angry, "./data/big_angry.csv")
write.csv(disgusted, "./data/big_disgusted.csv")
