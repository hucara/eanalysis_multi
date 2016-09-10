#'----------------------------------------
#' Emotion analisis - Master DATCOM 2016
#' 
#' ---------------------------------------
#' Emoji retrieving and encoding functions
#' ---------------------------------------

require(rvest)
require(magrittr)
require(dplyr)

fillEmojisKeywords <- function(emojis)
{
  for(i in 1:length(emojis))
  {
    if(length(emojis$Keywords[i]) == 0)
      emojis$Keywords[i] <- emojis$Keywords[i-1]
  }
  
  return(emojis)
}

fixEmojiName <- function(name)
{
  pos <- gregexpr("â‰š", name)[[1]][1]
  if(pos > 0)
  {
    name <- substr(name, 0, pos-1)
  }
  return(name)
}

#' Builds a table with all the emojis and their Hex Code Point
#' and their Hex UTF-8 bytes encoding. In the case of twitter,
#' we are interested in the second one. The table is obtained
#' via scrapping of the unicode.org website.
#' 
#' http://www.unicode.org/emoji/charts/full-emoji-list.html
#' 
#' Also contains a name for the emoji and keywords that will
#' help us to build an emotion analysis for the emoji.
#'
#' \code{buildUnicodeEmojis} creates a data frame structure to store all the tweets retrieved
#' 
#' @param None
#' @return a dataframe with the unicode emojis
#' @example buildUnicodeEmojis()
#' @export
buildUnicodeEmojis <- function()
{
  cat("\n Scrapping Unicode webpage...")
  url <- "http://www.unicode.org/emoji/charts/full-emoji-list.html"
  
  # get emojis
  emojis <- url %>%
    read_html() %>%
    html_nodes(xpath='/html/body/div[3]/table[1]') %>%
    html_table()
  
  cat("\n Fixing some stuff...")
  # convert to dataframe selecting the interesting data
  emojis <- data.frame(emojis[[1]]$Code, tolower(emojis[[1]]$Name), emojis[[1]]$Keywords, stringsAsFactors = FALSE)
  names(emojis) <- c("Code", "Name", "Keywords")
  
  # remove headers
  emojis <- emojis[!emojis$Code == "Code",]
  
  # sort by code
  # emojis <- emojis[order(emojis$Code, decreasing = FALSE),]
  
  # remove duplicates and "skin colored" emojis
  emojis <- emojis[!emojis$Keywords == "",]
  
  # fix the names
  for(i in 1:length(emojis$Code))
  {
    emojis$Name[i] <- fixEmojiName(emojis$Name[i])
  }
  emojis$Name <- str_to_upper(emojis$Name)
  
  cat("\n Merging with dictionary...")
  # read dictionary and merge
  dcit <- as.data.frame(read.csv("./data/emoji_dictionary.csv", sep=";"))
  View(dcit)
  View(emojis)
  # merge emojis with dictionary
  emojis <- merge(emojis, dcit, by.x="Name", by.y="Description")
  
  cat("\n Done!")
  return(emojis)
}

readEmoticonDictionary <- function(file)
{
  emoticons <- as.data.frame(read.csv(file, sep=";", encoding = "UTF-8", stringsAsFactors = FALSE))
  emoticons$emotes <- strsplit(emoticons$emotes, split=" ")
  emoticons$words <- strsplit(emoticons$words, split=", ")
  
  gc()
  return(emoticons)
}

readEmojiDictionary <- function(file)
{
  emojis <- as.data.frame(read.csv(file, sep=";", stringsAsFactors=FALSE))
  gc()
  return(emojis)
}

getEmoticonsWord <- function(texts, emoticonDict)
{
  if(require(plyr) && require(stringr) && length(texts)>0)
  {
    etexts <- aaply(.data = texts, .margins = 1, .progress = progress_text(char = "."), .fun = function(i)
    {
      tk <- str_split(i, pattern = " ")[[1]]
      word <- ""
      for(j in 1:length(emoticonDict$emotes))
      {
        # if there is any emoicon of this kind in the tokenized text...
        if(length(intersect(tk, emoticonDict$emotes[j][[1]])) != 0)
        {
          word <- sample(emoticonDict$words[j][[1]], 1)
        }
      }
      return(word)
    })
    return(etexts)
  }
}
