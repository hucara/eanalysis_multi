# corpus building

createCleanCorpus <- function(texts, stopwords)
{
  if(require(tm) && require(twitteR) && require(SnowballC) && length(texts)>0)
  {
    # input must be a vectorized corpus
    corpus <- Corpus(VectorSource(texts))
    message("Removing punctuation...")
    corpus <- tm_map(corpus, removePunctuation)
    message("Removing stopwords...")
    corpus <- tm_map(corpus, removeWords, stopwords)
    message("Stemming document...")
    corpus <- tm_map(corpus, stemDocument, language="en")
    message("Cleaning whitespaces...")
    corpus <- tm_map(corpus, stripWhitespace)
    message("Transforming to lower case...")
    corpus <- tm_map(corpus, content_transformer(tolower))
    message("Done!")
    
    return(corpus)
  }
}