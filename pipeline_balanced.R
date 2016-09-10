require(caret)
require(tm)
source("dataRetriever.R")
source("corpusBuilding.R")
source("stopWords.R")
set.seed(1234)

# Read the data
tweets <- read.csv("./data/used_tweets.csv", stringsAsFactors = FALSE)
tweets$class <- as.factor(tweets$class)
tweets$X <- NULL
dim(tweets)

prop.table(table(tweets$class))
table(tweets$class)

# create partitions with equivalent class sample ratios
inTrain <- createDataPartition(y=tweets[,"class"], p = .80, list = FALSE)
train <- createCleanCorpus(tweets$chtext[inTrain], sw)
test <- createCleanCorpus(tweets$chtext[-inTrain], sw)

prop.table(table(tweets$class[inTrain]))
length(test)
length(train)

# create dtms to build an intersected dictionary with them
train.dtm <- DocumentTermMatrix(train, control = list(wordLengths = c(3, 10)))
test.dtm <- DocumentTermMatrix(test, control = list(wordLengths = c(3, 10)))
train.dtm
test.dtm

train.d <- findFreqTerms(train.dtm, lowfreq = 7)
test.d <- findFreqTerms(test.dtm, lowfreq = 1)

dict <- intersect(train.d, test.d)
length(dict)
head(dict)

# create the real dtm
train.dtm <- DocumentTermMatrix(train, control = list(dictionary=dict, weighting = tm::weightTf))
test.dtm <- DocumentTermMatrix(test, control = list(dictionary=dict, weighting = tm::weightTf))
# train.dtm <- DocumentTermMatrix(train, control = list(dictionary=dict, weighting = tm::weightTfIdf))
# test.dtm <- DocumentTermMatrix(test, control = list(dictionary=dict, weighting = tm::weightTfIdf))

# remove empty documents due to the intersect dictionary
rowTotalsTrain <- apply(train.dtm, 1, sum)
train.dtm   <- train.dtm[rowTotalsTrain> 0,]
rowTotalsTest <- apply(test.dtm, 1, sum)
test.dtm   <- test.dtm[rowTotalsTest> 0,] 

# we also have to adjust the class values for the documents
train.y <- tweets$class[inTrain]
train.y <- train.y[which(rowTotalsTrain > 0)]

test.y <- tweets$class[-inTrain]
test.y <- test.y[which(rowTotalsTest > 0)]

# conversion to matrix
mtrain <- as.matrix(train.dtm)
mtest <- as.matrix(test.dtm)

save(mtrain, file = "./mat/balanced_train.Rdata")
save(mtest, file = "./mat/balanced_test.Rdata")
save(train.y, file = "./mat/balanced_train_y.Rdata")
save(test.y, file = "./mat/balanced_test_y.Rdata")
save(train.dtm, file = "./mat/balanced_train_dtm.Rdata")
save(test.dtm, file = "./mat/balanced_test_dtm.Rdata")

# clean a bit of memory
rm(train.dtm)
rm(test.dtm)
rm(ctweets)
gc()
