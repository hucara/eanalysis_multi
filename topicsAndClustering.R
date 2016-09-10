require(topicmodels)
#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

# Remove all rows/documents with zero terms.
# LDA will complain if we don't do this
base.dtm <- train.dtm
rowTotals <- apply(base.dtm , 1, sum)
dtm.new   <- base.dtm[rowTotals> 0, ]

# get frequencies
v <- sort(colSums(as.matrix(dtm.new)), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

# plot terms more freq than 30
library(ggplot2)
p <- ggplot(subset(d, d$freq > 40), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=90, hjust=1))
p <- p + labs(x = "Término", y = "Num. apariciones", title = "Frecuencias de términos en #Rio2016")
p

# plot frequency distribution
t <- as.data.frame(table(d$freq))
p <- ggplot(subset(t, t$Freq>20), aes(Var1, Freq, group = 1)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(x = "Num. Apariciones", y = "Num. Términos", title = "Frecuencias de términos en #Rio2016") +
  geom_point() + 
  geom_line(color = "Red")
p


barplot(table(d$freq))

p <- ggplot(subset(d, d$freq > 350), aes(word, freq))
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p

#Run LDA using Gibbs sampling
k <- 6
ldaOut <-LDA(dtm.new, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

control_LDA_Gibbs <- list(alpha = 50/k, estimate.beta = TRUE,
                          verbose = 0, prefix = tempfile(), save = 0, keep = 0,
                          seed = as.integer(Sys.time()), nstart = 1, best = TRUE,
                          delta = 0.1, iter = 2000, burnin = 0, thin = 2000)

ldaOut <-LDA(dtm.new, k, method="Gibbs", control = control_LDA_Gibbs)

#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.terms <- as.matrix(terms(ldaOut,6))


# TOPICS BY EMOTION
dtmHappy <- dtm.new[which(train.y == "happy"),]
k <- 4
ldaOut <-LDA(dtmHappy, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

control_LDA_Gibbs <- list(alpha = 50/k, estimate.beta = TRUE,
                          verbose = 0, prefix = tempfile(), save = 0, keep = 0,
                          seed = as.integer(Sys.time()), nstart = 1, best = TRUE,
                          delta = 0.1, iter = 2000, burnin = 0, thin = 2000)

ldaOut <-LDA(dtmHappy, k, method="Gibbs", control = control_LDA_Gibbs)

#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.terms <- as.matrix(terms(ldaOut,6))
ldaOut.terms

# TOPICS BY EMOTION
dtmSad <- dtm.new[which(train.y == "sad"),]

# number of topics
k <- 6

ldaOut <-LDA(dtmSad, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

control_LDA_Gibbs <- list(alpha = 50/k, estimate.beta = TRUE,
                          verbose = 0, prefix = tempfile(), save = 0, keep = 0,
                          seed = as.integer(Sys.time()), nstart = 1, best = TRUE,
                          delta = 0.1, iter = 2000, burnin = 0, thin = 2000)

ldaOut <-LDA(dtmSad, k, method="Gibbs", control = control_LDA_Gibbs)

#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.terms <- as.matrix(terms(ldaOut,3))
ldaOut.terms

require(cluster)
# # clustering for term similarity
# b.dtm.ns <- removeSparseTerms(base.dtm, 0.99)
# b.dtm.ns
# 
# require(cluster)
# d <- dist(t(b.dtm.ns), method ="euclidian")
# fit <- hclust(d = d, method = "ward.D")
# fit
# plot(fit, hang = -1)