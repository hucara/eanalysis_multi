require(caret)
require(doParallel)

# load data
# source("loadUsedTweets.R")

set.seed(1234)
cluster <- makeCluster(6)
registerDoParallel(cluster)

# Normal distribution test (d is built in graphs.R)
shapiro.test(d$freq)
qqnorm(d$freq)
qqline(d$freq, col = 2)

# WITH e1071
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

m2train <- apply(mtrain, 2, convert_count)
m2test <- apply(mtest, 2, convert_count)

nb_tuned <- e1071::naiveBayes(m2train, train.y, laplace = 0, type = c("raw"))
nb_pred <- predict(nb_tuned, m2test)
nb_pred_probs <- predict(nb_tuned, m2test, type = "raw")
nb_conf <- confusionMatrix(nb_pred, as.factor(test.y))
nb_mauc <- pROC::multiclass.roc(as.numeric(nb_pred), as.numeric(test.y))
nb_auc  <- pROC::auc(as.numeric(nb_pred), as.numeric(test.y))
nb_sum <- caret::multiClassSummary(data.frame(obs = test.y, pred=nb_pred), lev = as.numeric(test.y), nb_tuned)



# Tune with caret and decide which results are better (says mtry = 27)
nb_ctrl <- trainControl(method="cv",
                        number=10,
                        repeats= 5,
                        #summaryFunction = multiClassSummary)
                        classProbs = TRUE,
                        verboseIter = TRUE)

#grid <- expand.grid(fL = c(1:7), usekernel = TRUE, adjust = c(1:7))
grid <- expand.grid(fL = c(1), usekernel = TRUE, adjust = c(1))


#nb_tuned <- train(mtrain, train.y, method = "rf", metric="Accuracy", na.action = na.omit, ntree = 600, tuneGrid = grid)
nb_tuned <- train(mtrain, train.y, method = "nb", metric="Accuracy", trControl = nb_ctrl, tuneGrid = grid)
nb_pred <- predict(nb_tuned, mtest, type="prob")


nb_conf <- confusionMatrix(nb_pred, test.y)
nb_mauc <- pROC::multiclass.roc(nb_pred, as.numeric(test.y))
nb_auc  <- pROC::auc(nb_pred, as.numeric(test.y))
nb_sum <- caret::multiClassSummary(data.frame(obs = test.y, pred=nb_pred), lev = as.numeric(test.y), nb_tuned)

# save data
save(nb_tuned, file = "./models/nb_tuned.Rdata")
save(nb_pred_probs, file = "./models/nb_pred_probs.Rdata")
save(nb_pred, file = "./models/nb_pred.Rdata")
save(nb_conf, file = "./models/nb_conf.Rdata")
save(nb_mauc, file = "./models/nb_mauc.Rdata")
save(nb_auc, file = "./models/nb_auc.Rdata")
save(nb_sum, file = "./models/nb_sum.Rdata")

stopCluster(cluster)
