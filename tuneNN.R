require(caret)
require(doParallel)

# load data
# source("loadUsedTweets.R")

set.seed(1234)
cluster <- makeCluster(6)
registerDoParallel(cluster)

nn_ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, allowParallel = T)
nn_grid <- expand.grid(size = c(1, 5, 10), decay = c(0, 0.05, 0.1))


nn_tuned <- train(mtrain, train.y, method = "nnet", trControl = nn_ctrl, verbose = T, metric = "Accuracy",  MaxNWts = 5000)
nn_tuned <- train(mtrain, train.y, method = "nnet", trControl = nn_ctrl, verbose = T, metric = "Accuracy", tuneGrid = nn_grid)
nn_tuned

nn_pred <- predict(nn_tuned, mtest)

nn_conf <- confusionMatrix(nn_pred, test.y)
nn_mauc <- pROC::multiclass.roc(nn_pred, as.numeric(test.y))
nn_auc  <- pROC::auc(nn_pred, as.numeric(test.y))
nn_sum <- caret::multiClassSummary(data.frame(obs = test.y, pred=nn_pred), lev = as.numeric(test.y), nn_tuned)

# save data
save(nn_tuned, file = "./models/nn_tuned.Rdata")
save(nn_pred, file = "./models/nn_pred.Rdata")
save(nn_conf, file = "./models/nn_conf.Rdata")
save(nn_mauc, file = "./models/nn_mauc.Rdata")
save(nn_auc, file = "./models/nn_auc.Rdata")
save(nn_sum, file = "./models/nn_sum.Rdata")

stopCluster(cluster)