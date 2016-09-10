require(caret)
require(doParallel)

# load data
# source("loadUsedTweets.R")

set.seed(1234)
cluster <- makeCluster(6)
registerDoParallel(cluster)

xgb_ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, allowParallel = T)

# xgb_grid <- expand.grid(nrounds = 1000, max_depth = 6, eta = 0.4, gamma = 0, colsample_bytree = 4, min_child_weight = 1)
# xgb_tuned <- train(mtrain, train.y, method = "xgbTree", trControl = xgb_ctrl, verbose = T, metric = "Accuracy", tuneGrid = xgb_grid)
# xgb_tuned

xgb_grid <- expand.grid(nrounds = 1000, max_depth = 8, eta = 0.4, gamma = 0, colsample_bytree = 4, min_child_weight = 1)
xgb_tuned <- train(mtrain, train.y, method = "xgbTree", trControl = xgb_ctrl, verbose = T, metric = "Accuracy", tuneGrid = xgb_grid)
xgb_tuned

xgb_pred <- predict(xgb_tuned, mtest)
xgb_pred_probs <- predict(xgb_tuned, mtest, type = "prob")

xgb_conf <- confusionMatrix(xgb_pred, test.y)
xgb_mauc <- pROC::multiclass.roc(xgb_pred, as.numeric(test.y))
xgb_auc  <- pROC::auc(xgb_pred, as.numeric(test.y))
xgb_sum <- caret::multiClassSummary(data.frame(obs = test.y, pred=xgb_pred), lev = as.numeric(test.y), xgb_tuned)

# save data
save(xgb_tuned, file = "./models/xgb_tuned.Rdata")
save(xgb_pred_probs, file = "./models/xgb_pred_probs.Rdata")
save(xgb_pred, file = "./models/xgb_pred.Rdata")
save(xgb_conf, file = "./models/xgb_conf.Rdata")
save(xgb_mauc, file = "./models/xgb_mauc.Rdata")
save(xgb_auc, file = "./models/xgb_auc.Rdata")
save(xgb_sum, file = "./models/xgb_sum.Rdata")

stopCluster(cluster)
