require(caret)
require(doParallel)

set.seed(1234)
cluster <- makeCluster(2)
registerDoParallel(cluster)

bestmtry <- tuneRF(mtrain, train.y, stepFactor=1.5, improve=1e-5, ntreeTry=600, do.trace = TRUE, plot = TRUE)

# Tune with caret and decide which results are better (says mtry = 27)
rf_ctrl <- trainControl(method="cv",
                        number=10,
                        classProbs = TRUE,
                        verboseIter = TRUE)
# best mtry 163
grid <- expand.grid(mtry = c(163))

message("Training...")
#rf_tuned <- train(mtrain, train.y, method = "rf", metric="Accuracy", na.action = na.omit, ntree = 600, tuneGrid = grid)
rf_tuned <- train(mtrain, train.y, method = "rf", metric="Accuracy", na.action = na.omit, ntree = 600, trControl = rf_ctrl, tuneGrid = grid)

message("Predicting...")
rf_pred <- predict(rf_tuned, mtest)
rf_pred_probs <- predict(rf_tuned, mtest, type="prob")

message("Obtaining results...")
rf_conf <- confusionMatrix(rf_pred, test.y)
rf_mauc <- pROC::multiclass.roc(rf_pred, as.numeric(test.y))
rf_auc  <- pROC::auc(rf_pred, as.numeric(test.y))
rf_sum <- caret::multiClassSummary(data.frame(obs = test.y, pred=rf_pred), lev = as.numeric(test.y), rf_tuned)

message("Saving results...")
# save data
save(rf_tuned, file = "./models/rf_tuned.Rdata")
save(rf_pred_probs, file = "./models/rf_pred_probs.Rdata")
save(rf_pred, file = "./models/rf_pred.Rdata")
save(rf_conf, file = "./models/rf_conf.Rdata")
save(rf_mauc, file = "./models/rf_mauc.Rdata")
save(rf_auc, file = "./models/rf_auc.Rdata")
save(rf_sum, file = "./models/rf_sum.Rdata")

gc()

stopCluster(cluster)
message("Done!")
