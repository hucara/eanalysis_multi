## Not run:
# Load the normal svm function
library(e1071)
require(caret)
set.seed(1234)

svm_tuned <- tune(svm, train.x=mtrain, train.y=train.y, 
                 kernel="linear", ranges = list(gamma = c(0.1,0.15,0.2,0.25), cost = c(0.5,1,1.5,2)), 
                 scale=FALSE, probability = TRUE)

svm_pred <- predict(svm_tuned$best.model, mtest)
svm_pred_probs <- predict(svm_tuned$best.model, mtest, probability = TRUE)
svm_pred_probs <- attr(svm_pred_probs, "probabilities")

svm_conf <- confusionMatrix(svm_pred, test.y)
svm_mauc <- pROC::multiclass.roc(svm_pred, as.numeric(test.y))
svm_auc  <- pROC::auc(svm_pred, as.numeric(test.y))
svm_sum <- caret::multiClassSummary(data.frame(obs = test.y, pred=svm_pred), lev = as.numeric(test.y), svm_pred)

# save data
save(svm_tuned, file = "./models/svm_tuned.Rdata")
save(svm_pred_probs, file = "./models/svm_pred_probs.Rdata")
save(svm_pred, file = "./models/svm_pred.Rdata")
save(svm_conf, file = "./models/svm_conf.Rdata")
save(svm_mauc, file = "./models/svm_mauc.Rdata")
save(svm_auc, file = "./models/svm_auc.Rdata")
save(svm_sum, file = "./models/svm_sum.Rdata")

gc()