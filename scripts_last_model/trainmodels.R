
trainModels <- function(){

  trainData_t_all <- user_next_all[!(is.na(user_next_all$paidnext)),]
  trainData_all <- trainData_t_all[,c(-1,-2)]
  
  set.seed(4444)#sample(1:5000, 1)
  trainIndex <- createDataPartition(trainData_all$paidnext, p = .7, 
                                    list = FALSE, 
                                    times = 1)
  userTrain_all <- trainData_all[ trainIndex,]
  userTest_all  <- trainData_all[-trainIndex,]
  
  preproc_all <<- preProcess(userTrain_all, na.rm=TRUE, method = c("zv", "nzv"),#"corr","center","scale","pca","nzv"  
                         verbose = TRUE)
  
  userTrain_all_p <- na.exclude(predict(preproc_all, userTrain_all))
  userTest_all_p <- na.exclude(predict(preproc_all, userTest_all))
  
  print("userTrain_all_p: ")
  print(dim(userTrain_all_p))
  print(anyNA(userTrain_all_p))
  percentage <- prop.table(table(userTrain_all_p$paidnext)) * 100
  print(cbind(freq=table(userTrain_all_p$paidnext), percentage=percentage))

  
  control <- trainControl(method="cv", number=10, verboseIter = TRUE 
                          ,classProbs = TRUE, summaryFunction = twoClassSummary)
  control_b <- trainControl(method="cv", number=10, verboseIter = TRUE)
  metric <- "ROC"
  metric_b <- "Kappa"
  
  
  #cforest(paidnext ~ ., data=userTrain_all_p, controls=cforest_control(mtry=2, mincriterion=0))
  #cforest(paidnext ~ ., data=userTrain_all_p, controls=control)
  
  print("training model: treebag_all")
  m_treebag_all <<- train(paidnext~., data=userTrain_all_p, method="treebag",
                      metric=metric, trControl=control)
  print("training model: rpart_all")
  m_rpart_all <<- train(paidnext~., data=userTrain_all_p, method="rpart",
                    metric=metric, trControl=control)
  print("training model: rf_all")
  m_rf_all <<- train(paidnext~., data=userTrain_all_p, method="rf", 
                 metric=metric, trControl=control)
  print("training model: ranger_all")
  m_ranger_all <<- train(paidnext~., data=userTrain_all_p, method="ranger", 
                     metric=metric, trControl=control)
 
  
  model_performances <<- data.frame(model = NA, Accuracy = NA, Kappa = NA, TP = NA, FP = NA, FN = NA, TN = NA)
  
  print("evaluating: treebag_all")
  predicted <- predict(m_treebag_all, userTrain_all_p)
  mtx <- confusionMatrix(predicted, userTrain_all_p$paidnext)
  print("on training data: ")
  print(mtx)
  model_performances <<- rbind(model_performances, c(model="treebag_all_ontraining", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                                     TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))
  predicted <- predict(m_treebag_all, userTest_all_p)
  mtx <- confusionMatrix(predicted, userTest_all_p$paidnext)
  print("on test data: ")
  print(mtx)
  model_performances <<- rbind(model_performances, c(model="treebag_all_ontest", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                    TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))
  
  print("evaluating: rpart_all")
  predicted <- predict(m_rpart_all, userTrain_all_p)
  mtx <- confusionMatrix(predicted, userTrain_all_p$paidnext)
  print("on training data: ")
  print(mtx)
  model_performances <<- rbind(model_performances, c(model="rpart_all_ontraining", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                                     TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))
  predicted <- predict(m_rpart_all, userTest_all_p)
  mtx <- confusionMatrix(predicted, userTest_all_p$paidnext)
  print("on test data: ")
  print(mtx)
  model_performances <<- rbind(model_performances, c(model="rpart_all_ontest", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                    TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))
  
  print("evaluating: rf_all")
  predicted <- predict(m_rf_all, userTrain_all_p)
  mtx <- confusionMatrix(predicted, userTrain_all_p$paidnext)
  print("on training data: ")
  print(mtx)
  model_performances <<- rbind(model_performances, c(model="rf_all_ontraining", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                                     TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))
  predicted <- predict(m_rf_all, userTest_all_p)
  mtx <- confusionMatrix(predicted, userTest_all_p$paidnext)
  print("on test data: ")
  print(mtx)
  model_performances <<- rbind(model_performances, c(model="rf_all_ontest", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                    TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))
  
  print("evaluating: ranger_all")
  predicted <- predict(m_ranger_all, userTrain_all_p)
  mtx <- confusionMatrix(predicted, userTrain_all_p$paidnext)
  print("on training data: ")
  print(mtx)
  model_performances <<- rbind(model_performances, c(model="ranger_all_ontraining", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                                     TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))
  predicted <- predict(m_ranger_all, userTest_all_p)
  mtx <- confusionMatrix(predicted, userTest_all_p$paidnext)
  print("on test data: ")
  print(mtx)
  model_performances <<- rbind(model_performances, c(model="ranger_all_ontest", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                    TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))
  

  model_performances <<- model_performances[complete.cases(model_performances), ]
  #model_performances <<- model_performances[order(desc(model_performances[,"Kappa"]),desc(model_performances[,"Accuracy"])),]
  model_performances$Kappa <<- round(as.numeric(model_performances$Kappa),3)
  model_performances$Accuracy <<- round(as.numeric(model_performances$Accuracy),3)

}