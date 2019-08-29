
trainModels3 <- function(){
  
  trainData_t_all <- user_next_all[!(is.na(user_next_all$paidnext)),1:300]
  trainData_all <- trainData_t_all[,c(-1,-2)]
  
  set.seed(4444)#sample(1:5000, 1)
  trainIndex <- createDataPartition(trainData_all$paidnext, p = .7, 
                                    list = FALSE, 
                                    times = 1)
  userTrain_all <- trainData_all[ trainIndex,]
  userTest_all  <- trainData_all[-trainIndex,]
  
  preproc_all <<- preProcess(userTrain_all, na.rm=TRUE, method = c("center", "scale"),#"corr","center","scale","pca","nzv"  
                             verbose = TRUE)
  
  userTrain_all_p <- na.exclude(predict(preproc_all, userTrain_all))
  userTest_all_p <- na.exclude(predict(preproc_all, userTest_all))
  
  print("userTrain_all_p: ")
  print(dim(userTrain_all_p))
  print(anyNA(userTrain_all_p))
  percentage <- prop.table(table(userTrain_all_p$paidnext)) * 100
  print(cbind(freq=table(userTrain_all_p$paidnext), percentage=percentage))
  
  userTrain_all_p$paidnext <- as.factor(userTrain_all_p$paidnext)
  userTest_all_p$paidnext <- as.factor(userTest_all_p$paidnext)
  
  m_nn <<- train(paidnext ~ ., data = userTrain_all_p, method = "nnet", maxit = 1000)
  
  print("evaluating: neuralnets")
  predicted <- predict(m_nn, userTrain_all_p)
  mtx <- confusionMatrix(predicted, userTrain_all_p$paidnext)
  print("on training data: ")
  print(mtx)
  predicted <- predict(m_nn, userTest_all_p)
  mtx <- confusionMatrix(predicted, userTest_all_p$paidnext)
  print("on test data: ")
  print(mtx)
  
}