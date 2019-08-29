
preprocAndTrain <- function(){
  
  trainData_t <- user_next_firstm3[!(is.na(user_next_firstm3$paidnext)),]
  trainData <- trainData_t[-c(1:21),]
  
  set.seed(1152)
  trainIndex <- createDataPartition(trainData$paidnext, p = .8, 
                                    list = FALSE, 
                                    times = 1)
  userTrain <- trainData[ trainIndex,-1]
  userTest  <- trainData[-trainIndex,-1]
  
  preproc1 <- preProcess(userTrain, method = c("zv", "center", "scale"),
                         verbose = TRUE)
  
  userTrain_p <- predict(preproc1, userTrain)
  userTest_p <- predict(preproc1, userTest)
  
  print("userTrain_p: ")
  print(dim(userTrain_p))
  print(str(userTrain_p))
  print(anyNA(userTrain_p))
  print(summary(userTrain_p))
  print(sapply(userTrain_p, class))
  print(levels(userTrain_p$paidnext))
  print(levels(userTrain_p$paymenthandler))
  
  percentage <- prop.table(table(userTrain_p$paidnext)) * 100
  print(cbind(freq=table(userTrain_p$paidnext), percentage=percentage))
 
  control <- trainControl(method="cv", number=10, verboseIter = TRUE)
  metric <- "Kappa"
  
  print("training model: rpart")
  m_rpart <<- train(paidnext~., data=userTrain_p, method="rpart",
                      metric=metric, trControl=control)
  
  print("training model: svmRadial")
  m_svm <<- train(paidnext~., data=userTrain_p, method="svmRadial", 
                    metric=metric, trControl=control)
  print("training model: rf")
  m_rf <<- train(paidnext~., data=userTrain_p, method="rf", 
                   metric=metric, trControl=control)
  print("training model: treebag")
  m_treebag <<- train(paidnext~., data=userTrain_p, method="treebag",
                        metric=metric, trControl=control)
  
  print("training model: adaboost")
  model_adaboost2 <<-  train(paidnext~., 
                             data = userTrain_p, 
                             method = "adaboost", 
                             trControl = trainControl(verboseIter = TRUE),
                             verbose = TRUE)
  print("training model: deepboost")
  model_deepboost2 <<-  train(paidnext~., 
                              data = userTrain_p, 
                              method = "deepboost", 
                              trControl = trainControl(verboseIter = TRUE),
                              verbose = TRUE)
  
  print("train rpart")
  predicted <- predict(m_rpart, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  print("train svmRadial")
  predicted <- predict(m_svm, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  print("train rf")
  predicted <- predict(m_rf, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  print("train treebag")
  predicted <- predict(m_treebag, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  print("training model: adaboost")
  predicted <- predict(model_adaboost2, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  print("training model: deepboost")
  predicted <- predict(model_deepboost2, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  
  


'/

  
  print("training model: svmLinear")
  predicted <- predict(m_svmlinear_k, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  print("training model: svmLinearWeights")
  predicted <- predict(m_svmlinearweights_k, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  print("training model: svmRadialWeights")
  predicted <- predict(m_svmlinearweights_k, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  print("training model: ORFsvm")
  predicted <- predict(m_orfsvm_k, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  
  browser()
  
  
  m_svmlinear_k <<-  train(paidnext~., 
                           data = userTrain_p, 
                           method = "svmLinear",
                           metric = metric,
                           trControl = control)
  
  

  m_svmlinearweights_k <<-  train(paidnext~., 
                                  data = userTrain_p, 
                                  method = "svmLinearWeights",
                                  metric = metric,
                                  trControl = control)

  

  m_svmradialWeights_k <<-  train(paidnext~., 
                                  data = userTrain_p, 
                                  method = "svmRadialWeights",
                                  metric = metric,
                                  trControl = control)

  

  m_orfsvm_k <<-  train(paidnext~., 
                        data = userTrain_p, 
                        method = "ORFsvm",
                        metric = metric,
                        trControl = control)

  
  
  m_adaboost_k <<- train(paidnext~., 
                      data = userTrain_p, 
                      method = "adaboost", 
                      metric=metric,
                      trControl = control)

  

  m_deepboost_k <<-  train(paidnext~., 
                              data = userTrain_p, 
                              method = "deepboost",
                              metric = metric,
                              trControl = control)

  
  
  
  #browser()
  /'
  #TRAIN
  
  '/
  print("training model: rotationForest")
  model_rotationforest2 <<-  train(paidnext~., 
                                   data = userTrain_p, 
                                   method = "rotationForest", 
                                   trControl = trainControl(verboseIter = TRUE),
                                   verbose = TRUE)
  

  
  print("training model: C5.0Cost")
  model_c50cost2 <<-  train(paidnext~., 
                            data = userTrain_p, 
                            method = "C5.0Cost", 
                            trControl = trainControl(verboseIter = TRUE),
                            verbose = TRUE)
  
  print("training model: bartMachine")
  model_bartmachine2 <<-  train(paidnext~., 
                                data = userTrain_p, 
                                method = "bartMachine", 
                                trControl = trainControl(verboseIter = TRUE),
                                verbose = TRUE)
  
  print("training model: ada")
  model_ada2 <<-  train(paidnext~., 
                        data = userTrain_p, 
                        method = "ada", 
                        trControl = trainControl(verboseIter = TRUE),
                        verbose = TRUE)
  
  print("training model: rpartCost")
  model_rpartcost2 <<-  train(paidnext~., 
                              data = userTrain_p, 
                              method = "rpartCost", 
                              trControl = trainControl(verboseIter = TRUE),
                              verbose = TRUE)
  
  

  
  
  print("training model: rotationForestCp")
  model_rotationforestcp2 <<-  train(paidnext~., 
                                     data = userTrain_p, 
                                     method = "rotationForestCp", 
                                     trControl = trainControl(verboseIter = TRUE),
                                     verbose = TRUE)
  
  print("training model: nodeHarvest")
  model_nodeharvest2 <<-  train(paidnext~., 
                                data = userTrain_p, 
                                method = "nodeHarvest", 
                                trControl = trainControl(verboseIter = TRUE),
                                verbose = TRUE)
  /'
}
