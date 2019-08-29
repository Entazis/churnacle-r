
preprocessTrainingdata2 <- function(){
  
  #TODO filter out NZV predictors from the 3150 vars
  #TODO filter out correlating predictors
  #user_next_slice <- user_next_sessions1[,1:450]
  user_next_slice <- user_next_sessions2[,1:450]
  #user_next_slice <- user_next_sessions3[,1:450]
  #user_next_slice <- user_next_sessions4[,1:450]
  #user_next_slice <- user_next_sessions5[,1:450]
  #user_next_slice <- user_next_sessions6[,1:450]
  user_next_slice$email <- as.character(user_next_slice$email)
  print(dim(user_next_slice))
  
  #TODO: filter out paidnext != NA
  #TODO: filter out users with > 15 submcnt
  #TODO: filter out users with > 14 sessions != NA
  user_next_slice_filtered1 <- user_next_slice[!(is.na(user_next_slice$paidnext)),]
  user_next_slice_filtered2 <- user_next_slice_filtered1[user_next_slice_filtered1$submissionCnt>15,]
  user_next_slice_filtered3 <- 
    user_next_slice_filtered2[
    as.logical(apply(user_next_slice_filtered2, 1, function(user){
      if (sum(!(is.na(user[12:30]))) > 14) TRUE
      else FALSE
    }))
    ,]
  
  #browser()
  
  set.seed(3456)
  trainIndex <- createDataPartition(as.character(user_next_slice_filtered3$paidnext), p = .8, 
                                    list = FALSE, 
                                    times = 1)
  userTrain <- user_next_slice_filtered3[ trainIndex,]
  userTest  <- user_next_slice_filtered3[-trainIndex,]
  
  #TODO: centering, scaling, imputation
  preproc1 <- preProcess(userTrain, method = c("center", "scale"),
                         verbose = TRUE)
  preproc2 <- preProcess(userTrain, method = c("zv","center","scale","medianImpute"),
                         verbose = TRUE, na.remove = TRUE)
  
  userTrain_p <- predict(preproc2, userTrain)
  userTest_p <- predict(preproc2, userTest)
  
  #browser()
  
  print("training model: rotationForest")
  model_rotationforest2 <<-  train(paidnext~., 
                                  data = userTrain_p[,-1], 
                                  method = 'rotationForest', 
                                  trControl = trainControl(verboseIter = TRUE),
                                  verbose = TRUE)
  
  print("training model: adaboost")
  model_adaboost2 <<-  train(paidnext~., 
                           data = userTrain_p[,-1], 
                           method = 'adaboost', 
                           trControl = trainControl(verboseIter = TRUE),
                           verbose = TRUE)
  
  print("training model: C5.0Cost")
  model_c50cost2 <<-  train(paidnext~., 
                            data = userTrain_p[,-1], 
                            method = 'C5.0Cost', 
                            trControl = trainControl(verboseIter = TRUE),
                            verbose = TRUE)
  
  print("training model: bartMachine")
  model_bartmachine2 <<-  train(paidnext~., 
                            data = userTrain_p[,-1], 
                            method = 'bartMachine', 
                            trControl = trainControl(verboseIter = TRUE),
                            verbose = TRUE)
  
  print("training model: ada")
  model_ada2 <<-  train(paidnext~., 
                               data = userTrain_p[,-1], 
                               method = 'ada', 
                               trControl = trainControl(verboseIter = TRUE),
                               verbose = TRUE)
  
  '/
  #there is no package called ‘CHAID’
  print("training model: chaid")
  model_chaid <<-  train(paidnext~., 
                            data = userTrain_p[,-1], 
                            method = chaid, 
                            trControl = trainControl(verboseIter = TRUE),
                            verbose = TRUE)
  
  /'
  
  print("training model: rpartCost")
  model_rpartcost2 <<-  train(paidnext~., 
                            data = userTrain_p[,-1], 
                            method = 'rpartCost', 
                            trControl = trainControl(verboseIter = TRUE),
                            verbose = TRUE)
  
  
  print("training model: deepboost")
  model_deepboost2 <<-  train(paidnext~., 
                            data = userTrain_p[,-1], 
                            method = 'deepboost', 
                            trControl = trainControl(verboseIter = TRUE),
                            verbose = TRUE)
  
  
  print("training model: rotationForestCp")
  model_rotationforestcp2 <<-  train(paidnext~., 
                            data = userTrain_p[,-1], 
                            method = 'rotationForestCp', 
                            trControl = trainControl(verboseIter = TRUE),
                            verbose = TRUE)
  
  print("training model: nodeHarvest")
  model_nodeharvest2 <<-  train(paidnext~., 
                                    data = userTrain_p[,-1], 
                                    method = 'nodeHarvest', 
                                    trControl = trainControl(verboseIter = TRUE),
                                    verbose = TRUE)
  
  
}