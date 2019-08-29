
evaluateModel <- function(){
  
  user_next_slice <- user_next_sessions1[,1:450]
  #user_next_slice <- user_next_sessions2[,1:450]
  #user_next_slice <- user_next_sessions3[,1:450]
  #user_next_slice <- user_next_sessions4[,1:450]
  #user_next_slice <- user_next_sessions5[,1:450]
  #user_next_slice <- user_next_sessions6[,1:450]
  user_next_slice$email <- as.character(user_next_slice$email)
  print(dim(user_next_slice))
  
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
  
  preproc1 <- preProcess(userTrain, method = c("center", "scale"),
                         verbose = TRUE)
  preproc2 <- preProcess(userTrain, method = c("zv","center","scale","medianImpute"),
                         verbose = TRUE, na.remove = TRUE)
  
  userTrain_p <- predict(preproc2, userTrain)
  userTest_p <- predict(preproc2, userTest)
  
  print("evaluating model_ada")
  predicted <- predict(model_ada, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  
  print("evaluating model_adaboost")
  predicted <- predict(model_adaboost, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  
  print("evaluating model_c50cost")
  predicted <- predict(model_c50cost, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  
  print("evaluating model_deepboost")
  predicted <- predict(model_deepboost, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  
  print("evaluating model_rotationforest")
  predicted <- predict(model_rotationforest, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  
  print("evaluating model_rotationforestcp")
  predicted <- predict(model_rotationforestcp, userTest_p)
  print(confusionMatrix(predicted, userTest_p$paidnext))
  
  
}