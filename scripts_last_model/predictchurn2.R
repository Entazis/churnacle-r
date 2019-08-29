
predictChurn2 <- function(){
  
  user_expire_all <- user_next_all2[,c("email", "endDate")]
  user_expire_all$endDate <- parse_date(user_expire_all$endDate)
  
  user_churn_full_all <- user_next_all2[is.na(user_next_all2$paidnext),]
  user_churn_full_all$endDate <- parse_date(user_churn_full_all$endDate)
  user_churn_all <- user_churn_full_all[,c(-1,-2)]
  
  churn_list_all <- data.frame(email = NA, endDate = NA, expires = NA, model = NA, accuracy = NA, kappa = NA)
  
  user_churn_all_p <- predict(preproc_all2, user_churn_all)
  
  
  print("predicting: treebag_all2")
  predicted <- predict(m_treebag_all2, user_churn_all_p)
  user_churn_full_all$paidnext = predicted
  willchurn <- user_churn_full_all[user_churn_full_all$paidnext == "no", 1:2]
  if(length(willchurn[,2] > 0)){
    willchurn$expires <- difftime(parse_date(willchurn[,2]),parse_date(Sys.time()), units = "days")
    print(willchurn)
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="treebag_all2", 
                                   accuracy=model_performances2[model_performances2$model == "treebag_all2_ontraining","Accuracy"],
                                   kappa=model_performances2[model_performances2$model == "treebag_all2_ontraining","Kappa"]))
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="treebag_all2", 
                                   accuracy=model_performances2[model_performances2$model == "treebag_all2_ontest","Accuracy"],
                                   kappa=model_performances2[model_performances2$model == "treebag_all2_ontest","Kappa"]))
  }
  print("predicting: rpart_all2")
  predicted <- predict(m_rpart_all2, user_churn_all_p)
  user_churn_full_all$paidnext = predicted
  willchurn <- user_churn_full_all[user_churn_full_all$paidnext == "no", 1:2]
  if(length(willchurn[,2] > 0)){
    willchurn$expires <- difftime(parse_date(willchurn[,2]),parse_date(Sys.time()), units = "days")
    print(willchurn)
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="rpart_all2", 
                                   accuracy=model_performances2[model_performances2$model == "rpart_all2_ontraining","Accuracy"],
                                   kappa=model_performances2[model_performances2$model == "rpart_all2_ontraining","Kappa"]))
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="rpart_all2", 
                                   accuracy=model_performances2[model_performances2$model == "rpart_all2_ontest","Accuracy"],
                                   kappa=model_performances2[model_performances2$model == "rpart_all2_ontest","Kappa"]))
  }
  print("predicting: rf_all2")
  predicted <- predict(m_rf_all2, user_churn_all_p)
  user_churn_full_all$paidnext = predicted
  willchurn <- user_churn_full_all[user_churn_full_all$paidnext == "no", 1:2]
  if(length(willchurn[,2] > 0)){
    willchurn$expires <- difftime(parse_date(willchurn[,2]),parse_date(Sys.time()), units = "days")
    print(willchurn)
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="rf_all2", 
                                   accuracy=model_performances2[model_performances2$model == "rf_all2_ontraining","Accuracy"],
                                   kappa=model_performances2[model_performances2$model == "rf_all2_ontraining","Kappa"]))
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="rf_all2", 
                                   accuracy=model_performances2[model_performances2$model == "rf_all2_ontest","Accuracy"],
                                   kappa=model_performances2[model_performances2$model == "rf_all2_ontest","Kappa"]))
  }
  print("predicting: ranger_all2")
  predicted <- predict(m_ranger_all2, user_churn_all_p)
  user_churn_full_all$paidnext = predicted
  willchurn <- user_churn_full_all[user_churn_full_all$paidnext == "no", 1:2]
  if(length(willchurn[,2] > 0)){
    willchurn$expires <- difftime(parse_date(willchurn[,2]),parse_date(Sys.time()), units = "days")
    print(willchurn)
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="ranger_all2", 
                                   accuracy=model_performances2[model_performances2$model == "ranger_all2_ontraining","Accuracy"],
                                   kappa=model_performances2[model_performances2$model == "ranger_all2_ontraining","Kappa"]))
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="ranger_all2", 
                                   accuracy=model_performances2[model_performances2$model == "ranger_all2_ontest","Accuracy"],
                                   kappa=model_performances2[model_performances2$model == "ranger_all2_ontest","Kappa"]))
  }
  
  
  churn_list_all <- churn_list_all[complete.cases(churn_list_all), ]
  churn_list_unique_all2 <<- churn_list_all[!duplicated(churn_list_all$email),]
  
  print("the unique churn list (for all, ordered by kappa): ")
  print(churn_list_unique_all2[order(desc(churn_list_unique_all2[,"kappa"]), 
                                    desc(churn_list_unique_all2[,"accuracy"]),
                                    churn_list_unique_all2[,"expires"]),])
  
  print("the unique churn list (for all, ordered by time left): ")
  print(churn_list_unique_all2[order(churn_list_unique_all2[,"expires"],
                                    desc(churn_list_unique_all2[,"kappa"]), 
                                    desc(churn_list_unique_all2[,"accuracy"])),])
  
  print("the unique churn list (time left <= 8 days, model prec > 20%): ")
  
  print(churn_list_unique_all2[churn_list_unique_all2$expires <= 8 & churn_list_unique_all2$kappa >= 0.2,])
  
  #rpart.plot(m_rpart_all$finalModel)
  
  
}