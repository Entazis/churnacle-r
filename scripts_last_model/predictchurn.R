
predictChurn <- function(){
  
  user_expire_all <- user_next_all[,c("email", "endDate")]
  user_expire_all$endDate <- parse_date(user_expire_all$endDate)
  
  user_churn_full_all <- user_next_all[is.na(user_next_all$paidnext),]
  user_churn_full_all$endDate <- parse_date(user_churn_full_all$endDate)
  user_churn_all <- user_churn_full_all[,c(-1,-2)]
  
  churn_list_all <- data.frame(email = NA, endDate = NA, expires = NA, model = NA, accuracy = NA, kappa = NA)
  
  user_churn_all_p <- predict(preproc_all, user_churn_all)
  
  
  print("predicting: treebag_all")
  predicted <- predict(m_treebag_all, user_churn_all_p)
  user_churn_full_all$paidnext = predicted
  willchurn <- user_churn_full_all[user_churn_full_all$paidnext == "no", 1:2]
  if(length(willchurn[,2] > 0)){
    willchurn$expires <- difftime(parse_date(willchurn[,2]),parse_date(Sys.time()), units = "days")
    print(willchurn)
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="treebag_all", 
                                   accuracy=model_performances[model_performances$model == "treebag_all_ontraining","Accuracy"],
                                   kappa=model_performances[model_performances$model == "treebag_all_ontraining","Kappa"]))
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="treebag_all", 
                                   accuracy=model_performances[model_performances$model == "treebag_all_ontest","Accuracy"],
                                   kappa=model_performances[model_performances$model == "treebag_all_ontest","Kappa"]))
  }
  print("predicting: rpart_all")
  predicted <- predict(m_rpart_all, user_churn_all_p)
  user_churn_full_all$paidnext = predicted
  willchurn <- user_churn_full_all[user_churn_full_all$paidnext == "no", 1:2]
  if(length(willchurn[,2] > 0)){
    willchurn$expires <- difftime(parse_date(willchurn[,2]),parse_date(Sys.time()), units = "days")
    print(willchurn)
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="treebag_all", 
                                   accuracy=model_performances[model_performances$model == "rpart_all_ontraining","Accuracy"],
                                   kappa=model_performances[model_performances$model == "rpart_all_ontraining","Kappa"]))
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="treebag_all", 
                                   accuracy=model_performances[model_performances$model == "rpart_all_ontest","Accuracy"],
                                   kappa=model_performances[model_performances$model == "rpart_all_ontest","Kappa"]))
  }
  print("predicting: rf_all")
  predicted <- predict(m_rf_all, user_churn_all_p)
  user_churn_full_all$paidnext = predicted
  willchurn <- user_churn_full_all[user_churn_full_all$paidnext == "no", 1:2]
  if(length(willchurn[,2] > 0)){
    willchurn$expires <- difftime(parse_date(willchurn[,2]),parse_date(Sys.time()), units = "days")
    print(willchurn)
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="treebag_all", 
                                   accuracy=model_performances[model_performances$model == "rf_all_ontraining","Accuracy"],
                                   kappa=model_performances[model_performances$model == "rf_all_ontraining","Kappa"]))
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="treebag_all", 
                                   accuracy=model_performances[model_performances$model == "rf_all_ontest","Accuracy"],
                                   kappa=model_performances[model_performances$model == "rf_all_ontest","Kappa"]))
  }
  print("predicting: ranger_all")
  predicted <- predict(m_ranger_all, user_churn_all_p)
  user_churn_full_all$paidnext = predicted
  willchurn <- user_churn_full_all[user_churn_full_all$paidnext == "no", 1:2]
  if(length(willchurn[,2] > 0)){
    willchurn$expires <- difftime(parse_date(willchurn[,2]),parse_date(Sys.time()), units = "days")
    print(willchurn)
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="treebag_all", 
                                   accuracy=model_performances[model_performances$model == "ranger_all_ontraining","Accuracy"],
                                   kappa=model_performances[model_performances$model == "ranger_all_ontraining","Kappa"]))
    churn_list_all <- rbind(churn_list_all, 
                            mutate(willchurn, model="treebag_all", 
                                   accuracy=model_performances[model_performances$model == "ranger_all_ontest","Accuracy"],
                                   kappa=model_performances[model_performances$model == "ranger_all_ontest","Kappa"]))
  }

  
  churn_list_all <- churn_list_all[complete.cases(churn_list_all), ]
  churn_list_unique_all <<- churn_list_all[!duplicated(churn_list_all$email),]
  
  print("the unique churn list (for all, ordered by kappa): ")
  print(churn_list_unique_all[order(desc(churn_list_unique_all[,"kappa"]), 
                                    desc(churn_list_unique_all[,"accuracy"]),
                                    churn_list_unique_all[,"expires"]),])
  
  print("the unique churn list (for all, ordered by time left): ")
  print(churn_list_unique_all[order(churn_list_unique_all[,"expires"],
                                    desc(churn_list_unique_all[,"kappa"]), 
                                    desc(churn_list_unique_all[,"accuracy"])),])
  
  print("the unique churn list (time left <= 8 days, model prec > 20%): ")
  
  print(churn_list_unique_all[churn_list_unique_all$expires <= 8 & churn_list_unique_all$kappa >= 0.2,])
  
  #rpart.plot(m_rpart_all$finalModel)
  

}