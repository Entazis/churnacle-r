
churnacle <- function(){
  
  data_path <<- "./"
  #data_path <<- "/usr/src/churnVolume/"
  
  require(caret)
  require(ranger)
  require(dplyr)
  require(parsedate)
  require(tidyr)
  require(stringr)
  require(jsonlite)
  require(rpart.plot)
  require(slackr)
  
  source('churnacle.R')
  source('readfiles.R')
  source('preparedata.R')
  source('processcentral.R')
  source('processorange.R')
  source('processorange2.R')
  source('checkchurn.R')
  source('calculateestimated.R')
  source('trainmodels.R')
  source('trainmodels2.R')
  source('predictchurn.R')
  source('predictchurn2.R')
  
  start_time <<- Sys.time()
  
  #read data
  read_start <<- Sys.time()
  readFiles()
  read_end <<- Sys.time()
  print("reading time was: ")
  print(read_end-read_start)
  
  #prepare data for train
  prepare_start <<- Sys.time()
  prepareData()
  prepare_end <<- Sys.time()
  print("preparing time was: ")
  print(prepare_end-prepare_start)
  
  #jsonlite::write_json(jsonlite::toJSON(user_next_all), paste(data_path, "user_next_all.json", sep = ""))
  #write.table(user_next_all, file=paste(data_path,"user_next_all.tsv",sep = ""), quote=FALSE, sep="\t", col.names = NA)
  #write.table(user_next_all, file=paste(data_path,"user_next_all.csv",sep = ""), quote=FALSE, sep=",", col.names = TRUE)
  
  #check churned users
  check_start <<- Sys.time()
  if(file.exists("emailstest.csv") == TRUE){
    checkChurn()
  }
  check_end <<- Sys.time()
  print("checking time was: ")
  print(check_end-check_start)
  
  #calculate estimated times for each assignments
  calcest_start <<- Sys.time()
  calculateEstimated()
  calcest_end <<- Sys.time()
  print("estimated time calculation time was: ")
  print(calcest_end-calcest_start)
  
  #preprocess & train & evaluate
  train_start <<- Sys.time()
  trainModels()
  train_end <<- Sys.time()
  print("training time was: ")
  print(train_end-train_start)
  train2_start <<- Sys.time()
  trainModels2()
  train2_end <<- Sys.time()
  print("training time 2 was: ")
  print(train2_end-train2_start)
  
  #predictchurn
  predict_start <<- Sys.time()
  predictChurn()
  predict_end <<- Sys.time()
  print("prediction time was: ")
  print(predict_end-predict_start)
  predict2_start <<- Sys.time()
  predictChurn2()
  predict2_end <<- Sys.time()
  print("prediction time 2 was: ")
  print(predict2_end-predict2_start)
  
  end_time <<- Sys.time()
  
  print("program execution time was: ")
  print(end_time-start_time)
  print("reading time was: ")
  print(read_end-read_start)
  print("preparing time was: ")
  print(prepare_end-prepare_start)
  print("checking time was: ")
  print(check_end-check_start)
  print("training time was: ")
  print(train_end-train_start)
  print("training time 2 was: ")
  print(train2_end-train2_start)
  print("prediction time was: ")
  print(predict_end-predict_start)
  print("prediction time 2 was: ")
  print(predict2_end-predict2_start)
  
  prog_time <<- end_time-start_time
  read_time <<- read_end-read_start
  prep_time <<- prepare_end-prepare_start
  
  check_time <<- check_end-check_start
  calcest_time <<- calcest_end-calcest_start
  
  train_time <<- train_end-train_start
  train2_time <<- train2_end-train2_start
  pred_time <<- predict_end-predict_start
  pred2_time <<- predict2_end-predict2_start
  
  #FIXME: 2 kinds of prediction
  slackr_setup()
  churnList <<- churn_list_unique_all[churn_list_unique_all$expires <= 10 & churn_list_unique_all$kappa >= 0.2,]
  churnList2 <<- churn_list_unique_all2[churn_list_unique_all2$expires <= 10 & churn_list_unique_all2$kappa >= 0.2,]
  
  groups <- ifelse(sample(1:dim(churnList)[1])%%2 == 1, "A", "B")
  a_b_test_new <<- cbind(churnList, test=groups)
  a_b_test_new <<- cbind(a_b_test_new, saved=NA)
  a_b_test_new <<- a_b_test_new[,c("email", "endDate", "test", "saved")]
  churnList <<- churnList[order(churnList[,"expires"],
                                desc(churnList[,"kappa"]), 
                                desc(churnList[,"accuracy"])),]
  churnList$endDate <<- parse_date(churnList$endDate)
  churn_emails <<- churnList$email
  write.table(churn_emails, file=paste(data_path,"emails.csv",sep = ""),quote=FALSE,row.names = FALSE,col.names = FALSE,sep = ",")
  write.table(a_b_test_new,file=paste(data_path,"emailstest.csv", sep = ""),quote=FALSE,row.names = FALSE,sep = ",")

  groups <- ifelse(sample(1:dim(churnList2)[1])%%2 == 1, "A", "B")
  a_b_test_new2 <<- cbind(churnList2, test=groups)
  a_b_test_new2 <<- cbind(a_b_test_new2, saved=NA)
  a_b_test_new2 <<- a_b_test_new2[,c("email", "endDate", "test", "saved")]
  churnList2 <<- churnList2[order(churnList2[,"expires"],
                                desc(churnList2[,"kappa"]), 
                                desc(churnList2[,"accuracy"])),]
  churnList2$endDate <<- parse_date(churnList2$endDate)
  churn_emails2 <<- churnList2$email
  write.table(churn_emails2, file=paste(data_path,"emails2.csv",sep = ""),quote=FALSE,row.names = FALSE,col.names = FALSE,sep = ",")
  write.table(a_b_test_new2,file=paste(data_path,"emailstest2.csv", sep = ""),quote=FALSE,row.names = FALSE,sep = ",")
  
  
  slackr_bot("Churn prediction model: ", Sys.time(),
             prog_time,
             model_performances,
             model_performances2,
             read_time,
             prep_time,
             train_time,
             train2_time,
             pred_time,
             pred2_time,
             username="DataScientist-bot", channel="#team-rsc",
             incoming_webhook_url="https://hooks.slack.com/services/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")
  
  slackr_bot("Churn prediction: ",
             churnList,
             churnList2,
             username="DataScientist-bot", channel="#team-rsc",
             incoming_webhook_url="https://hooks.slack.com/services/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")
  
  if(exists("churn_check") && is.data.frame(get("churn_check"))){
    slackr_bot("Results from the last prediction (with A/B testing): ", 
               churn_check, all_ab, still_live_ab,
               saved_a, lost_a, saved_b, lost_b,
               username="DataScientist-bot", channel="#team-rsc",
               incoming_webhook_url="https://hooks.slack.com/services/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")
    
  }

  TRUE  
}