
prepareTraningData <- function(){
  
  #RWNM <<- 1
  #user_next <<- do.call(rbind, apply(central_users_flatten, 1, aggregateCentral2b))
  #user_next <<- user_next[!duplicated(user_next[c("email", "subscription", "transaction")]),]
  
  keep <- c("personalData.email", 
            "levelProgress.firstAssignment", "levelProgress.fifthAssignment",
            "levelProgress.finishFirstProject", "levelProgress.numberOfMissingTasks",
            "levelProgress.slackRegistration")
  lapply(names(orange_users), function(name){
    if(grepl("created_at",name)){
      keep <<- c(keep, name)
    } 
  })
  
  user_submissions <- orange_users[keep]
  user_next_submissions <- left_join(user_next, user_submissions, by = c("email" = "personalData.email"))
  keep <- !duplicated(user_next_submissions[c("email", "subscription", "transaction")])
  user_next_submissions_clean <- user_next_submissions[keep,]
  
  '/
  print("preparing user_next_sessions1")
  RWNM <<- 1
  user_next_sessions1 <<- as.data.frame(do.call(rbind, apply(user_next_submissions_clean,1,aggregateOrange4b)))
  user_next_sessions1$subscription <<- sapply(user_next_sessions1$subscription, as.integer)
  user_next_sessions1$transaction <<- sapply(user_next_sessions1$transaction, as.integer)
  user_next_sessions1$period <<- sapply(user_next_sessions1$period, as.integer)
  user_next_sessions1$submissionCnt <<- sapply(user_next_sessions1$submissionCnt, as.integer)
  user_next_sessions1$missingTasks <<- sapply(user_next_sessions1$missingTasks, as.integer)
  user_next_sessions1$paidnext[user_next_sessions1$paidnext == "TRUE"] <<- "yes"
  user_next_sessions1$paidnext[user_next_sessions1$paidnext == "FALSE"] <<- "no"
  user_next_sessions1$paidnext <<- sapply(user_next_sessions1$paidnext, as.factor)
  user_next_sessions1$slack[user_next_sessions1$slack == "TRUE"] <<- "yes"
  user_next_sessions1$slack[user_next_sessions1$slack == "FALSE"] <<- "no"
  user_next_sessions1$slack <<- sapply(user_next_sessions1$slack, as.factor)
  user_next_sessions1$firstAssignment[user_next_sessions1$firstAssignment == "TRUE"] <<- "yes"
  user_next_sessions1$firstAssignment[user_next_sessions1$firstAssignment == "FALSE"] <<- "no"
  user_next_sessions1$firstAssignment <<- sapply(user_next_sessions1$firstAssignment, as.factor)
  user_next_sessions1$fifthAssignment[user_next_sessions1$fifthAssignment == "TRUE"] <<- "yes"
  user_next_sessions1$fifthAssignment[user_next_sessions1$fifthAssignment == "FALSE"] <<- "no"
  user_next_sessions1$fifthAssignment <<- sapply(user_next_sessions1$fifthAssignment, as.factor)
  user_next_sessions1$firstProject[user_next_sessions1$firstProject == "TRUE"] <<- "yes"
  user_next_sessions1$firstProject[user_next_sessions1$firstProject == "FALSE"] <<- "no"
  user_next_sessions1$firstProject <<- sapply(user_next_sessions1$firstProject, as.factor)
  lapply(names(user_next_sessions1), function(column){
    if(grepl("session", column)){
      user_next_sessions1[,column] <<- sapply(user_next_sessions1[,column], as.numeric)
    }
  })
  /'
  
  '/
  print("preparing user_next_sessions2")
  RWNM <<- 1
  user_next_sessions2 <<- as.data.frame(do.call(rbind, apply(user_next_submissions_clean,1,aggregateOrange4bb)))
  user_next_sessions2$subscription <<- sapply(user_next_sessions2$subscription, as.integer)
  user_next_sessions2$transaction <<- sapply(user_next_sessions2$transaction, as.integer)
  user_next_sessions2$period <<- sapply(user_next_sessions2$period, as.integer)
  user_next_sessions2$submissionCnt <<- sapply(user_next_sessions2$submissionCnt, as.integer)
  user_next_sessions2$paidnext[user_next_sessions2$paidnext == "TRUE"] <<- "yes"
  user_next_sessions2$paidnext[user_next_sessions2$paidnext == "FALSE"] <<- "no"
  user_next_sessions2$paidnext <<- sapply(user_next_sessions2$paidnext, as.factor)
  user_next_sessions2$slack[user_next_sessions2$slack == "TRUE"] <<- "yes"
  user_next_sessions2$slack[user_next_sessions2$slack == "FALSE"] <<- "no"
  user_next_sessions2$slack <<- sapply(user_next_sessions2$slack, as.factor)
  user_next_sessions2$firstAssignment[user_next_sessions2$firstAssignment == "TRUE"] <<- "yes"
  user_next_sessions2$firstAssignment[user_next_sessions2$firstAssignment == "FALSE"] <<- "no"
  user_next_sessions2$firstAssignment <<- sapply(user_next_sessions2$firstAssignment, as.factor)
  user_next_sessions2$fifthAssignment[user_next_sessions2$fifthAssignment == "TRUE"] <<- "yes"
  user_next_sessions2$fifthAssignment[user_next_sessions2$fifthAssignment == "FALSE"] <<- "no"
  user_next_sessions2$fifthAssignment <<- sapply(user_next_sessions2$fifthAssignment, as.factor)
  user_next_sessions2$firstProject[user_next_sessions2$firstProject == "TRUE"] <<- "yes"
  user_next_sessions2$firstProject[user_next_sessions2$firstProject == "FALSE"] <<- "no"
  user_next_sessions2$firstProject <<- sapply(user_next_sessions2$firstProject, as.factor)
  user_next_sessions2$missingTasks[user_next_sessions2$missingTasks == "TRUE"] <<- "yes"
  user_next_sessions2$missingTasks[user_next_sessions2$missingTasks == "FALSE"] <<- "no"
  user_next_sessions2$missingTasks <<- sapply(user_next_sessions2$missingTasks, as.factor)
  lapply(names(user_next_sessions2), function(column){
    if(grepl("session", column)){
      user_next_sessions2[,column] <<- sapply(user_next_sessions2[,column], as.numeric)
    }
  })  
  /'
  

  print("preparing user_next_sessions3")
  user_next_sessions3 <<- user_next_sessions2
  lapply(names(user_next_sessions3), function(column){
    if(grepl("session", column)){
      colvals <- user_next_sessions3[,column]
      mod <- is.na(colvals)
      user_next_sessions3[mod, column] <<- 0
    }
  })
  
  user_next_sessions3$subscription <<- sapply(user_next_sessions3$subscription, as.integer)
  user_next_sessions3$transaction <<- sapply(user_next_sessions3$transaction, as.integer)
  user_next_sessions3$period <<- sapply(user_next_sessions3$period, as.integer)
  user_next_sessions3$submissionCnt <<- sapply(user_next_sessions3$submissionCnt, as.integer)
  user_next_sessions3$missingTasks <<- sapply(user_next_sessions3$missingTasks, as.integer)
  user_next_sessions3$paidnext[user_next_sessions3$paidnext == "TRUE"] <<- "yes"
  user_next_sessions3$paidnext[user_next_sessions3$paidnext == "FALSE"] <<- "no"
  user_next_sessions3$paidnext <<- sapply(user_next_sessions3$paidnext, as.factor)
  user_next_sessions3$slack[user_next_sessions3$slack == "TRUE"] <<- "yes"
  user_next_sessions3$slack[user_next_sessions3$slack == "FALSE"] <<- "no"
  user_next_sessions3$slack <<- sapply(user_next_sessions3$slack, as.factor)
  user_next_sessions3$firstAssignment[user_next_sessions3$firstAssignment == "TRUE"] <<- "yes"
  user_next_sessions3$firstAssignment[user_next_sessions3$firstAssignment == "FALSE"] <<- "no"
  user_next_sessions3$firstAssignment <<- sapply(user_next_sessions3$firstAssignment, as.factor)
  user_next_sessions3$fifthAssignment[user_next_sessions3$fifthAssignment == "TRUE"] <<- "yes"
  user_next_sessions3$fifthAssignment[user_next_sessions3$fifthAssignment == "FALSE"] <<- "no"
  user_next_sessions3$fifthAssignment <<- sapply(user_next_sessions3$fifthAssignment, as.factor)
  user_next_sessions3$firstProject[user_next_sessions3$firstProject == "TRUE"] <<- "yes"
  user_next_sessions3$firstProject[user_next_sessions3$firstProject == "FALSE"] <<- "no"
  user_next_sessions3$firstProject <<- sapply(user_next_sessions3$firstProject, as.factor)
  lapply(names(user_next_sessions3), function(column){
    if(grepl("session", column)){
      user_next_sessions3[,column] <<- sapply(user_next_sessions3[,column], as.numeric)
    }
  })


  print("preparing user_next_sessions4")
  RWNM <<- 1
  user_next_sessions4 <<- as.data.frame(do.call(rbind, apply(user_next_submissions_clean,1,aggregateOrange4b2)))
  user_next_sessions4$subscription <<- sapply(user_next_sessions4$subscription, as.integer)
  user_next_sessions4$transaction <<- sapply(user_next_sessions4$transaction, as.integer)
  user_next_sessions4$period <<- sapply(user_next_sessions4$period, as.integer)
  user_next_sessions4$submissionCnt <<- sapply(user_next_sessions4$submissionCnt, as.integer)
  user_next_sessions4$missingTasks <<- sapply(user_next_sessions4$missingTasks, as.integer)
  user_next_sessions4$paidnext[user_next_sessions4$paidnext == "TRUE"] <<- "yes"
  user_next_sessions4$paidnext[user_next_sessions4$paidnext == "FALSE"] <<- "no"
  user_next_sessions4$paidnext <<- sapply(user_next_sessions4$paidnext, as.factor)
  user_next_sessions4$slack[user_next_sessions4$slack == "TRUE"] <<- "yes"
  user_next_sessions4$slack[user_next_sessions4$slack == "FALSE"] <<- "no"
  user_next_sessions4$slack <<- sapply(user_next_sessions4$slack, as.factor)
  user_next_sessions4$firstAssignment[user_next_sessions4$firstAssignment == "TRUE"] <<- "yes"
  user_next_sessions4$firstAssignment[user_next_sessions4$firstAssignment == "FALSE"] <<- "no"
  user_next_sessions4$firstAssignment <<- sapply(user_next_sessions4$firstAssignment, as.factor)
  user_next_sessions4$fifthAssignment[user_next_sessions4$fifthAssignment == "TRUE"] <<- "yes"
  user_next_sessions4$fifthAssignment[user_next_sessions4$fifthAssignment == "FALSE"] <<- "no"
  user_next_sessions4$fifthAssignment <<- sapply(user_next_sessions4$fifthAssignment, as.factor)
  user_next_sessions4$firstProject[user_next_sessions4$firstProject == "TRUE"] <<- "yes"
  user_next_sessions4$firstProject[user_next_sessions4$firstProject == "FALSE"] <<- "no"
  user_next_sessions4$firstProject <<- sapply(user_next_sessions4$firstProject, as.factor)
  lapply(names(user_next_sessions4), function(column){
    if(grepl("session", column)){
      user_next_sessions4[,column] <<- sapply(user_next_sessions4[,column], as.numeric)
    }
  })


  print("preparing user_next_sessions5")
  RWNM <<- 1
  user_next_sessions5 <<- as.data.frame(do.call(rbind, apply(user_next_submissions_clean,1,aggregateOrange4bb2)))
  user_next_sessions5$subscription <<- sapply(user_next_sessions5$subscription, as.integer)
  user_next_sessions5$transaction <<- sapply(user_next_sessions5$transaction, as.integer)
  user_next_sessions5$period <<- sapply(user_next_sessions5$period, as.integer)
  user_next_sessions5$submissionCnt <<- sapply(user_next_sessions5$submissionCnt, as.integer)
  user_next_sessions5$missingTasks <<- sapply(user_next_sessions5$missingTasks, as.integer)
  user_next_sessions5$paidnext[user_next_sessions5$paidnext == "TRUE"] <<- "yes"
  user_next_sessions5$paidnext[user_next_sessions5$paidnext == "FALSE"] <<- "no"
  user_next_sessions5$paidnext <<- sapply(user_next_sessions5$paidnext, as.factor)
  user_next_sessions5$slack[user_next_sessions5$slack == "TRUE"] <<- "yes"
  user_next_sessions5$slack[user_next_sessions5$slack == "FALSE"] <<- "no"
  user_next_sessions5$slack <<- sapply(user_next_sessions5$slack, as.factor)
  user_next_sessions5$firstAssignment[user_next_sessions5$firstAssignment == "TRUE"] <<- "yes"
  user_next_sessions5$firstAssignment[user_next_sessions5$firstAssignment == "FALSE"] <<- "no"
  user_next_sessions5$firstAssignment <<- sapply(user_next_sessions5$firstAssignment, as.factor)
  user_next_sessions5$fifthAssignment[user_next_sessions5$fifthAssignment == "TRUE"] <<- "yes"
  user_next_sessions5$fifthAssignment[user_next_sessions5$fifthAssignment == "FALSE"] <<- "no"
  user_next_sessions5$fifthAssignment <<- sapply(user_next_sessions5$fifthAssignment, as.factor)
  user_next_sessions5$firstProject[user_next_sessions5$firstProject == "TRUE"] <<- "yes"
  user_next_sessions5$firstProject[user_next_sessions5$firstProject == "FALSE"] <<- "no"
  user_next_sessions5$firstProject <<- sapply(user_next_sessions5$firstProject, as.factor)
  lapply(names(user_next_sessions5), function(column){
    if(grepl("session", column)){
      user_next_sessions5[,column] <<- sapply(user_next_sessions5[,column], as.numeric)
    }
  })

  print("preparing user_next_sessions6")
  user_next_sessions6 <<- user_next_sessions5
  lapply(names(user_next_sessions6), function(column){
    if(grepl("session", column)){
      colvals <- user_next_sessions3[,column]
      mod <- is.na(colvals)
      user_next_sessions3[mod, column] <<- 0
    }
  })
  user_next_sessions6$subscription <<- sapply(user_next_sessions6$subscription, as.integer)
  user_next_sessions6$transaction <<- sapply(user_next_sessions6$transaction, as.integer)
  user_next_sessions6$period <<- sapply(user_next_sessions6$period, as.integer)
  user_next_sessions6$submissionCnt <<- sapply(user_next_sessions6$submissionCnt, as.integer)
  user_next_sessions6$missingTasks <<- sapply(user_next_sessions6$missingTasks, as.integer)
  user_next_sessions6$paidnext[user_next_sessions6$paidnext == "TRUE"] <<- "yes"
  user_next_sessions6$paidnext[user_next_sessions6$paidnext == "FALSE"] <<- "no"
  user_next_sessions6$paidnext <<- sapply(user_next_sessions6$paidnext, as.factor)
  user_next_sessions6$slack[user_next_sessions6$slack == "TRUE"] <<- "yes"
  user_next_sessions6$slack[user_next_sessions6$slack == "FALSE"] <<- "no"
  user_next_sessions6$slack <<- sapply(user_next_sessions6$slack, as.factor)
  user_next_sessions6$firstAssignment[user_next_sessions6$firstAssignment == "TRUE"] <<- "yes"
  user_next_sessions6$firstAssignment[user_next_sessions6$firstAssignment == "FALSE"] <<- "no"
  user_next_sessions6$firstAssignment <<- sapply(user_next_sessions6$firstAssignment, as.factor)
  user_next_sessions6$fifthAssignment[user_next_sessions6$fifthAssignment == "TRUE"] <<- "yes"
  user_next_sessions6$fifthAssignment[user_next_sessions6$fifthAssignment == "FALSE"] <<- "no"
  user_next_sessions6$fifthAssignment <<- sapply(user_next_sessions6$fifthAssignment, as.factor)
  user_next_sessions6$firstProject[user_next_sessions6$firstProject == "TRUE"] <<- "yes"
  user_next_sessions6$firstProject[user_next_sessions6$firstProject == "FALSE"] <<- "no"
  user_next_sessions6$firstProject <<- sapply(user_next_sessions6$firstProject, as.factor)
  lapply(names(user_next_sessions6), function(column){
    if(grepl("session", column)){
      user_next_sessions6[,column] <<- sapply(user_next_sessions6[,column], as.numeric)
    }
  })

}