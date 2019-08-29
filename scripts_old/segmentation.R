
segmentation <- function(x){
  
  browser()
  #x <- list(x)
  email_address <- x[["authentication.email_address"]]

  subs2act <- x[["subscription2.isActive"]]
  subs2act <- gsub("^[[:space:]]*","",subs2act)
  x[["subscription2.isActive"]] <- subs2act
  
  if(subs2act == TRUE){
    subs2type <- x[["subscription2.plan.type"]]
    subs2start <- x[["subscription2.dateTimes.start"]]
    
    subs2periodcnt <- x[["subscription2.periodCounts.active"]] #space
    subs2periodcnt <- gsub("^[[:space:]]*","",subs2periodcnt)
    x[["subscription2.periodCounts.active"]] <- subs2periodcnt
    
    subs2planid <- x[["subscription2.plan.internalId"]]
    
    fix <- x[["subscription2.plan.amount.gross"]]
    fix <- gsub("^[[:space:]]*","",fix)
    x[["subscription2.plan.amount.gross"]] <- fix
    
  } else{
    x[["subscription2.periodCounts.active"]] <- NA
  }
  
  totalsubcnt <- x[["learning.total_submission_count"]] #space
  totalsubcnt <- gsub("^[[:space:]]*","",totalsubcnt)
  x[["learning.total_submission_count"]] <- totalsubcnt
  
  firstass <- x[["1st assignment"]]
  fifthass <- x[["5th assignment"]]
  churn <- x[["churn"]]
  
  gender <- x[["personal_data.gender"]] #NA
  country <- x[["address.country_name"]] #NA
  postalcode <- x[["address.postal_code"]] #NA
  
  lastsub <- x[["learning.latest_submission_datetime"]] #NA
  lastact <- x[["learning.latest_activity_datetime"]] #NA
  
  oldpaidcnt <- x[["subscription.old_plans.paidCount"]]
  oldpaidtime <- x[["subscription.old_plans.paidTime"]] #timediff
  oldfreecnt <- x[["subscription.old_plans.freeCount"]]
  oldfreetime <- x[["subscription.old_plans.freeTime"]]
  
  if(subs2act == TRUE && (subs2type == "free" || subs2type == "trial") && totalsubcnt == 0 && 
     firstass == 0 && fifthass == 0){
    x <- c(x, "freshly_acquired")
    #x$segment <- "freshly_acquired"
  } else if(subs2act == TRUE && (subs2type == "free" || subs2type == "trial") && 
            firstass == 1 && fifthass == 1 ){
    x <- c(x, "active_trial")
    #x$segment <- "active_trial"
  } else if(subs2act == TRUE && (subs2type == "free" || subs2type == "trial") &&
            totalsubcnt >= 10){
    x <- c(x, "finished_trial")
    #x$segment <- "finished_trial"
  } else if(subs2act == TRUE && subs2type == "paid" && oldpaidcnt == 0){
    x <- c(x, "fresh_paying")
    #x$segment <- "fresh_paying"
  } else if(subs2act == TRUE && subs2type == "paid" && oldpaidcnt > 0){
    x <- c(x, "active_paying")
    #x$segment <- "active_paying"
  } else if(!subs2act == TRUE && oldpaidcnt > 0){
    x <- c(x, "churned")
    #x$segment <- "churned"
  } else{
    x <- c(x, NA)
    #x$segment <- NA
  }
  
  #class(x)
  #print(x)
  #str(x)
  #browser()
  
  x
}