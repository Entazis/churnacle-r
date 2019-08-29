
filterNA <- function(x){
  
  #☼browser()
  ret <- x
  if(is.na(ret[["learning.latest_submission_datetime"]])){ret[["learning.latest_submission_datetime"]] <- 0}
  if(is.na(ret[["learning.latest_activity_datetime"]])){ret[["learning.latest_activity_datetime"]] <- 0}
  if(is.na(ret[["learning.total_submission_count"]])){ret[["learning.total_submission_count"]] <- 0}
  if(is.na(ret[["subscription2.dateTimes.start"]])){ret[["subscription2.dateTimes.start"]] <- 0}
  if(is.na(ret[["subscription2.periodCounts.active"]])){ret[["subscription2.periodCounts.active"]] <- 0}
  if(is.na(ret[["subscription2.plan.amount.gross"]])){ret[["subscription2.plan.amount.gross"]] <- 0}
  if(is.na(ret[["subscription2.plan.type"]])){ret[["subscription2.plan.type"]] <- 0}
  if(is.na(ret[["subscription2.plan.internalId"]])){ret[["subscription2.plan.internalId"]] <- 0}
  
  #data.frame("_id" = user_id, "churn" = churn, stringsAsFactors = FALSE)
  
  ret
}

#df <- data.frame(matrix(unlist(l), nrow=132, byrow=T))