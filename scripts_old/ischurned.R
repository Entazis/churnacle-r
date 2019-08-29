
isChurned <- function(x){
  churn<-FALSE
  user_id <- x[["_id"]]
  subs2.isAct <- x[["subscription2.isActive"]]
  subs.paidCount <- x[["subscription.old_plans.paidCount"]]
  if(subs2.isAct == FALSE && subs.paidCount > 0) churn <-TRUE
  
  data.frame("_id" = user_id, "churn" = churn, stringsAsFactors = FALSE)
}