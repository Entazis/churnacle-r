
isChurned2 <- function(x){
  
  #fix the posiclt error, char string is not in a standard unambiguous format
  
  user_id <- x[["_id"]]
  subchurn <- NA
  activitychurn <- NA
  totalsubmissioncount <-NA

  if(!is.na(x[["learning.latest_submission_datetime"]])){
    latestsubmission <- x[["learning.latest_submission_datetime"]]
    if((as.POSIXct(Sys.time())-as.POSIXct(latestsubmission, origin="1970-01-01")) < 30) subchurn <- TRUE
  }
  
  if(!is.na(x[["learning.latest_activity_datetime"]])){
    latestactivity <- x[["learning.latest_activity_datetime"]]
    if((as.POSIXct(Sys.time())-as.POSIXct(latestactivity, origin="1970-01-01")) < 30) activitychurn <- TRUE
  }
  
  if(!is.na(x[["learning.total_submission_count"]])){
    totalsubmissioncount <- x[["learning.total_submission_count"]]
  }
  
  data.frame("_id" = user_id, "subchurn" = subchurn, "activitychurn" = activitychurn, stringsAsFactors = FALSE)
}