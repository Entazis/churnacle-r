
restoreDatetimes <- function(x){
  
  #ret <- data.frame(x)
  latest_subm <- x[["learning.latest_submission_datetime"]]
  latest_act <- x[["learning.latest_activity_datetime"]]
  subs2_start <- x[["subscription2.dateTimes.start"]]
  first_ass <- x[["1st assignment"]]
  fifth_ass <- x[["5th assignment"]]
  first_ass_date <- as.numeric(as.POSIXct(x[["1st assignment datetime"]]))
  fifth_ass_date <- as.numeric(as.POSIXct(x[["5th assignment datetime"]]))
  arr_orange <- x[["arrived to orange"]]
  arr_slack <- x[["arrived to slack"]]
  arr_orange_date <- as.numeric(as.POSIXct(x[["arrived to orange datetime"]]))
  arr_slack_date <- as.numeric(as.POSIXct(x[["arrived to slack datetime"]]))
  
  if(as.POSIXct(latest_subm) == "1970-01-01 01:00:00 CET"){latest_subm <- NA}
  
  if(as.POSIXct(latest_act) == "1970-01-01 01:00:00 CET"){latest_act <- NA}
  
  if(as.POSIXct(subs2_start) == "1970-01-01 01:00:00 CET"){subs2_start <- NA}
  
  if(as.numeric(first_ass) > 1){
    #browser()
    first_ass_date <- as.POSIXct(first_ass_date/as.numeric(first_ass), origin = "1970-01-01")
    first_ass <- 1
  }
  
  if(as.numeric(fifth_ass) > 1){
    #browser()
    fifth_ass_date <- as.POSIXct(fifth_ass_date/as.numeric(fifth_ass), origin = "1970-01-01") 
    fifth_ass <- 1
  }
  
  if(as.numeric(arr_orange) > 1){
    #browser()
    arr_orange_date <- as.POSIXct(arr_orange_date/as.numeric(arr_orange), origin = "1970-01-01")
    arr_orange <- 1
  }
  
  if(as.numeric(arr_slack) > 1){
    #browser()
    arr_slack_date <- as.POSIXct(arr_slack_date/as.numeric(arr_slack), origin = "1970-01-01")
    arr_slack <- 1
  }
  
  x[["learning.latest_submission_datetime"]] <- latest_subm
  x[["learning.latest_activity_datetime"]] <- latest_act
  x[["subscription2.dateTimes.start"]] <- subs2_start
  x[["1st assignment"]] <- first_ass
  x[["5th assignment"]] <- fifth_ass
  x[["1st assignment datetime"]] <- first_ass_date
  x[["5th assignment datetime"]] <- fifth_ass_date
  x[["arrived to orange"]] <- arr_orange
  x[["arrived to slack"]] <- arr_slack
  x[["arrived to orange datetime"]] <- arr_orange_date
  x[["arrived to slack datetime"]] <- arr_slack_date
  
  x
}