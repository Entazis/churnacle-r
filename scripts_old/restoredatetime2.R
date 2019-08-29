
restoreDatetimes2 <- function(x){

  first_ass_date <- x[["1st assignment datetime"]]
  fifth_ass_date <- x[["5th assignment datetime"]]
  arr_orange_date <- x[["arrived to orange datetime"]]
  arr_slack_date <- x[["arrived to slack datetime"]]
  
  if(as.POSIXct(first_ass_date) == "1970-01-01 01:00:00 CET"){first_ass_date <- NA}
  
  if(as.POSIXct(fifth_ass_date) == "1970-01-01 01:00:00 CET"){fifth_ass_date <- NA}
  
  if(as.POSIXct(arr_orange_date) == "1970-01-01 01:00:00 CET"){arr_orange_date <- NA}
  
  if(as.POSIXct(arr_slack_date) == "1970-01-01 01:00:00 CET"){arr_slack_date <- NA}
  
  x[["1st assignment datetime"]] <- first_ass_date
  x[["5th assignment datetime"]] <- fifth_ass_date
  x[["arrived to orange datetime"]] <- arr_orange_date
  x[["arrived to slack datetime"]] <- arr_slack_date
  
  x
}