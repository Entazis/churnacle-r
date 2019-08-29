
aggr_events <- function(x){
  
  email_address <- x[["email_address"]]
  datetime <- x[["datetime"]]
  
  if(!is.na(x[["Onboarding: Arrived to Slack"]]) && 
     x[["Onboarding: Arrived to Slack"]] == TRUE){
    arrived_to_slack <- x[["Onboarding: Arrived to Slack"]]
    arrived_to_slack_datetime <- x[["datetime"]]
  } else {
    arrived_to_slack <- FALSE
    arrived_to_slack_datetime <- 0
  }
  if(!is.na(x[["Onboarding: Arrived to Orange"]]) &&
     x[["Onboarding: Arrived to Orange"]] == TRUE){
    arrived_to_orange <- x[["Onboarding: Arrived to Orange"]]
    arrived_to_orange_datetime <- x[["datetime"]]
  } else {
    arrived_to_orange <- FALSE
    arrived_to_orange_datetime <- 0
  }
  if(!is.na(x[["Activation: Orange - submitted first assignment"]]) &&
     x[["Activation: Orange - submitted first assignment"]] == TRUE){
    submitted_1 <- x[["Activation: Orange - submitted first assignment"]]
    submitted_1_datetime <- x[["datetime"]]
  } else {
    submitted_1 <- FALSE
    submitted_1_datetime <- 0
  }
  if(!is.na(x[["Activation: Orange - submitted fifth assignment"]]) &&
     x[["Activation: Orange - submitted fifth assignment"]] == TRUE){
    submitted_5 <- x[["Activation: Orange - submitted fifth assignment"]]
    submitted_5_datetime <- x[["datetime"]]
  } else {
    submitted_5 <- FALSE
    submitted_5_datetime <- 0
  }
  #if(submitted_5 && submitted_1) 
   # first_session_time <- submitted_5_datetime-submitted_1_datetime
  
  
  #x[1:length(x),c(email_address, event, is_successful, datetime),by="email_address"]
  
  #fixit
  
  data.frame("email_address" = email_address, 
             "Activation: Orange - submitted fifth assignment" = submitted_5,
             "Activation: Orange - submitted first assignment" = submitted_1,
             "Submitted first assignment datetime" = submitted_1_datetime,
             "Submitted fifth assignment datetime" = submitted_5_datetime,
             "Onboarding: Arrived to Orange" = arrived_to_orange,
             "Onboarding: Arrived to Slack" = arrived_to_slack,
             "Arrived to Slack datetime" = arrived_to_slack_datetime,
             "Arrived to Orange datetime" = arrived_to_orange_datetime,
             stringsAsFactors = FALSE)
}