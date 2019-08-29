
prepareSessions <- function(x){
  
  email <- x[["email"]]
  period <- as.integer(x["period"])
  start <- parse_date(x[["startDate"]])
  end <- parse_date(x[["endDate"]])
  handler <- x[["paymentHandler"]]

  postal_code <- x[["postalCode"]]
  slack_id <- x[["slackId"]]
  google_id <- x[["googleId"]]
  facebook_id <- x[["facebookId"]]
  
  last_subm <- ifelse(is.na(x[["lastSubmission"]]),NA,x[["lastSubmission"]])
  last_act <- ifelse(is.na(x[["lastActivity"]]),NA,x[["lastActivity"]])
  
  locale <- x[["locale"]]
  coupon <- x[["coupon"]]
  free_mat <- x[["freeMaterial"]]
  utm_campaign <- x[["utmCampaign"]]
  utm_medium <- x[["utmMedium"]]
  utm_source <- x[["utmSource"]]
  #grapes_cnt | count plans
  #apple_cnt | count plans
  #melon_cnt | count plans
  
  fifth_ass <- ifelse(!is.na(x[["levelProgress.fifthAssignment"]]),ifelse(x[["levelProgress.fifthAssignment"]], TRUE, FALSE),NA)    
  first_proj <- ifelse(!is.na(x[["levelProgress.finishFirstProject"]]),ifelse(x[["levelProgress.finishFirstProject"]], TRUE, FALSE),NA)
  first_ass <- ifelse(!is.na(x[["levelProgress.firstAssignment"]]),ifelse(x[["levelProgress.firstAssignment"]], TRUE, FALSE),NA)                                                       
  #missing_tasks <- ifelse(is.na(x[["levelProgress.numberOfMissingTasks"]]),"miss",x[["levelProgress.numberOfMissingTasks"]])                                                  
  slackreg <- ifelse(!is.na(x[["levelProgress.slackRegistration"]]),ifelse(x[["levelProgress.slackRegistration"]], TRUE, FALSE),NA)
  
  last_finish <- ifelse(!is.na(x[["projectProgress.latest.isFinished"]]),ifelse(x[["projectProgress.latest.isFinished"]], TRUE, FALSE),NA)
  last_lesson <- ifelse(is.na(x[["projectProgress.latest.lessonHash"]]),NA,x[["projectProgress.latest.lessonHash"]])
  last_name <- ifelse(is.na(x[["projectProgress.latest.name"]]),NA,x[["projectProgress.latest.name"]])
  
  paidnext <- x[["paidnext"]]
  
  print(paste("orange processing:", email))
  print(paste("rownumber: ", RWNM))
  RWNM <<- RWNM + 1
  
  #FIXME: count the same submissions.assigment.created_at in the same row -> mistakes
  #orange_users[grep(".created_at.", names(orange_users))][,order(names(orange_users[grep(".created_at.", names(orange_users))]))]
  
  submissions <- x[grepl(".created_at.", names(x))]
  submissions <- sort(submissions, na.last = TRUE)
  submCount <- 0
  sessNum <- 0
  sessSum <- 0
  
  sessNames <- paste("session", toString(1), sep = "")
  
  #SESSIONS: 60
  #submissions <- submissions[1:100]
  lapply(2:length(submissions), function(x){
    sessNames <<- c(sessNames, paste("session", toString(x), sep = ""))
  })
  
  sessions <- vector("list", length(sessNames))
  names(sessions) <- sessNames
  
  threshold_high <- 60*60*1.5
  threshold_low <- 5
  impaired <- 0
  paired <- 0
  part1 <- 0
  part2 <- 0
  part3 <- 0
  part4 <- 0
  
  timedf <- (end-start)/4
  limit1 <- start+1*timedf
  limit2 <- start+2*timedf
  limit3 <- start+3*timedf
  
  if(period < 2){
    if(sum(!is.na(submissions)) >= 2){
      lapply(2:(length(submissions[!is.na(submissions)])), function(s){
        submfor <- parse_date(submissions[s])
        submback <-parse_date(submissions[s-1])
        
        if(start < submfor && submfor < end){
          submCount <<- submCount + 1
          if(start<submfor && submfor<limit1){
            part1 <<- part1 + 1
          } else if(limit1<=submfor && submfor<limit2){
            part2 <<- part2 + 1
          } else if(limit2<=submfor && submfor<limit3){
            part3 <<- part3 + 1
          } else if(limit3<=submfor && submfor<end){
            part4 <<- part4 + 1
          }
          if(start < submback && submfor < end &&
             threshold_high > difftime(submfor,submback, units = "secs") &&
             threshold_low < difftime(submfor,submback, units = "secs")){
            sessNum <<- sessNum + 1
            sessSum <<- sessSum + as.numeric(difftime(submfor,submback, units = "secs"))
            sessions[paste("session", toString(submCount), sep = "")] <<- 
              as.numeric(difftime(submfor,submback, units = "secs"))
          }
          else{
            sessions[paste("session", toString(submCount), sep = "")] <<- 0
          }
        } 
        
      })
      
      avg <- sessSum/sessNum
      if(is.nan(avg))
        avg <- 0
      
      if(submCount > 0){
        lapply(1:submCount, function(s){
          if(sessions[paste("session", toString(s), sep = "")] == 0)
            sessions[paste("session", toString(s), sep = "")] <<- avg
        })
      }
      
      lapply((sessNum+1):length(sessions), function(n){
        sessions[paste("session", toString(n), sep = "")] <<- NA
      })
      
    } else{
      submCount <- sum(!is.na(submissions))
      lapply(1:length(sessions), function(n){
        sessions[paste("session", toString(n), sep = "")] <<- NA
      })
    }
    
    c("email" = email, "endDate" = end, "paidnext" = paidnext,
      "period" = period, 
      "part1" = part1, "part2" = part2, "part3" = part3, "part4" = part4,
      "paymentHandler" = handler, "submissions" = submCount,
      
      "postalCode"=postal_code, "slackId"=slack_id,
      "googleId"=google_id, "facebookId"=facebook_id,
      
      "locale"=locale, "coupon"=coupon,
      "freeMaterial"=free_mat, "utmCampaign"=utm_campaign,
      "utmMedium"=utm_medium, "utmSource"=utm_source,
      
      "firstAssignment"=first_ass, "fifthAssignment"=fifth_ass,
      "firstProject"=first_proj, "slackRegistration"=slackreg,
      
      sessions)
  } else NULL
}