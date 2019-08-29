
processOrange <- function(x){
  
  email <- x[["email"]]
  period <- as.integer(x[["period"]])
  start <- parse_date(x[["startDate"]])
  end <- parse_date(x[["endDate"]])
  paid_next <- x[["paidnext"]]
  handler <- x[["paymentHandler"]]
  plan_id <- x[["planId"]]
  cost <- x[["cost"]]
  spent <- x[["spent"]]
  currency <- x[["currency"]]
  locale <- x[["locale"]]
  has_slack <- x[["hasSlack"]]
  has_facebook <- x[["hasFacebook"]]
  has_google <- x[["hasGoogle"]]
  has_freemat <- x[["hasFreematerial"]]
  coupon <- x[["coupon"]]                   #NA_character_
  utm_campaign <- x[["utmCampaign"]]        #NA_character_
  utm_medium <- x[["utmMedium"]]            #NA_character_
  utm_source <- x[["utmSource"]]            #NA_character_
  #hasGithub someday...
  
  sessNames <- paste0("session", 1:400, sep="")
  sessions <- setNames(rep(0,400), sessNames)
  lessNames <- paste0("lesson.",lessonHashes)
  lessons <- setNames(rep(0,lessonCnt), lessNames)
  assignments <- setNames(rep(0,assignmentCnt), submissions)
  
  RWNM <<- RWNM + 1
  if(RWNM%%100 == 0){
    print(paste("orange processing:", email))
    print(paste("rownumber: ", RWNM))
  }
  
  submissions <- x[grepl("submissions", names(x))]
  submissions_df <- bind_rows(submissions, .id = "task")

  if(nrow(submissions_df) < 1) 
    return(as.list(c(email=email, endDate=end, paidnext=paid_next,
                     period=period, locale=locale, planId=plan_id, cost=cost, spent=spent,
                     currency=currency,
                     part1=0, part2=0, part3=0, part4=0,
                     duplicatedSubmissions=0,
                     paymentHandler=handler, submissions=0,
                     hasSlack=has_slack, hasFacebook=has_facebook,
                     hasGoogle=has_google, hasFreematerial=has_freemat, coupon=coupon, 
                     utmSource=utm_source, utmMedium=utm_medium, utmCampaign=utm_campaign,
                     assignments
                     #sessions
                     )))

  #leave sub.dsfaf.assigment as submissions_df$task
  #submissions_df$lesson <- str_split_fixed(submissions_df$task, "\\.", 3)[,2]
  #submissions_df$assignment <- str_split_fixed(submissions_df$task, "\\.", 3)[,3]
  submissions_df$duplicated <- duplicated(submissions_df$task)
  #submissions_df <- submissions_df[,!grepl("task",names(submissions_df))]
  submissions_df <- submissions_df[,c("task","created_at","duplicated","isSkipped","value")]
  
  session_times <- as.numeric(difftime(submissions_df$created_at[order(submissions_df$created_at)],
                            lag(submissions_df$created_at[order(submissions_df$created_at)]), units = "secs"))
  submissions_sess <- submissions_df[order(submissions_df$created_at),]
  submissions_sess$sessionTime <- session_times
  submissions_sess$sessionTime[is.na(submissions_sess$sessionTime)] <- 0
  
  threshold_high <- 60*60*1.5
  threshold_low <- 3
  part1 <- 0
  part2 <- 0
  part3 <- 0
  part4 <- 0
  
  timedf <- difftime(end, start, units = "secs")/4
  limit1 <- start+1*timedf
  limit2 <- start+2*timedf
  limit3 <- start+3*timedf
  
  this_subscription <- (start<submissions_sess$created_at & submissions_sess$created_at < end)
  submission_cnt <- sum(this_subscription)
  submissions_this <- submissions_sess[this_subscription,]
  multiple_answers <- sum(submissions_this$duplicated)

  part1 <- sum(start < submissions_this$created_at & submissions_this$created_at < limit1)
  part2 <- sum(limit1 <= submissions_this$created_at & submissions_this$created_at < limit2)
  part3 <- sum(limit2 <= submissions_this$created_at & submissions_this$created_at < limit3)
  part4 <- sum(limit3 <= submissions_this$created_at & submissions_this$created_at < end)
  
  valid <- (threshold_low < submissions_this$sessionTime & 
            submissions_this$sessionTime < threshold_high)
  if(any(valid)){
    #TODO: consider using here median instead of mean
    submissions_this[!valid,"sessionTime"] <- mean(submissions_this[valid,"sessionTime"])
    #submissions_this <- 
    #  submissions_this %>%
    #  group_by(lesson) %>%
    #  mutate(lessonTime=sum(sessionTime))
    sessiontime_df <- 
      submissions_this[!duplicated(submissions_this[c("task","sessionTime")]),][,c("task","sessionTime")]
    apply(sessiontime_df, 1, function(S){
      assignments[[S[["task"]]]] <<- S[["sessionTime"]]
    })
  }else
    return(as.list(c(email=email, endDate=end, paidnext=paid_next,
                     period=period, locale=locale, planId=plan_id, cost=cost, spent=spent,
                     currency=currency,
                     part1=part1, part2=part2, part3=part3, part4=part4,
                     duplicatedSubmissions=multiple_answers,
                     paymentHandler=handler, submissions=submission_cnt,
                     hasSlack=has_slack, hasFacebook=has_facebook,
                     hasGoogle=has_google, hasFreematerial=has_freemat, coupon=coupon, 
                     utmSource=utm_source, utmMedium=utm_medium, utmCampaign=utm_campaign,
                     assignments
                     #sessions
                     )))
  
  #sessions <- setNames(submissions_this$sessionTime[1:400], sessNames)
  #sessions[is.na(sessions)] <- 0
  
  as.list(c(email=email, endDate=end, paidnext=paid_next,
            period=period, locale=locale, planId=plan_id, cost=cost, spent=spent,
            currency=currency,
            part1=part1, part2=part2, part3=part3, part4=part4,
            duplicatedSubmissions=multiple_answers,
            paymentHandler=handler, submissions=submission_cnt,
            hasSlack=has_slack, hasFacebook=has_facebook,
            hasGoogle=has_google, hasFreematerial=has_freemat, coupon=coupon, 
            utmSource=utm_source, utmMedium=utm_medium, utmCampaign=utm_campaign,
            assignments
            #sessions
            ))

}