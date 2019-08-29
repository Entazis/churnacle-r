
prepareFirstMonth <- function(x){
  
  email <- x[["authentication.email_address"]]
  postal_code <- ifelse(!is.na(x[["address.postal_code"]]),as.character(x[["address.postal_code"]]),NA)
  slack_id <- ifelse(!is.na(x[["authentication.slack_user_id"]]),TRUE,FALSE)
  google_id <- ifelse(!is.na(x[["authentication.google_user_id"]]), TRUE, FALSE)
  facebook_id <- ifelse(!is.na(x[["authentication.facebook_user_id"]]), TRUE, FALSE)
  
  #??FIXME: if paidnext: NA Sys.time - last else subsc_end-last
  last_subm <- x[["learning.latest_submission_datetime"]]
  last_act <- x[["learning.latest_activity_datetime"]]
  
  locale <- ifelse(!is.na(x[["locale.fullLocaleCode"]]),as.character(x[["locale.fullLocaleCode"]]),NA)
  coupon <- ifelse(!is.na(x[["codes.coupon"]]),x[["codes.coupon"]],NA)
  free_mat <- ifelse(!is.na(x[["curriculumPermissions.internal.freeMaterials"]]),ifelse(x[["curriculumPermissions.internal.freeMaterials"]], TRUE, FALSE),NA)
  utm_campaign <- ifelse(!is.na(x[["campaign.utm_campaign"]]),x[["campaign.utm_campaign"]],NA)
  utm_medium <- ifelse(!is.na(x[["campaign.utm_medium"]]),x[["campaign.utm_medium"]],NA)
  utm_source <- ifelse(!is.na(x[["campaign.utm_source"]]),x[["campaign.utm_source"]],NA)
  #grapes_cnt | count plans
  #apple_cnt | count plans
  #melon_cnt | count plans
  
  oldSubs <- x[grepl("oldSubscriptions", names(x))]
  subs2 <- x[grepl("subscription2", names(x))]
  
  print(paste("central processing: ", email))
  print(paste("rownumber: ", RWNM))
  RWNM <<- RWNM + 1

  user_next <- data.frame(email=character(0), period=integer(0),
                          startDate=character(0), endDate=character(0), 
                          paidnext=character(0), 
                          
                          lastSubmission=character(0), lastActivity=character(0),
                          paymentHandler=character(0),
                          
                          postalCode=character(0), slackId=character(0),
                          googleId=character(0), facebookId=character(0),
                          
                          locale=character(0), coupon=character(0),
                          freeMaterial=character(0), utmCampaign=character(0),
                          utmMedium=character(0), utmSource=character(0),
                          
                          stringsAsFactors=FALSE)

  subcnt2 <- 0
  i <- 1
  
  sapply(0:5, function(s){
    if(is.na(oldSubs[paste("oldSubscriptions",s,"_id.$oid", sep = ".")]))
      NULL
    else
      sapply(0:15, function(t){
        if(is.na(oldSubs[paste("oldSubscriptions",s,"transactions",t,"_id.$oid", sep = ".")]))
          NULL
        else{
          per <- paste("oldSubscriptions",s,"plan.billingDetails.periodInDays", sep = ".")
          typ <- paste("oldSubscriptions",s,"plan.type", sep = ".")
          pro <- paste("oldSubscriptions",s,"transactions",t,"handler.processorName", sep = ".")
          ful <- paste("oldSubscriptions",s,"transactions",t,"transaction.fulfillmentDateTime", sep = ".")
          
          if(grepl("paid",oldSubs[typ])){
            start <- parse_date(oldSubs[ful])
            end <- start + as.numeric(oldSubs[per])*24*60*60
            handler <- oldSubs[pro]
            paidnext <- 
              ifelse(!is.na(oldSubs[paste("oldSubscriptions",s,"transactions",t+1,"_id.$oid", sep = ".")]),
                     "yes","no")
            user_next[i, ] <<- c(email=email, period=i,
                                 startDate=start, endDate=end, 
                                 paidnext=paidnext,
                                 
                                 lastSubmission=last_subm, lastActivity=last_act,
                                 paymentHandler=handler,
                                 
                                 postalCode=postal_code, slacId=slack_id,
                                 googleId=google_id, facebookId=facebook_id,
                                 
                                 locale=locale, coupon=coupon,
                                 freeMaterial=free_mat, utmCampaign=utm_campaign,
                                 utmMedium=utm_medium, utmSource=utm_source)
            i <<- i + 1
          }
        }
      })
    
  })
  
  if(grepl("TRUE", subs2["subscription2.isActive"]) && 
     grepl("paid", subs2["subscription2.plan.type"])){
    
    sapply(0:15,function(t){
      if(is.na(subs2[paste("subscription2.transactions",t,"_id.$oid", sep = ".")]))
        NULL
      else{
        per <- "subscription2.plan.billingDetails.periodInDays"
        pro <- paste("subscription2.transactions",t,"handler.processorName", sep = ".")
        ful <- paste("subscription2.transactions",t,"transaction.fulfillmentDateTime", sep = ".")
        
        start <- parse_date(subs2[ful])
        end <- start + as.numeric(subs2[per])*24*60*60
        handler <- subs2[pro]
        paidnext <- !is.na(subs2[paste("subscription2.transactions",t+1,"_id.$oid", sep = ".")])
   
        if(paidnext == TRUE){
          user_next[i, ] <<- c(email=email, period=i,
                               startDate=start, endDate=end, 
                               paidnext="yes",
                               
                               lastSubmission=last_subm, lastActivity=last_act,
                               paymentHandler=handler,
                               
                               postalCode=postal_code, slacId=slack_id,
                               googleId=google_id, facebookId=facebook_id,
                               
                               locale=locale, coupon=coupon,
                               freeMaterial=free_mat, utmCampaign=utm_campaign,
                               utmMedium=utm_medium, utmSource=utm_source)
        }
        else{
          user_next[i, ] <<- c(email=email, period=i,
                               startDate=start, endDate=end, 
                               paidnext=NA,
                               
                               lastSubmission=last_subm, lastActivity=last_act,
                               paymentHandler=handler,
                               
                               postalCode=postal_code, slacId=slack_id,
                               googleId=google_id, facebookId=facebook_id,
                               
                               locale=locale, coupon=coupon,
                               freeMaterial=free_mat, utmCampaign=utm_campaign,
                               utmMedium=utm_medium, utmSource=utm_source)
        }
        
        i <<- i + 1
      }
    })
  }
  
  user_next
}