
processCentral3 <- function(x){
  
  email <- x[["authentication.email_address"]]
  RWNM <<- RWNM + 1
  
  if (RWNM %% 100 == 0){
    print(paste("central processing row: ", RWNM))
  }
  
  has_slack <- ifelse(!is.na(x[["authentication.slack_user_id"]]),TRUE,FALSE)
  has_google <- ifelse(!is.na(x[["authentication.google_user_id"]]), TRUE, FALSE)
  has_facebook <- ifelse(!is.na(x[["authentication.facebook_user_id"]]), TRUE, FALSE)
  
  locale <- ifelse(!is.na(x[["locale.fullLocaleCode"]]),as.character(x[["locale.fullLocaleCode"]]),NA)
  coupon <- ifelse(!is.na(x[["codes.coupon"]]),x[["codes.coupon"]],NA)
  has_freematerial <- ifelse(!is.na(x[["curriculumPermissions.internal.freeMaterials"]]),ifelse(x[["curriculumPermissions.internal.freeMaterials"]], TRUE, FALSE),NA)
  utm_campaign <- ifelse(!is.na(x[["campaign.utm_campaign"]]),x[["campaign.utm_campaign"]],NA)
  utm_medium <- ifelse(!is.na(x[["campaign.utm_medium"]]),x[["campaign.utm_medium"]],NA)
  utm_source <- ifelse(!is.na(x[["campaign.utm_source"]]),x[["campaign.utm_source"]],NA)
  
  if(!is.null(x[["oldSubscriptions"]]) && 
     !is.null(x[["oldSubscriptions"]][["transactions"]]) &&
     length(x[["oldSubscriptions"]][["transactions"]])!=0 &&
     length(unlist(x[["oldSubscriptions"]][["transactions"]]))!=0){
    oldSubscriptions <- unnest(x[["oldSubscriptions"]])
  }else{
    return(as.list(c(email=NA, period=NA, startDate=NA, endDate=NA, 
                     paidnext=NA, paymentHandler=NA, planId=NA, cost=NA, spent=NA,
                     currency=NA,
                     locale=NA, hasSlack=NA, hasFacebook=NA, hasGoogle=NA, 
                     hasFreematerial=NA, coupon=NA, 
                     utmSource=NA, utmMedium=NA, utmCampaign=NA)))
  }
  paidSubscriptions <- oldSubscriptions[grepl("paid",oldSubscriptions[["plan.type"]]),]
  if(dim(paidSubscriptions)[1]==0)
    return(as.list(c(email=NA, period=NA, startDate=NA, endDate=NA, 
                     paidnext=NA, paymentHandler=NA, planId=NA, cost=NA, spent=NA,
                     currency=NA,
                     locale=NA, hasSlack=NA, hasFacebook=NA, hasGoogle=NA, 
                     hasFreematerial=NA, coupon=NA, 
                     utmSource=NA, utmMedium=NA, utmCampaign=NA)))
  
  which_subscription <- 0
  which_id <- character()
  which_entry <- 0
  price_acc <- 0
  
  user_next <- do.call(rbind, apply(paidSubscriptions, 1, function(y){
    which_entry <<- which_entry + 1
    which_id <<- y[["uniqueId"]]
    
    if(which_entry == 1){
      which_subscription <<- which_subscription+1
    }else{
      which_subscription <<- ifelse(grepl(which_id,paidSubscriptions[which_entry-1,"uniqueId"]),
                                    which_subscription, which_subscription+1)
    }
    
    if(is.na(paidSubscriptions[which_entry+1, "uniqueId"])){
      paid_next <- FALSE
    }else{
      paid_next <- ifelse(grepl(which_id,paidSubscriptions[which_entry+1,"uniqueId"]),TRUE,FALSE)
    }
    
    internalId <- y[["plan.internalId"]]
    periodInDays <- as.integer(y[["plan.billingDetails.periodInDays"]])
    price <- y[["plan.amount.gross"]]
    handler <- y[["handler.processorName"]]
    paid <- y[["transaction.amount"]]
    price_acc <<- price_acc + as.numeric(paid)
    currency <- y[["transaction.currency.ISO3"]]
    start <- parse_date(y[["transaction.fulfillmentDateTime"]])
    end <- start + (periodInDays*24*60*60)
    
    #5-days tolerance
    if(Sys.time() < end+(24*60*60*5)) paid_next <- NA
    
    as.list(c(email=email, period=which_entry, startDate=start, endDate=end, 
              paidnext=paid_next, paymentHandler=handler, planId=internalId, cost=price, spent=price_acc,
              currency=currency,
              locale=locale, hasSlack=has_slack, hasFacebook=has_facebook, hasGoogle=has_google, 
              hasFreematerial=has_freematerial, coupon=coupon, 
              utmSource=utm_source, utmMedium=utm_medium, utmCampaign=utm_campaign))
  }))
  
  which_subscription <- which_subscription + 1
  which_id <- character()
  which_period <- which_entry
  which_entry <- 0
  price_acc <- 0
  
  if(x[["subscription2.isActive"]] && grepl("paid",x[["subscription2.plan.type"]]) && !is.null(nrow(x[["subscription2.transactions"]]))){
    subscription2 <- x[["subscription2.transactions"]] 
    user_next <- rbind(user_next, do.call(rbind, apply(subscription2, 1, function(z){
      which_entry <<- which_entry + 1
      
      paid_next <- ifelse(is.na(subscription2[which_entry+1, "uniqueId"]),NA,TRUE)
      
      internalId <- x[["subscription2.plan.internalId"]]
      periodInDays <- x[["subscription2.plan.billingDetails.periodInDays"]]
      price <- x[["subscription2.plan.amount.gross"]]
      handler <- z[["handler.processorName"]]
      paid <- z[["transaction.amount"]]
      price_acc <<- price_acc + as.numeric(paid)
      currency <- z[["transaction.currency.ISO3"]]
      start <- parse_date(z[["transaction.fulfillmentDateTime"]])
      end <- start + (periodInDays*24*60*60)
      
      #5-days tolerance
      if(Sys.time() < end+(24*60*60*5)) paid_next <- NA
      
      as.list(c(email=email, period=which_period+which_entry, startDate=start, endDate=end, 
                paidnext=paid_next, paymentHandler=handler, planId=internalId, cost=price, spent=price_acc,
                currency=currency,
                locale=locale, hasSlack=has_slack, hasFacebook=has_facebook, hasGoogle=has_google, 
                hasFreematerial=has_freematerial, coupon=coupon, 
                utmSource=utm_source, utmMedium=utm_medium, utmCampaign=utm_campaign))
    })))
  }
  
  user_next
  
  #later improvement
  #grapes_cnt | count plans
  #apple_cnt | count plans
  #melon_cnt | count plans
  #latest_submission_datetime, latest_activity_datetime
}