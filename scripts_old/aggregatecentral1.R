
aggregateCentral <- function(x){
  
  #browser()
  oldSubs <- as.list(x[["oldSubscriptions"]])
  email <- x[["authentication.email_address"]]
  print(paste("central processing: ", email))
  
  subs2IsAct <- x[["subscription2.isActive"]]
  
  if(subs2IsAct == TRUE){
    subs2PlanType <- x[["subscription2.plan.type"]]
    subs2PlanId <- x[["subscription2.plan.internalId"]]
    subs2DateTimesStart <- x[["subscription2.dateTimes.start"]]
  }
  
  #subs2PlanName <- x[["subscription2.plan.name"]]
  #ubs2PlanPeriodInDays <- x[["subscription2.plan.billingDetails.periodInDays"]]
  #subs2PlanAmountGross <- x[["subscription2.plan.amount.gross"]]
  #subs2PeriodCountsAct <- x[["subscription2.periodCounts.active"]]
  
  
  Alap <-0
  Alma <- 0
  Dinnye <- 0
  paidcnt <- 0
  freecnt <- 0
  time_paid <- 0
  time_free <- 0
  i <- 0
  
  for(i in 1:length(oldSubs[["plan"]][["type"]])){
    
    if(is.null(oldSubs[["plan"]][["type"]][[i]])) next
    
    #paid counter
    #free counter
    if(grepl("paid",oldSubs[["plan"]][["type"]][[i]])){
      paidcnt <- paidcnt + 1
      time_paid <- time_paid + (oldSubs[["dateTimes"]][["end"]][[i]]-oldSubs[["dateTimes"]][["start"]][[i]])
      if(grepl("Alap", oldSubs[["plan"]][["name"]][[i]])){
        Alap <- Alap+1
      }
      if(grepl("Alma", oldSubs[["plan"]][["name"]][[i]])){
        Alma <- Alma+1
      }
      if(grepl("Dinnye", oldSubs[["plan"]][["name"]][[i]])){
        Dinnye <- Dinnye+1
      }
    }else if(grepl("free", oldSubs[["plan"]][["type"]][[i]]) || grepl("trial",oldSubs[["plan"]][["type"]][[i]])){
      freecnt <- freecnt + 1
      time_free <- time_free + (oldSubs[["dateTimes"]][["end"]][[i]]-oldSubs[["dateTimes"]][["start"]][[i]])
    }
  }
  
  if(subs2IsAct == TRUE){
    #browser()
    out <- as.list(c(personalData.email = email, 
                     subscription2.isActive = subs2IsAct, 
                     subscription2.dateTimes.start = subs2DateTimesStart,
                     subscription2.plan.type = subs2PlanType, 
                     subscription2.plan.internalId = subs2PlanId,
                     #oldSubscriptions = oldSubs,
                     oldSubscriptions.paidCount = paidcnt, 
                     oldSubscriptions.paidTime = time_paid, 
                     oldSubscriptions.freeCount= freecnt, 
                     oldSubscriptions.freeTime = time_free, 
                     oldSubscriptions.alapCount = Alap, 
                     oldSubscriptions.almaCount = Alma, 
                     oldSubscriptions.dinnyeCount = Dinnye, 
                     stringsAsFactors = FALSE))
  } else{
    #browser()
    out <- as.list(c(personalData.email = email, 
                     subscription2.isActive = subs2IsAct, 
                     subscription2.dateTimes.start = NA,
                     subscription2.plan.type = NA, 
                     subscription2.plan.internalId = NA,
                     #oldSubscriptions = oldSubs,
                     oldSubscriptions.paidCount = paidcnt, 
                     oldSubscriptions.paidTime = time_paid, 
                     oldSubscriptions.freeCount= freecnt, 
                     oldSubscriptions.freeTime = time_free, 
                     oldSubscriptions.alapCount = Alap, 
                     oldSubscriptions.almaCount = Alma, 
                     oldSubscriptions.dinnyeCount = Dinnye, 
                     stringsAsFactors = FALSE))
  }
  
  
  
  
  
}