
#odsubscriptions, subscriptions2, transacations -> is paid next?

aggregateCentral2 <- function(x){
  
  email <- x[["authentication.email_address"]]
  oldSubs <- x[["oldSubscriptions"]]
  
  print(paste("central processing: ", email))
  user_round <- vector()
  user_next <- data.frame(email=character(), subscription=integer(), transaction=integer(), 
                          start_date=character(), end_date=character(), paidnext=logical(), 
                          stringsAsFactors=FALSE)
  
  subcnt2 <- 0
  i <- 1
  
  if(dim(oldSubs)[1] != 0){
    
    for(subcnt in 1:(dim(oldSubs)[1])){
      
      if(length(oldSubs[[subcnt, "transactions"]]) == 0){
        next()
      }
      
      for(transcnt in 1:(dim(oldSubs[[subcnt, "transactions"]])[1])){
        
        transdate <- oldSubs[[subcnt, "transactions"]][["transaction"]][["fulfillmentDateTime"]][transcnt]
        period <- oldSubs[subcnt, "plan"][["billingDetails"]][["periodInDays"]]
        
        start <- parse_date(transdate)
        end <- parse_date(transdate) + (period*24*60*60)
        
        #user_round <- c(subscription=subcnt, transaction=transcnt, end_date=end, 
        #                paidnext=(transcnt<dim(oldSubs[[subcnt, "transactions"]])[1]))
        #print(user_round)
        
        user_next[i, ] <- c(email, subcnt, transcnt, 
                                              as.character(start), as.character(end), 
                                              (transcnt<dim(oldSubs[[subcnt, "transactions"]])[1]))
        i <- i + 1
        #print(user_next)
      }
      subcnt2 <- subcnt
    }
  
  }
  
  #if(email == "kpeterr2@gmail.com"){browser()}
  
  subcnt2 <- subcnt2 + 1
  
  if(x[["subscription2.isActive"]] == TRUE && 
     x[["subscription2.plan.type"]] == "paid"){
    
    #browser()
    #print(user_next)
    
    actSubTrans <- x[["subscription2.transactions"]]
    #View(actSubTrans)
    
      for(transcnt in 1:(dim(actSubTrans)[1])){
        
        transdate <- actSubTrans[["transaction"]][["fulfillmentDateTime"]][transcnt]
        period <- x[["subscription2.plan.billingDetails.periodInDays"]]
        
        start <- parse_date(transdate)
        end <- parse_date(transdate) + (period*24*60*60)
        
        if(transcnt < (dim(actSubTrans)[1])){paidnext <- TRUE}
        else {paidnext <- NA}
        
        user_round <- c(email, subcnt2, transcnt,
                        as.character(start), as.character(end),
                        paidnext)
        
        user_next[i, ] <- c(email, subcnt2, transcnt, 
                                              as.character(start), as.character(end), 
                                              paidnext)
        i <- i + 1
        #print(user_next)
      }
      
    
    
  }
  
  user_next
  
}