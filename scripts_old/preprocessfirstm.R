
preprocessFirstM1 <- function(x){
  
  browser()
  email <- x[["authentication.email_address"]]
  oldSubs <- x[["oldSubscriptions"]]
  
  print(paste("central processing: ", email))
  print(paste("rownumber: ", RWNM))
  RWNM <<- RWNM + 1
  
  user_next <- data.frame(email=character(), period=integer(), type=character(),
                          start_date=character(), end_date=character(), 
                          paymenthandler=character(), paidnext=logical(), 
                          stringsAsFactors=FALSE)
  
  subcnt2 <- 0
  i <- 1
  
  #filter plan type paid, first month
  
  if(dim(oldSubs)[1] != 0){
    
    lapply(1:(dim(oldSubs)[1]), function(subcnt){
      if(length(oldSubs[[subcnt, "transactions"]]) == 0){
        NULL
      }else
        lapply(1:(dim(oldSubs[[subcnt, "transactions"]])[1]), function(transcnt){
          
          type <- oldSubs[subcnt, "plan"][["type"]]
          
          if(type %in% "paid"){
            transdate <- oldSubs[[subcnt, "transactions"]][["transaction"]][["fulfillmentDateTime"]][transcnt]
            period <- oldSubs[subcnt, "plan"][["billingDetails"]][["periodInDays"]]
            handler <- oldSubs[[subcnt, "transactions"]][["handler"]][["processorName"]][transcnt]
            start <- parse_date(transdate)
            end <- parse_date(transdate) + (period*24*60*60)
            
            user_next[i, ] <<- c(email, i, type,
                                 as.character(start), as.character(end), 
                                 handler,
                                 (transcnt<dim(oldSubs[[subcnt, "transactions"]])[1]))
            i <<- i + 1
          }
          
        })
      subcnt2 <<- subcnt
    })
    
  }
  
  subcnt2 <- subcnt2 + 1
  
  if(x[["subscription2.isActive"]] == TRUE && 
     x[["subscription2.plan.type"]] == "paid"){
    
    actSubTrans <- x[["subscription2.transactions"]]
    
    lapply(1:(dim(actSubTrans)[1]), function(transcnt){
      
      transdate <- actSubTrans[["transaction"]][["fulfillmentDateTime"]][transcnt]
      period <- x[["subscription2.plan.billingDetails.periodInDays"]]
      
      type <- x[["subscription2.plan.type"]]
      handler <- actSubTrans[transcnt, "handler"][["processorName"]]
      
      start <- parse_date(transdate)
      end <- parse_date(transdate) + (period*24*60*60)
      
      if(transcnt < (dim(actSubTrans)[1])){paidnext <- TRUE}
      else {paidnext <- NA}
      
      user_next[i, ] <<- c(email, i, type, 
                           as.character(start), as.character(end), 
                           handler,
                           paidnext)
      i <<- i + 1
    })
    
  }
  
  user_next
}