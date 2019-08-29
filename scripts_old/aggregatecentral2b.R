
#odsubscriptions, subscriptions2, transacations -> is paid next?

aggregateCentral2b <- function(x){
  
  email <- x[["authentication.email_address"]]
  oldSubs <- x[["oldSubscriptions"]]
  
  print(paste("central processing: ", email))
  print(paste("rownumber: ", RWNM))
  RWNM <<- RWNM + 1
  
  #☺user_round <- vector()
  user_next <- data.frame(email=character(), subscription=integer(), transaction=integer(),
                          period=integer(),
                          start_date=character(), end_date=character(), paidnext=logical(), 
                          stringsAsFactors=FALSE)
  
  subcnt2 <- 0
  i <- 1
  
  if(dim(oldSubs)[1] != 0){
    
    lapply(1:(dim(oldSubs)[1]), function(subcnt){
      if(length(oldSubs[[subcnt, "transactions"]]) == 0){
        NULL
      }else
      lapply(1:(dim(oldSubs[[subcnt, "transactions"]])[1]), function(transcnt){
        transdate <- oldSubs[[subcnt, "transactions"]][["transaction"]][["fulfillmentDateTime"]][transcnt]
        period <- oldSubs[subcnt, "plan"][["billingDetails"]][["periodInDays"]]
        
        start <- parse_date(transdate)
        end <- parse_date(transdate) + (period*24*60*60)
        
        user_next[i, ] <<- c(email, subcnt, transcnt, 
                             i,
                            as.character(start), as.character(end), 
                            (transcnt<dim(oldSubs[[subcnt, "transactions"]])[1]))
        i <<- i + 1
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
      
      start <- parse_date(transdate)
      end <- parse_date(transdate) + (period*24*60*60)
      
      if(transcnt < (dim(actSubTrans)[1])){paidnext <- TRUE}
      else {paidnext <- NA}
      
      user_next[i, ] <<- c(email, subcnt2, transcnt, 
                           i,
                          as.character(start), as.character(end), 
                          paidnext)
      i <<- i + 1
    })
    
  }
  
  user_next
  
}