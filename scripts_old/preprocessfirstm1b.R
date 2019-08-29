
#central aggregating

preprocessFirstM1b <- function(x){
  
  email <- x[["authentication.email_address"]]
  oldSubs <- x[grepl("oldSubscriptions", names(x))]
  subs2 <- x[grepl("subscription2", names(x))]
  
  print(paste("central processing: ", email))
  print(paste("rownumber: ", RWNM))
  RWNM <<- RWNM + 1
  
  user_next <- data.frame(email=character(), period=integer(),
                          start_date=character(), end_date=character(), 
                          paymenthandler=character(), paidnext=logical(), 
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
          paidnext <- !is.na(oldSubs[paste("oldSubscriptions",s,"transactions",t+1,"_id.$oid", sep = ".")])
          
          user_next[i, ] <<- c(email, i,
                               as.character(start), as.character(end), 
                               handler,
                               paidnext)
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
          user_next[i, ] <<- c(email, i,
                               as.character(start), as.character(end), 
                               handler,
                               paidnext)
        }
        else{
          user_next[i, ] <<- c(email, i,
                               as.character(start), as.character(end), 
                               handler,
                               NA)
        }

        i <<- i + 1
      }
    })
  }
  
  user_next
}