
#calc session times with time bounds

aggregateOrange4 <- function(x){
  
  email <- x[["email"]]
  #browser()
  start <- parse_date(x[["start_date"]])
  end <- parse_date(x[["end_date"]])
  subs <- x[["subscription"]]
  trans <- x[["transaction"]]
  if(email == users_submission_next4[1,1] && 
     subs ==  users_submission_next4[1,2] && 
     trans == users_submission_next4[1,3]) ROWCNT <<- 0
  
  if (grepl("TRUE", x[["paidnext"]])) paidnext <- TRUE
  else if (grepl("FALSE", x[["paidnext"]])) paidnext <- FALSE
  else paidnext <- NA
  
  if (grepl("TRUE", x[["levelProgress.slackRegistration"]])) slackReg <- TRUE
  else if (grepl("FALSE", x[["levelProgress.slackRegistration"]])) slackReg <- FALSE
  else slackReg <- NA
  
  ROWCNT <<- ROWCNT + 1
  print(paste("Processing:", email, "Rownumber:", ROWCNT))
  
  submissions <- x[grepl(".created_at.", names(x))]
  submissions <- sort(submissions, na.last = TRUE)
  submCount <- 0
  sessNum <- 0
  #print(paste("na submissions:" , sum(is.na(submissions))))
  #print(paste("real submissions: ", sum(!is.na(submissions))))
  
  sessNames <- paste("session", toString(1), sep = "")

  lapply(2:(length(submissions)-1), function(x){
    sessNames <<- c(sessNames, paste("session", toString(x), sep = ""))
  })
  sessions <- vector("list", length(sessNames))
  names(sessions) <- sessNames
  threshold_high <- 60*60*3
  threshold_low <- 1
  impaired <- 0
  paired <- 0
  
  #browser()
  if(sum(!is.na(submissions)) >= 2){
    
    '/
    for(s in 2:(length(submissions[!is.na(submissions)]))){

      if(parse_date(submissions[s]) > parse_date(start) &&
         parse_date(submissions[s]) < parse_date(end)) submCount <- submCount + 1
      
      if((parse_date(submissions[s])) < parse_date(end) && 
         (parse_date(submissions[s-1]) > parse_date(start)) &&
         threshold_low < difftime(parse_date(submissions[s]),parse_date(submissions[s-1]))){
        
        #browser()
        
        if(threshold_high > difftime(parse_date(submissions[s]),parse_date(submissions[s-1]))){
          sessNum <- sessNum + 1
          sessions[paste("session", toString(sessNum), sep = "")] <- 
          difftime(parse_date(submissions[s]),parse_date(submissions[s-1]))
        } else{ 
          sessNum <- sessNum + 1
          sessions[paste("session", toString(sessNum), sep = "")] <- NULL #out of threshold
        }
        
      }
      
    }
    /'
    lapply(2:(length(submissions[!is.na(submissions)])), function(s){
    
      submfor <- parse_date(submissions[s])
      submback <-parse_date(submissions[s-1])
      
      if(start < submfor && submfor < end) submCount <<- submCount + 1
      
      #FIXME: 
      if(start < submback && 
         submfor < end  && 
         threshold_low < difftime(submfor,submback, units = "secs")){
        #browser()
        
        if(threshold_high > difftime(submfor,submback, units = "secs")){
          #browser()
          sessNum <<- sessNum + 1
          sessions[paste("session", toString(sessNum), sep = "")] <<- difftime(submfor,submback, units = "secs")
          paired <<- paired + 1
        } else{ 
          sessNum <<- sessNum + 1
          #browser()
          sessions[paste("session", toString(sessNum), sep = "")] <<- -1 #out of threshold
          impaired <<- impaired + 1
        }
        
      }
    })
    '/
    for(n in (sessNum+1):length(sessions)){
      #browser()
      sessions[paste("session", toString(n), sep = "")] <- 0
    }
    /'
    lapply((sessNum+1):length(sessions), function(n){
      sessions[paste("session", toString(n), sep = "")] <<- 0
    })
    
    '/
    for(m in 1:length(sessions)){
      #browser()
      if(is.null(sessions[m])) sessions[m] <- 
        mean(sessions[lapply(sessions, !is.null) && (sessions > 0)])
    }
    /'
    
    #FIXMEEEEE
    
    #print(!as.logical(lapply(sessions, is.null)))
    #print(!as.logical(lapply(sessions, is.character)))
    #print(!as.logical(lapply(sessions, is.character)) & as.logical(sessions > 0))
    #browser()
    #temp <- sessions[as.logical(lapply(sessions, function(x){x>0}))]
    
    #print(sessions[!as.logical(lapply(sessions, is.character)) & as.logical(sessions > 0)])
    
    #mn <- mean(sessions[!as.logical(lapply(sessions, is.null)) & as.logical(sessions > 0)])
    
    print(paste("Submcnt:", submCount))
    print(paste("Paired: ", paired, "Impaired: ", impaired))
    
    #FIXME: 31: In mean.default(sesstimes) : argument is not numeric or logical: returning NA
    if(impaired > 0){
      #browser()
      issess <- lapply(sessions, function(x) {
        if(x > 0) TRUE
        else FALSE})
      sesstimes <- unname(unlist(sessions[unname(unlist(issess))]))
      mn <- mean(sesstimes)
      lapply(1:length(sessions), function(m){
        if(sessions[m] < 0) sessions[m] <<- mn
      })
    }
    
    
  
  } else{
    submCount <- sum(!is.na(submissions))
    '/
    for(n in 1:length(sessions)){
      
      sessions[paste("session", toString(n), sep = "")] <- 0
      
    }
    /'
    lapply(1:length(sessions), function(n){
      sessions[paste("session", toString(n), sep = "")] <<- 0
    })
  }
  
  names(email) <- "email"
  c("email" <- email, "slack" = slackReg, "subscription" = subs, 
    "transaction" = trans, "submissionCnt" = submCount, "paidnext" = paidnext, sessions)

  
}