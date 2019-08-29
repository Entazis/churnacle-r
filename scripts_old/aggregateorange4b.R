
#calc session times without time bounds, time between submissions

aggregateOrange4b <- function(x){
  
  email <- x[["email"]]
  start <- parse_date(x[["start_date"]])
  end <- parse_date(x[["end_date"]])
  subs <- x[["subscription"]]
  trans <- x[["transaction"]]
  period <- x[["period"]]
  
  if (grepl("TRUE", x[["paidnext"]])) paidnext <- TRUE
  else if (grepl("FALSE", x[["paidnext"]])) paidnext <- FALSE
  else paidnext <- NA
  
  if (grepl("TRUE", x[["levelProgress.slackRegistration"]])) slackReg <- TRUE
  else if (grepl("FALSE", x[["levelProgress.slackRegistration"]])) slackReg <- FALSE
  else slackReg <- NA
  
  if (grepl("TRUE", x[["levelProgress.firstAssignment"]])) firstass <- TRUE
  else if (grepl("FALSE", x[["levelProgress.firstAssignment"]])) firstass <- FALSE
  else firstass <- NA
  
  if (grepl("TRUE", x[["levelProgress.fifthAssignment"]])) fifthass <- TRUE
  else if (grepl("FALSE", x[["levelProgress.fifthAssignment"]])) fifthass <- FALSE
  else fifthass <- NA
  
  if (grepl("TRUE", x[["levelProgress.finishFirstProject"]])) firstproj <- TRUE
  else if (grepl("FALSE", x[["levelProgress.finishFirstProject"]])) firstproj <- FALSE
  else firstproj <- NA
  
  missingNum <- x[["levelProgress.numberOfMissingTasks"]]
  
  print(paste("orange processing:", email))
  print(paste("rownumber: ", RWNM))
  RWNM <<- RWNM + 1
  
  submissions <- x[grepl(".created_at.", names(x))]
  submissions <- sort(submissions, na.last = TRUE)
  submCount <- 0
  sessNum <- 0
  
  sessNames <- paste("session", toString(1), sep = "")
  
  lapply(2:(length(submissions)-1), function(x){
    sessNames <<- c(sessNames, paste("session", toString(x), sep = ""))
  })
  sessions <- vector("list", length(sessNames))
  names(sessions) <- sessNames
  #threshold_high <- 60*60*3
  threshold_low <- 1
  #impaired <- 0
  #paired <- 0
  
  #browser()
  
  if(sum(!is.na(submissions)) >= 2){
    
    lapply(2:(length(submissions[!is.na(submissions)])), function(s){
      
      submfor <- parse_date(submissions[s])
      submback <-parse_date(submissions[s-1])
      
      if(start < submfor && submfor < end) submCount <<- submCount + 1
      
      if(start < submback && 
         submfor < end  && 
         threshold_low < difftime(submfor,submback, units = "secs")){
          
          sessNum <<- sessNum + 1
          sessions[paste("session", toString(sessNum), sep = "")] <<- 
            difftime(submfor,submback, units = "secs")
      }
    })
    
    lapply((sessNum+1):length(sessions), function(n){
      sessions[paste("session", toString(n), sep = "")] <<- NA
    })
    
  } else{
    submCount <- sum(!is.na(submissions))
    
    lapply(1:length(sessions), function(n){
      sessions[paste("session", toString(n), sep = "")] <<- NA
    })
  }
  
  
  c("email" = email, "subscription" = subs, "transaction" = trans, 
    "period" = period, "paidnext" = paidnext, "submissionCnt" = submCount,
    "slack" = slackReg, "firstAssignment" = firstass, "fifthAssignment" = fifthass, 
    "firstProject" = firstproj, "missingTasks" = missingNum,
    sessions)
  
  
}