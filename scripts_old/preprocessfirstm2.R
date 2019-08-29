
preprocessFirstM2 <- function(x){
  
  email <- x[["email"]]
  period <- as.integer(x["period"])
  start <- parse_date(x[["start_date"]])
  end <- parse_date(x[["end_date"]])
  handler <- x[["paymenthandler"]]
  
  if (grepl("TRUE", x[["paidnext"]])) paidnext <- TRUE
  else if (grepl("FALSE", x[["paidnext"]])) paidnext <- FALSE
  else paidnext <- NA
  
  print(paste("orange processing:", email))
  print(paste("rownumber: ", RWNM))
  RWNM <<- RWNM + 1
  
  submissions <- x[grepl(".created_at.", names(x))]
  submissions <- sort(submissions, na.last = TRUE)
  submCount <- 0
  sessNum <- 0
  sessSum <- 0
  
  sessNames <- paste("session", toString(1), sep = "")
  
  lapply(2:(length(submissions)-1), function(x){
    sessNames <<- c(sessNames, paste("session", toString(x), sep = ""))
  })
  sessions <- vector("list", length(sessNames))
  names(sessions) <- sessNames
  
  threshold_high <- 60*60*1.5
  threshold_low <- 5
  impaired <- 0
  paired <- 0
  
  ##Just the first paid period
  if(period < 2){
    if(sum(!is.na(submissions)) >= 2){
      
      lapply(2:(length(submissions[!is.na(submissions)])), function(s){
        
        submfor <- parse_date(submissions[s])
        submback <-parse_date(submissions[s-1])
        
        if(start < submfor && submfor < end){
          submCount <<- submCount + 1
          if(start < submback && submfor < end &&
             threshold_high > difftime(submfor,submback, units = "secs") &&
             threshold_low < difftime(submfor,submback, units = "secs")){
            sessNum <<- sessNum + 1
            sessSum <<- sessSum + as.numeric(difftime(submfor,submback, units = "secs"))
            sessions[paste("session", toString(submCount), sep = "")] <<- 
              as.numeric(difftime(submfor,submback, units = "secs"))
            
          }
          else{
            sessions[paste("session", toString(submCount), sep = "")] <<- 0
          }
        } 
        
      })
      
      avg <- sessSum/sessNum
      if(is.nan(avg))
        avg <- 0

      if(submCount > 0){
        lapply(1:submCount, function(s){
          if(sessions[paste("session", toString(s), sep = "")] == 0)
            sessions[paste("session", toString(s), sep = "")] <<- avg
        })
      }
      
      lapply((sessNum+1):length(sessions), function(n){
        sessions[paste("session", toString(n), sep = "")] <<- NA
      })
      
    } else{
      submCount <- sum(!is.na(submissions))
      lapply(1:length(sessions), function(n){
        sessions[paste("session", toString(n), sep = "")] <<- NA
      })
    }
    
    c("email" = email, "paymenthandler" = handler, 
      "submissions" = submCount, "paidnext" = paidnext,
      sessions)
  } else NULL
  
}