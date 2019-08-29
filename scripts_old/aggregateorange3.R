
#calc all session times from orange data

aggregateOrange3 <- function(x){
  
  #browser()
  email <- x[["personalData.email"]]
  localeCode <- x[["locale.fullLocaleCode"]]
  print(paste("Processing:", email))
  
  submissions <- x[grepl("submissions", names(x))]
  submissions <- sort(submissions, na.last = TRUE)
  
  #joib central_next with orange_submissions3, 
  #left_join(central_next, orange_submissions, by.1=email, by.2=personalData.email)
  
  sessions <- list()
 
  if(sum(!is.na(submissions)) >= 2){
    for(s in 1:(sum(!is.na(submissions))-1)){
      sessions[paste("session", toString(s), sep = "")] <- 
        difftime(parse_date(submissions[s+1]),parse_date(submissions[s]))
    }
    for(n in sum(!is.na(submissions)):length(submissions)){
      sessions[paste("session", toString(n), sep = "")] <- NA
    }
  } else{
    for(n in 1:length(submissions)){
      sessions[paste("session", toString(n), sep = "")] <- NA
    }
  }
  
  as.list(c(email, sessions))
  #do call -> rbind -> apply -> data.table -> oranges_sessions
  #as.data.table(do.call(rbind, apply(orange_users_submissions3, 1, aggregateOrange3)))
  
}