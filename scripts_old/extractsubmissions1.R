
#extracts submissions created.at$date times from orange data

extractSubmissions1 <- function(x){
  
  orange_users_t <- as.list(x)
  #browser()
  print(paste("orange processing: ", x[["personalData.email"]]))
  
  keep <- c("personalData.email",
            "levelProgress.slackRegistration")
  
  lapply(names(x), function(y){
    if(grepl("created_at",y)){
      keep <<- c(keep, y)
    } 
  })
  
  #print(keep)
  orange_users_t[keep]
  
}