
aggregateOrange2 <- function(x){
  
  orange_users_t <- as.data.frame(x)
  #print(paste("orange processing: ", x[["personalData.email"]]))
  
  keep <- c("personalData.email","locale.fullLocaleCode",
            "levelProgress.firstAssignment", "levelProgress.fifthAssignment",
            "levelProgress.finishFirstProject", "levelProgress.numberOfMissingTasks",
            "levelProgress.slackRegistration"
  )
  
  lapply(names(x), function(y){
    #browser()
    #if(grepl("projectProgress.progressMap",y)){
    #  keep <<- c(keep, y)
    #} else 
    #if(grepl("submissions",y)){
    #  keep <<- c(keep, y)
    #}
    if(grepl("created_at",y)){
      keep <<- c(keep, y)
    } 
    #else if(grepl("timeSpentOn",y)){
    #    keep <<- c(keep, y)
    #  }
  })

  as.data.frame(orange_users_t[keep])
  
}