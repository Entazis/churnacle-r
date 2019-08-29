
aggregateOrange <- function(x){
  
  browser()
  
  #key for join
  print(paste("orange processing: ", x[["personalData.email"]]))
  
  keep <- c("personalData.email","locale.fullLocaleCode",
            "levelProgress.firstAssignment", "levelProgress.fifthAssignment",
            "levelProgress.finishFirstProject", "levelProgress.numberOfMissingTasks",
            "levelProgress.slackRegistration"
            )
  
  lapply(names(x), function(y){
    #browser()
    if(grepl("projectProgress.progressMap",y)){
      keep <<- c(keep, y)
    } else if(grepl("submissions",y)){
      keep <<- c(keep, y)
    }
  })
  
  browser()
  
  x2 <- x[keep]
  df <- data.frame(as.list(x2))
  
  browser()
  
  lapply(levels(projectNames), function(x){
    #browser()
    query1 <- paste("projectProgress.progressMap", x, 
                    "isStarted", sep=".")
    query2 <- paste("projectProgress.progressMap", x, 
                    "isFinished", sep=".")
    if(query1 %in% names(x)) {ppProjects[[paste(x, "isStarted", sep = ".")]] <<- x[[query1]]}
    if(query2 %in% names(x)) {ppProjects[[paste(x, "isFinished", sep = ".")]] <<- x[[query2]]}
  })
  
  browser()
  
  #projectProgress.progressMap.<project_name>.lessons.<lesson_hash>.
  
  lapply(levels(projectNames), function(x){
    lapply(levels(lessonHashes), function(y){
      #browser()
      query1 <- paste("projectProgress.progressMap", x, 
                      "lessons", y, "isStarted", sep=".")
      query2 <- paste("projectProgress.progressMap", x, 
                      "lessons", y, "isFinished", sep=".")
      if(query1 %in% names(x)){ppProjectsLessons[[paste(x, y, "isStarted", sep = ".")]] <<- x[[query1]]}
      if(query2 %in% names(x)){ppProjectsLessons[[paste(x, y, "isFinished", sep = ".")]] <<- x[[query2]]}
    })
  })
  
  
  browser()
  #FIXME: assNum

  lapply(levels(projectNames), function(x){
    lapply(levels(lessonHashes), function(y){
      lapply(1:15, function(z){
        query1 <- paste("projectProgress.progressMap.", x, 
                        ".lessons.", y, ".assignments.assignment", z, sep="")
        query2 <- paste("projectProgress.progressMap.", x, 
                        ".lessons.", y, ".assignments.assignment0", z, sep="")
        query3 <- paste("projectProgress.progressMap.", x, 
                        ".lessons.", y, ".assignments.assignment00", z, sep="")
        ppProjectsLessonsAssignments[[paste(x, y, 
                                            z, sep = ".")]] <<- ifelse(
                                              query1 %in% names(x), x[[query1]], ifelse(
                                                query2 %in% names(x), x[[query2]], ifelse(
                                                  query3 %in% names(x), x[[query3]], NA
                                                )
                                              )
                                            )
      })
    })
  })

  browser()
  Submissions <- list()
  
  lapply(levels(lessonHashes), function(y){
    lapply(1:20, function(z1){
      lapply(1:20, function(z2){
        query1a <- paste("submissions.", lesson_hash, 
                         ".assignment", assNum1, ".", assNum2, ".", "created_at.$date", sep="")
        query1b <- paste("submissions.", lesson_hash, 
                         ".assignment0", assNum1, ".", assNum2, ".", "created_at.$date", sep="")
        query1c <- paste("submissions.", lesson_hash, 
                         ".assignment00", assNum1, ".", assNum2, ".", "created_at.$date", sep="")
        Submissions[[paste("submissions", lesson_hash, "assignment",
                           assNum1, ".", assNum2, ".",
                           "created_at.$date", sep="")]] <<- ifelse(
                             query1a %in% names(x), x[[query1a]], ifelse(
                               query1b %in% names(x), x[[query1b]], ifelse(
                                 query1c %in% names(x), x[[query1c]], NA
                               )
                             )
                           )
        query2a <-paste("submissions.", lesson_hash, 
                        ".assignment", assNum1, ".", assNum2, ".", "isSkipped", sep="")
        query2b <-paste("submissions.", lesson_hash, 
                        ".assignment0", assNum1, ".", assNum2, ".", "isSkipped", sep="")
        query2c <-paste("submissions.", lesson_hash, 
                        ".assignment00", assNum1, ".", assNum2, ".", "isSkipped", sep="")
        Submissions[[paste("submissions", lesson_hash, "assignment",
                           assNum1, ".", assNum2, ".",
                           "isSkipped", sep="")]] <<- ifelse(
                             query2a %in% names(x), x[[query2a]], ifelse(
                               query2b %in% names(x), x[[query2b]], ifelse(
                                 query2c %in% names(x), x[[query2c]], NA
                               )
                             )
                           )
        query3a <-paste("submissions.", lesson_hash, 
                        ".assignment", assNum1, ".", assNum2, ".", "value", sep="")
        query3b <-paste("submissions.", lesson_hash, 
                        ".assignment0", assNum1, ".", assNum2, ".", "value", sep="")
        query3c <-paste("submissions.", lesson_hash, 
                        ".assignment00", assNum1, ".", assNum2, ".", "value", sep="")
        Submissions[[paste("submissions", lesson_hash, "assignment",
                           assNum1, ".", assNum2, ".", 
                           "value", sep="")]] <<- ifelse(
                             query3a %in% names(x), x[[query3a]], ifelse(
                               query3b %in% names(x), x[[query3b]], ifelse(
                                 query3c %in% names(x), x[[query3c]], NA
                               )
                             )
                           )
        query4a <-paste("submissions.", lesson_hash, 
                        ".assignment", assNum1, ".", assNum2, ".", "timeSpentOnAssignmentInSeconds", sep="")
        query4b <-paste("submissions.", lesson_hash, 
                        ".assignment0", assNum1, ".", assNum2, ".", "timeSpentOnAssignmentInSeconds", sep="")
        query4c <-paste("submissions.", lesson_hash, 
                        ".assignment00", assNum1, ".", assNum2, ".", "timeSpentOnAssignmentInSeconds", sep="")
        Submissions[[paste("submissions", lesson_hash, "assignment",
                           assNum1, ".", assNum2, ".", 
                           "timeSpentOnAssignmentInSeconds", sep="")]] <<- ifelse(
                             query4a %in% names(x), x[[query4a]], ifelse(
                               query4b %in% names(x), x[[query4b]], ifelse(
                                 query4c %in% names(x), x[[query4c]], NA
                               )
                             )
                           )        
        
      })
    })
  })
  
  browser()
  
  
  
}