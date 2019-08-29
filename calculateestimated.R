

#FIXME
calculateEstimated <- function(){
  estimated_times <<- data.frame(lessonTitle = NA, lessonHash = NA, median = NA, mean = NA, variance = NA)

  sapply(submissions, function(x) {

    lesson_hash <- str_split_fixed(x,"\\.", 3)[,2]
    lesson_title <- ifelse(dim(projects_metadata[projects_metadata$hash %in% lesson_hash,]["title"])[1] == 1,
                              projects_metadata[projects_metadata$hash %in% lesson_hash,][["title"]], NA)

    mean <- mean(as.numeric(sort(unlist(user_next_all[user_next_all[,x] != 0,x]))))
    sd <- sd(as.numeric(sort(unlist(user_next_all[user_next_all[,x] != 0,x]))))
    median <- median(as.numeric(sort(unlist(user_next_all[user_next_all[,x] != 0,x]))))
    #quantile <- quantile(sort(user_next_all[user_next_all[,paste("lesson",x, sep = ".")] != 0,paste("lesson",x, sep = ".")]))
    
    med_min <- ceiling(median/60)
    mea_min <- ceiling(mean/60)
    sd_min <- ceiling(sd/60)
    lesson_time <- data.frame(lessonTitle = lesson_title, lessonHash = x, median = med_min, mean = mea_min, variance = sd_min)

    estimated_times <<- rbind(estimated_times, lesson_time)
    #estimated_times <<- estimated_times[complete.cases(estimated_times),]
  
  })
  
  write.table(estimated_times, file=paste(data_path,"estimated_times.csv",sep = ""),quote=FALSE,row.names = FALSE,col.names = TRUE,sep = ",")
  
  TRUE
}