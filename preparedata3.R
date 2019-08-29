
prepareData3 <- function(){
  
  RWNM <<- 0
  print("central preparing...")
  user_next <- do.call(rbind, apply(central_users, 1, processCentral3))
  row.names(user_next) <- 1:nrow(user_next)
  user_next <- as.data.frame(user_next)
  user_next <- user_next[!is.na(user_next$email),]
  
  #default features
  user_next$email <- as.character(user_next$email)
  user_next$paidnext <- as.logical(user_next$paidnext)
  user_next$startDate <- parse_date(user_next$startDate)
  user_next$endDate <- parse_date(user_next$endDate)
  
  #categorical features
  user_next$period <- as.integer(user_next$period)
  user_next$paymentHandler <- as.character(user_next$paymentHandler)
  user_next$planId <- as.character(user_next$planId)
  user_next$locale <- as.character(user_next$locale)
  user_next$coupon <- as.character(user_next$coupon)
  user_next$utmSource <- as.character(user_next$utmSource)
  user_next$utmMedium <- as.character(user_next$utmMedium)
  user_next$utmCampaign <- as.character(user_next$utmCampaign)
  user_next$currency <- as.character(user_next$currency)
  
  #logical features
  user_next$hasSlack <- as.logical(user_next$hasSlack)
  user_next$hasFacebook <- as.logical(user_next$hasFacebook)
  user_next$hasGoogle <- as.logical(user_next$hasGoogle)
  user_next$hasFreematerial <- as.logical(user_next$hasFreematerial)
  
  #numerical features
  user_next$cost <- as.numeric(as.character(user_next$cost))
  user_next$spent <- as.numeric(user_next$spent)
  
  user_next <- user_next[order(user_next[,"startDate"]), ]
  
  #removing unnecessary data
  orange_users_p <- orange_users
  orange_users_p[is.na(orange_users_p$isAdmin),"isAdmin"] <- FALSE
  orange_users_p[is.na(orange_users_p$admin), "admin"] <- FALSE
  orange_users_p <- orange_users_p[!orange_users_p$admin & !orange_users_p$isAdmin,]
  keep <- c("personalData.email", "badges", names(orange_users_p)[grepl("submissions",names(orange_users_p))])
  orange_users_p <- orange_users_p[,keep]
  names(orange_users_p)[names(orange_users_p) == 'personalData.email'] <- 'email'  
  
  print("joining data...")
  user_next_orange <- left_join(user_next, orange_users_p, by = "email")
  keep <- !duplicated(user_next_orange[c("email","period")])
  user_next_orange_clean <- user_next_orange[keep,]
  
  #collect lesson hashes
  submissions <<- names(user_next_orange_clean)[grepl("submissions",names(user_next_orange_clean))]
  submissions_split <- strsplit(submissions, ".", fixed = TRUE)
  lessonHashes <<- unique(unlist(submissions_split)[!grepl("submissions",unlist(submissions_split)) & 
                                                      !grepl("assignment",unlist(submissions_split))])
  lessonCnt <<- length(lessonHashes)
  assignmentCnt <<- length(submissions)
  
  print("orange preparing...")
  RWNM <<- 0
  user_next_all3 <- as.data.frame(do.call(rbind, apply(user_next_orange_clean,1,processOrange3)))
  
  user_next_all3$coupon[is.na(user_next_all3$coupon)] <- "noinfo"
  user_next_all3$utmCampaign[is.na(user_next_all3$utmCampaign)] <- "noinfo"
  user_next_all3$utmMedium[is.na(user_next_all3$utmMedium)] <- "noinfo"
  user_next_all3$utmSource[is.na(user_next_all3$utmSource)] <- "noinfo"
  
  user_next_all3[,grepl("submissions.",names(user_next_all3))] <- 
    as.numeric(unlist(user_next_all3[,grepl("submissions.",names(user_next_all3))]))
  user_next_all3[,grepl("session",names(user_next_all3))] <- 
    as.numeric(unlist(user_next_all3[,grepl("session",names(user_next_all3))]))
  user_next_all3[,grepl("lesson",names(user_next_all3))] <- 
    as.numeric(unlist(user_next_all3[,grepl("lesson",names(user_next_all3))]))
  user_next_all3[,grepl("first",names(user_next_all3))] <- 
    as.numeric(unlist(user_next_all3[,grepl("first",names(user_next_all3))]))
  user_next_all3[,grepl("last",names(user_next_all3))] <- 
    as.numeric(unlist(user_next_all3[,grepl("last",names(user_next_all3))]))
  
  user_next_all3$paidnext <- as.logical(user_next_all3$paidnext)
  user_next_all3$paidnext <- ifelse(user_next_all3$paidnext, "yes", "no")
  user_next_all3$paidnext <- as.factor(user_next_all3$paidnext)
  
  #default
  user_next_all3$email <- as.character(user_next_all3$email)
  user_next_all3$endDate <- parse_date(user_next_all3$endDate)
  
  #categorical
  user_next_all3$paymentHandler <- as.factor(as.character(user_next_all3$paymentHandler))
  user_next_all3$locale <- as.factor(as.character(user_next_all3$locale))
  user_next_all3$planId <- as.factor(as.character(user_next_all3$planId))
  user_next_all3$coupon <- as.factor(as.character(user_next_all3$coupon))
  user_next_all3$utmSource <- as.factor(as.character(user_next_all3$utmSource))
  user_next_all3$utmMedium <- as.factor(as.character(user_next_all3$utmMedium))
  user_next_all3$utmCampaign <- as.factor(as.character(user_next_all3$utmCampaign))
  user_next_all3$currency <- as.factor(as.character(user_next_all3$currency))
  
  #numeric
  user_next_all3$period <- as.numeric(as.integer(user_next_all3$period))
  user_next_all3$cost <- as.numeric(user_next_all3$cost)
  user_next_all3$spent <- as.numeric(user_next_all3$spent)
  user_next_all3$part1 <- as.numeric(as.integer(user_next_all3$part1))
  user_next_all3$part2 <- as.numeric(as.integer(user_next_all3$part2))
  user_next_all3$part3 <- as.numeric(as.integer(user_next_all3$part3))
  user_next_all3$part4 <- as.numeric(as.integer(user_next_all3$part4))
  user_next_all3$submissions <- as.numeric(as.integer(user_next_all3$submissions))
  user_next_all3$submissionsOlders <- as.numeric(as.integer(user_next_all3$submissionsOlders))
  user_next_all3$totalTimeSpentOnLessons <- as.numeric(user_next_all3$totalTimeSpentOnLessons)
  user_next_all3$totalTimeSpentOnLessonsAndOlders <- as.numeric(user_next_all3$totalTimeSpentOnLessonsAndOlders)
  user_next_all3$duplicatedSubmissions <- as.numeric(as.integer(user_next_all3$duplicatedSubmissions))
  
  #logical
  user_next_all3$hasSlack <- as.logical(user_next_all3$hasSlack)
  user_next_all3$hasFacebook <- as.logical(user_next_all3$hasFacebook)
  user_next_all3$hasGoogle <- as.logical(user_next_all3$hasGoogle)
  user_next_all3$hasFreematerial <- as.logical(user_next_all3$hasFreematerial)
  
  user_next_all3 <<- user_next_all3[order(user_next_all3$endDate, user_next_all3$email),]
  
  print("finished!")
  paste("cells with NA value: ", sum(sapply(user_next_all3, function(x) sum(is.na(x)))), sep="")
  
}