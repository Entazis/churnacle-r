
prepareData <- function(){

  RWNM <<- 1
  print("central preparing...")
  user_next <<- do.call(rbind, apply(central_users, 1, processCentral)) #prepareFirstMonth
  row.names(user_next) <<- 1:nrow(user_next)
  user_next <<- as.data.frame(user_next)
  user_next <<- user_next[!is.na(user_next$email),]
  user_next$paidnext <<- as.logical(user_next$paidnext)
  user_next$startDate <<- parse_date(user_next$startDate)
  user_next$endDate <<- parse_date(user_next$endDate)
  user_next$email <<- as.character(user_next$email)
  user_next$period <<- as.integer(user_next$period)
  user_next$paymentHandler <<- as.character(user_next$paymentHandler)
  user_next$planId <<- as.character(user_next$planId)
  user_next$cost <<- as.character(user_next$cost)
  user_next$spent <<- as.numeric(user_next$spent)
  user_next$locale <<- as.character(user_next$locale)
  user_next$hasSlack <<- as.logical(user_next$hasSlack)
  user_next$hasFacebook <<- as.logical(user_next$hasFacebook)
  user_next$hasGoogle <<- as.logical(user_next$hasGoogle)
  user_next$hasFreematerial <<- as.logical(user_next$hasFreematerial)
  user_next$coupon <<- as.character(user_next$coupon)
  user_next$utmSource <<- as.character(user_next$utmSource)
  user_next$utmMedium <<- as.character(user_next$utmMedium)
  user_next$utmCampaign <<- as.character(user_next$utmCampaign)
  user_next <<- user_next[order(user_next[,"startDate"]), ]

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
  user_next_orange_clean <<- user_next_orange[keep,]
  
  #collect lesson hashes
  submissions <<- names(user_next_orange_clean)[grepl("submissions",names(user_next_orange_clean))]
  submissions_split <- strsplit(submissions, ".", fixed = TRUE)
  lessonHashes <<- unique(unlist(submissions_split)[!grepl("submissions",unlist(submissions_split)) & 
                                                      !grepl("assignment",unlist(submissions_split))])
  lessonCnt <<- length(lessonHashes)
  assignmentCnt <<- length(submissions)
  
  print("orange preparing...")
  RWNM <<- 1
  #user_next_all <<- as.data.frame(do.call(rbind, apply(user_next_orange_clean,1,processOrange)))
  #user_next_all2 <<- as.data.frame(do.call(rbind, apply(user_next_orange_clean,1,processOrange2)))
  user_next_all3 <<- as.data.frame(do.call(rbind, apply(user_next_orange_clean,1,processOrange3)))

  user_next_all3$coupon[is.na(user_next_all3$coupon)] <<- "noinfo"
  user_next_all3$utmCampaign[is.na(user_next_all3$utmCampaign)] <<- "noinfo"
  user_next_all3$utmMedium[is.na(user_next_all3$utmMedium)] <<- "noinfo"
  user_next_all3$utmSource[is.na(user_next_all3$utmSource)] <<- "noinfo"
  
  user_next_all3[,grepl("submissions.",names(user_next_all3))] <<- 
    as.numeric(unlist(user_next_all3[,grepl("submissions.",names(user_next_all3))]))
  user_next_all3[,grepl("session",names(user_next_all3))] <<- 
    as.numeric(unlist(user_next_all3[,grepl("session",names(user_next_all3))]))
  user_next_all3[,grepl("lesson",names(user_next_all3))] <<- 
    as.numeric(unlist(user_next_all3[,grepl("lesson",names(user_next_all3))]))
  
  user_next_all3$paidnext <<- as.logical(user_next_all3$paidnext)
  user_next_all3$paidnext <<- ifelse(user_next_all3$paidnext, "yes", "no")
  
  user_next_all3$email <<- as.character(user_next_all3$email)
  user_next_all3$period <<- as.integer(user_next_all3$period)
  user_next_all3$endDate <<- parse_date(user_next_all3$endDate)
  user_next_all3$locale <<- as.factor(as.character(user_next_all3$locale))
  user_next_all3$planId <<- as.factor(as.character(user_next_all3$planId))
  user_next_all3$cost <<- as.numeric(user_next_all3$cost)
  user_next_all3$spent <<- as.numeric(user_next_all3$spent)
  user_next_all3$part1 <<- as.integer(user_next_all3$part1)
  user_next_all3$part2 <<- as.integer(user_next_all3$part2)
  user_next_all3$part3 <<- as.integer(user_next_all3$part3)
  user_next_all3$part4 <<- as.integer(user_next_all3$part4)
  user_next_all3$paymentHandler <<- as.factor(as.character(user_next_all3$paymentHandler))
  user_next_all3$submissions <<- as.integer(user_next_all3$submissions)
  user_next_all3$hasSlack <<- as.logical(user_next_all3$hasSlack)
  user_next_all3$hasFacebook <<- as.logical(user_next_all3$hasFacebook)
  user_next_all3$hasGoogle <<- as.logical(user_next_all3$hasGoogle)
  user_next_all3$hasFreematerial <<- as.logical(user_next_all3$hasFreematerial)
  user_next_all3$coupon <<- as.factor(as.character(user_next_all3$coupon))
  user_next_all3$utmSource <<- as.factor(as.character(user_next_all3$utmSource))
  user_next_all3$utmMedium <<- as.factor(as.character(user_next_all3$utmMedium))
  user_next_all3$utmCampaign <<- as.factor(as.character(user_next_all3$utmCampaign))
  user_next_all3$currency <<- as.factor(as.character(user_next_all3$currency))
  user_next_all3$duplicatedSubmissions <<- as.integer(user_next_all3$duplicatedSubmissions)
  user_next_all3$totalTimeSpentOnLessons <<- as.numeric(user_next_all3$totalTimeSpentOnLessons)
  
  '/
  user_next_all$coupon[is.na(user_next_all$coupon)] <<- "noinfo"
  user_next_all$utmCampaign[is.na(user_next_all$utmCampaign)] <<- "noinfo"
  user_next_all$utmMedium[is.na(user_next_all$utmMedium)] <<- "noinfo"
  user_next_all$utmSource[is.na(user_next_all$utmSource)] <<- "noinfo"
  user_next_all2$coupon[is.na(user_next_all2$coupon)] <<- "noinfo"
  user_next_all2$utmCampaign[is.na(user_next_all2$utmCampaign)] <<- "noinfo"
  user_next_all2$utmMedium[is.na(user_next_all2$utmMedium)] <<- "noinfo"
  user_next_all2$utmSource[is.na(user_next_all2$utmSource)] <<- "noinfo"
  
  user_next_all[,grepl("submissions.",names(user_next_all))] <<- 
    as.numeric(unlist(user_next_all[,grepl("submissions.",names(user_next_all))]))
  user_next_all2[,grepl("session",names(user_next_all2))] <<- 
    as.numeric(unlist(user_next_all2[,grepl("session",names(user_next_all2))]))
  user_next_all2[,grepl("lesson",names(user_next_all2))] <<- 
    as.numeric(unlist(user_next_all2[,grepl("lesson",names(user_next_all2))]))
  
  user_next_all$paidnext <<- as.logical(user_next_all$paidnext)
  user_next_all$paidnext <<- ifelse(user_next_all$paidnext, "yes", "no")
  user_next_all2$paidnext <<- as.logical(user_next_all2$paidnext)
  user_next_all2$paidnext <<- ifelse(user_next_all2$paidnext, "yes", "no")
  
  user_next_all$email <<- as.character(user_next_all$email)
  user_next_all$period <<- as.factor(as.integer(user_next_all$period))
  user_next_all$endDate <<- parse_date(user_next_all$endDate)
  user_next_all$locale <<- as.factor(as.character(user_next_all$locale))
  user_next_all$planId <<- as.factor(as.character(user_next_all$planId))
  user_next_all$cost <<- as.factor(as.character(user_next_all$cost))
  user_next_all$spent <<- as.numeric(user_next_all$spent)
  user_next_all$part1 <<- as.integer(user_next_all$part1)
  user_next_all$part2 <<- as.integer(user_next_all$part2)
  user_next_all$part3 <<- as.integer(user_next_all$part3)
  user_next_all$part4 <<- as.integer(user_next_all$part4)
  user_next_all$paymentHandler <<- as.factor(as.character(user_next_all$paymentHandler))
  user_next_all$submissions <<- as.integer(user_next_all$submissions)
  user_next_all$hasSlack <<- as.logical(user_next_all$hasSlack)
  user_next_all$hasFacebook <<- as.logical(user_next_all$hasFacebook)
  user_next_all$hasGoogle <<- as.logical(user_next_all$hasGoogle)
  user_next_all$hasFreematerial <<- as.logical(user_next_all$hasFreematerial)
  user_next_all$coupon <<- as.factor(as.character(user_next_all$coupon))
  user_next_all$utmSource <<- as.factor(as.character(user_next_all$utmSource))
  user_next_all$utmMedium <<- as.factor(as.character(user_next_all$utmMedium))
  user_next_all$utmCampaign <<- as.factor(as.character(user_next_all$utmCampaign))
  user_next_all$currency <<- as.factor(as.character(user_next_all$currency))
  user_next_all$duplicatedSubmissions <<- as.integer(user_next_all$duplicatedSubmissions)
  
  user_next_all2$email <<- as.character(user_next_all2$email)
  user_next_all2$period <<- as.factor(as.integer(user_next_all2$period))
  user_next_all2$endDate <<- parse_date(user_next_all2$endDate)
  user_next_all2$locale <<- as.factor(as.character(user_next_all2$locale))
  user_next_all2$planId <<- as.factor(as.character(user_next_all2$planId))
  user_next_all2$cost <<- as.factor(as.character(user_next_all2$cost))
  user_next_all2$spent <<- as.numeric(user_next_all2$spent)
  user_next_all2$part1 <<- as.integer(user_next_all2$part1)
  user_next_all2$part2 <<- as.integer(user_next_all2$part2)
  user_next_all2$part3 <<- as.integer(user_next_all2$part3)
  user_next_all2$part4 <<- as.integer(user_next_all2$part4)
  user_next_all2$paymentHandler <<- as.factor(as.character(user_next_all2$paymentHandler))
  user_next_all2$submissions <<- as.integer(user_next_all2$submissions)
  user_next_all2$hasSlack <<- as.logical(user_next_all2$hasSlack)
  user_next_all2$hasFacebook <<- as.logical(user_next_all2$hasFacebook)
  user_next_all2$hasGoogle <<- as.logical(user_next_all2$hasGoogle)
  user_next_all2$hasFreematerial <<- as.logical(user_next_all2$hasFreematerial)
  user_next_all2$coupon <<- as.factor(as.character(user_next_all2$coupon))
  user_next_all2$utmSource <<- as.factor(as.character(user_next_all2$utmSource))
  user_next_all2$utmMedium <<- as.factor(as.character(user_next_all2$utmMedium))
  user_next_all2$utmCampaign <<- as.factor(as.character(user_next_all2$utmCampaign))
  user_next_all2$currency <<- as.factor(as.character(user_next_all2$currency))
  user_next_all2$duplicatedSubmissions <<- as.integer(user_next_all2$duplicatedSubmissions)
  
  user_next_firstmonth <<- user_next_all[user_next_all$period==1,]
  user_next_firstmonth <<- user_next_firstmonth[,-which(names(user_next_firstmonth) %in% c("period"))]
  user_next_firstmonth2 <<- user_next_all2[user_next_all2$period==1,]
  user_next_firstmonth2 <<- user_next_firstmonth2[,-which(names(user_next_firstmonth2) %in% c("period"))]
  '
  
  print("finished!")
  
}