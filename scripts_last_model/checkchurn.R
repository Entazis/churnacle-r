
checkChurn <- function(){
  
  #check old endDates
  user_end <- user_next_all[,c("email", "endDate", "paidnext")]
  user_end <- user_end[order(user_end[,"email"],desc(user_end[,"endDate"])),]
  user_end <- user_end[!duplicated(user_end[,"email"]),]
  names(user_end) <- c("email", "endDate_new", "paidnext")
  
  user_end$email <- as.character(user_end$email)
  user_end$endDate_new <- parse_date(user_end$endDate_new)
  user_end$paidnext <- as.factor(user_end$paidnext)
  a_b_test$email <- as.character(a_b_test$email)
  a_b_test$test <- as.factor(a_b_test$test)
  a_b_test$saved <- as.logical(a_b_test$saved)
  a_b_test$endDate <- parse_date(a_b_test$endDate)
  
  churn_check <<- left_join(a_b_test, user_end, by="email")
  churn_check$saved <<- ifelse(churn_check$endDate<churn_check$endDate_new, TRUE, ifelse(churn_check$paidnext %in% "no", FALSE, NA))
  churn_check <<- churn_check[,c("email","test","saved","endDate","endDate_new","paidnext")]
  
  mod <- churn_check[!is.na(churn_check$saved),]
  
  all_ab <<- dim(mod)[1]
  still_live_ab <<- dim(churn_check[is.na(churn_check$saved),])[1]

  lost_a <<- dim(mod[mod$test %in% "A" & mod$saved == FALSE,])[1]
  saved_a <<- dim(mod[mod$test %in% "A" & mod$saved == TRUE,])[1]
  
  lost_b <<- dim(mod[mod$test %in% "B" & mod$saved == FALSE,])[1]
  saved_b <<- dim(mod[mod$test %in% "B" & mod$saved == TRUE,])[1]
  
}