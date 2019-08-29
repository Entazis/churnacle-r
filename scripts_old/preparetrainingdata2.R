
prepareTraningData2 <- function(){
  
  #RWNM <<- 1
  print("central preprocessing")
  #user_next <<- do.call(rbind, apply(central_users_flatten2, 1, preprocessFirstM1b))

  keep <- c("personalData.email")
  lapply(names(orange_users2), function(name){
    if(grepl("created_at",name)){
      keep <<- c(keep, name)
    } 
  })
  
  print("joining data")
  user_submissions <- orange_users2[keep]
  user_next_submissions <- left_join(user_next, user_submissions, by = c("email" = "personalData.email"))
  keep <- !duplicated(user_next_submissions[c("email","period")])
  user_next_submissions_clean <- user_next_submissions[keep,]
  
  print("preparing user_next_firstm3")
  RWNM <<- 1
  user_next_firstm3 <<- as.data.frame(do.call(rbind, apply(user_next_submissions_clean,1,preprocessFirstM2)))
  
  user_next_firstm3$email <<- sapply(user_next_firstm3$email, as.character)
  user_next_firstm3$paymenthandler <<- sapply(user_next_firstm3$paymenthandler, as.factor)
  user_next_firstm3$submissions <<- sapply(user_next_firstm3$submissions, as.integer)
  
  user_next_firstm3$paidnext[user_next_firstm3$paidnext %in% "TRUE"] <<- "yes"
  user_next_firstm3$paidnext[user_next_firstm3$paidnext %in% "FALSE"] <<- "no"
  user_next_firstm3$paidnext <<- sapply(user_next_firstm3$paidnext, as.factor)
  
  lapply(names(user_next_firstm3), function(column){
    if(grepl("session", column)){
      user_next_firstm3[,column] <<- sapply(user_next_firstm3[,column], as.numeric)
    }
  })

  lapply(names(user_next_firstm3), function(column){
    if(grepl("session", column)){
      colvals <- user_next_firstm3[,column]
      mod <- is.na(colvals)
      user_next_firstm3[mod, column] <<- 0
    }
  })
  
  '/
  user_next_slice <- user_next_firstm3
  #TODO: select only > 5 && < 2500
  
  sel <- is.na(user_next_slice_filtered[1:dim(user_next_slice_filtered)[2]]) |
    user_next_slice_filtered[1:dim(user_next_slice_filtered)[2]] < 5 |
    user_next_slice_filtered[1:dim(user_next_slice_filtered)[2]] > 2500
  sel[,1:4] <- FALSE
  
  user_next_slice_filtered2 <- user_next_slice_filtered
  user_next_slice_filtered2[sel] <- NA
  s2 <- rowSums(user_next_slice_filtered2[5:dim(user_next_slice_filtered2)[2]])
  c2 <- rowSums(!is.na(user_next_slice_filtered2[,5:dim(user_next_slice_filtered2)[2]]))
  avg2 <- s2/c2
  user_next_slice_filtered["sessionsum"] <- s2
  user_next_slice_filtered["sessioncnt"] <- c2
  user_next_slice_filtered["sessionavg"] <- avg2
  user_next_slice_filtered[is.nan(user_next_slice_filtered[,"sessionavg"]), "sessionavg"] <- 0
  /'
  print("finished!")
}