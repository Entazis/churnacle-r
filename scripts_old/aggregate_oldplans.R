ut <- users_table[, c(1, 6, 14, 16, 19, 20, 22, 27, 28, 29, 30, 38, 39, 40, 48, 51, 56, 59, 60, 61)]

plan_aggr_frame <- data.frame(id = character(0), subscription.old_plans.paidCount = integer(0), subscription.old_plans.paidTime = numeric(0), subscription.old_plans.freeCount = integer(0), subscription.old_plans.freeTime = numeric(0))

FUN2 <- function(){
  
}

FUN1 <- function(x){
  user_id <- x[[1]]
  oldsubs_list_t <- x[[7]]
  A <-0
  A1 <- 0
  A2 <- 0
  A3 <- 0
  A4 <- 0
  A5 <- 0
  D1 <- 0
  D2 <- 0
  Fr <- 0
  paidcnt <- 0
  freecnt <- 0
  time_paid <- 0
  time_free <- 0
  
  
  for(i in 1:length(oldsubs_list_t)){
    
    #paid counter
    if(oldsubs_list_t[[i]]["type"] == "paid"){
      paidcnt <- paidcnt + 1
      time_paid <- time_paid + (oldsubs_list_t[i][["end_datetime"]]-oldsubs_list_t[i][["start_datetime"]])
      if(oldsubs_list_t[[i]]["name"] == "Alap csomag (2.500 HUF)"){
        A <- A+1
      }
      if(oldsubs_list_t[[i]]["name"] == "Alma-1 csomag (5.850 HUF)"){
        A1 <- A1+1
      }
      if(oldsubs_list_t[[i]]["name"] == "Alma-1 csomag (9.850 HUF)"){
        A1 <- A1+1
      }
      if(oldsubs_list_t[[i]]["name"] == "Alma-2 csomag (7.850 HUF)"){
        A2 <- A2+1
      }
      if(oldsubs_list_t[[i]]["name"] == "Alma-3 csomag (5.850 HUF)"){
        A3 <- A3+1
      }
      if(oldsubs_list_t[[i]]["name"] == "Alma-4 csomag (3.850 HUF)"){
        A4 <- A4+1
      }
      if(oldsubs_list_t[[i]]["name"] == "Alma-5 csomag (1.850 HUF)"){
        A5 <- A5+1
      }
      if(oldsubs_list_t[[i]]["name"] == "Dinnye-2 csomag (19.850 HUF)"){
        D2 <- D2+1
      }
      if(oldsubs_list_t[[i]]["name"] == "Dinnye csomag (29.850 HUF)"){
        D1 <- D1+1
      }
    }
    
    #free counter
    if((oldsubs_list_t[[i]]["type"] == "free") || (oldsubs_list_t[[i]]["type"] == "trial")){
      freecnt <- freecnt + 1
      time_free <- time_free + (oldsubs_list_t[[i]]["end_datetime"]-oldsubs_list_t[[i]]["start_datetime"])
      if(oldsubs_list_t[[i]]["name"] == "3 Napos Próba (0 HUF)"){
        Fr <- Fr+1
      }
      if(oldsubs_list_t[[i]]["name"] == "7 Napos Próba (0 HUF)"){
        Fr <- Fr+1
      }
      if(oldsubs_list_t[[i]]["name"] == "Ingyenes (0 HUF)"){
        Fr <- Fr+1
      }
    }
  }
  
  user_oldplans_df <- data.frame(user_id, paidcnt, time_paid, freecnt, time_free)
  names(user_oldplans_df) <- c("id", "subscription.old_plans.paidCount", "subscription.old_plans.paidTime", "subscription.old_plans.freeCount", "subscription.old_plans.freeTime")
  
  plan_aggr_frame <- rbind(plan_aggr_frame, plan_aggr_frame)
}

apply(ut, 1, FUN1)

