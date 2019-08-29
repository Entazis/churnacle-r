#ut <- users_table[, c(1, 6, 14, 16, 19, 20, 22, 27, 28, 29, 30, 38, 39, 40, 48, 51, 56, 59, 60, 61)]

#plan_aggr_frame <- data.frame(id = character(0), subscription.old_plans.paidCount = integer(0), subscription.old_plans.paidTime = numeric(0), subscription.old_plans.freeCount = integer(0), subscription.old_plans.freeTime = numeric(0), subscription.old_plans.alapCount = integer(0), subscription.old_plans.almaCount = integer(0), subscription.old_plans.dinnyeCount = integer(0))

FUN1 <- function(x){
  user_id <- x[[1]]
  oldsubs_list_t <- x[[7]]
  Alap <-0
  Alma <- 0
  Dinnye <- 0
  D2 <- 0
  Fr <- 0
  paidcnt <- 0
  freecnt <- 0
  time_paid <- 0
  time_free <- 0
  
  temp<-length(oldsubs_list_t)
  
  #my_seq <- seq(1, length(oldsubs_list_t), 1)
  my_seq <- seq(1, 5, 1)
  
  for(i in 1:5){
    
    #paid counter
    if(grepl("paid",oldsubs_list_t[[i]]["type"])){
      paidcnt <- paidcnt + 1
      time_paid <- time_paid + (oldsubs_list_t[[i]]["end_datetime"]-oldsubs_list_t[[i]]["start_datetime"])
      if(grepl("Alap", oldsubs_list_t[[i]]["name"])){
        Alap <- Alap+1
      }
      if(grepl("Alma", oldsubs_list_t[[i]]["name"])){
        Alma <- Alma+1
      }
      if(grepl("Dinnye", oldsubs_list_t[[i]]["name"])){
        Dinnye <- Dinnye+1
      }
    }
    
    #free counter
    if(grepl("free", oldsubs_list_t[[i]]["type"]) || grepl("trial",oldsubs_list_t[[i]]["type"])){
      freecnt <- freecnt + 1
      time_free <- time_free + (oldsubs_list_t[[i]]["end_datetime"]-oldsubs_list_t[[i]]["start_datetime"])
    }
  }
  
  class(time_paid)
  plan_aggr_frame <- rbind(plan_aggr_frame, data.frame(id = user_id, subscription.old_plans.paidCount = paidcnt, subscription.old_plans.paidTime = time_paid, subscription.old_plans.freeCount= freecnt, subscription.old_plans.freeTime = time_free, subscription.old_plans.alapCount = Alap, subscription.old_plans.almaCount = Alma, subscription.old_plans.dinnyeCount = Dinnye)) 
  #names(user_oldplans_df) <- c("id", "subscription.old_plans.paidCount", "subscription.old_plans.paidTime", "subscription.old_plans.freeCount", "subscription.old_plans.freeTime", "subscription.old_plans.alapCount", "subscription.old_plans.almaCount", "subscription.old_plans.dinnyeCount")
  
}

apply(ut, 1, FUN1)
