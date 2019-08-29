
AGGR <- function(x){
  
  #browser()
  user_id <- x[[1]]
  oldsubs_list_t <- as.list(x[[7]])
  #str(oldsubs_list_t)
  #print(oldsubs_list_t[["type"]][[1]])
  Alap <-0
  Alma <- 0
  Dinnye <- 0
  paidcnt <- 0
  freecnt <- 0
  time_paid <- 0
  time_free <- 0
  i <- 0
  
  for(i in 1:length(oldsubs_list_t[["type"]])){
    
    if(is.null(oldsubs_list_t[["type"]][[i]])) next
    
    #paid counter
    #free counter
    if(grepl("paid",oldsubs_list_t[["type"]][[i]])){
      paidcnt <- paidcnt + 1
      time_paid <- time_paid + (oldsubs_list_t[["end_datetime"]][[i]]-oldsubs_list_t[["start_datetime"]][[i]])
      if(grepl("Alap", oldsubs_list_t[["name"]][[i]])){
        Alap <- Alap+1
      }
      if(grepl("Alma", oldsubs_list_t[["name"]][[i]])){
        Alma <- Alma+1
      }
      if(grepl("Dinnye", oldsubs_list_t[["name"]][[i]])){
        Dinnye <- Dinnye+1
      }
    }else if(grepl("free", oldsubs_list_t[["type"]][[i]]) || grepl("trial",oldsubs_list_t[["type"]][[i]])){
      freecnt <- freecnt + 1
      time_free <- time_free + (oldsubs_list_t[["end_datetime"]][[i]]-oldsubs_list_t[["start_datetime"]][[i]])
    }
  }
  
  temp2 <- data.frame(id = user_id, subscription.old_plans.paidCount = paidcnt, subscription.old_plans.paidTime = time_paid, subscription.old_plans.freeCount= freecnt, subscription.old_plans.freeTime = time_free, subscription.old_plans.alapCount = Alap, subscription.old_plans.almaCount = Alma, subscription.old_plans.dinnyeCount = Dinnye, stringsAsFactors = FALSE)
  #aggr_frame <- rbind(aggr_frame, data.frame(id = user_id, subscription.old_plans.paidCount = paidcnt, subscription.old_plans.paidTime = time_paid, subscription.old_plans.freeCount= freecnt, subscription.old_plans.freeTime = time_free, subscription.old_plans.alapCount = Alap, subscription.old_plans.almaCount = Alma, subscription.old_plans.dinnyeCount = Dinnye)) 
  #print(dim(aggr_frame))
  #browser()
}  
  
  #names(user_oldplans_df) <- c("id", "subscription.old_plans.paidCount", "subscription.old_plans.paidTime", "subscription.old_plans.freeCount", "subscription.old_plans.freeTime", "subscription.old_plans.alapCount", "subscription.old_plans.almaCount", "subscription.old_plans.dinnyeCount")
  
  #apply(ut, 1, AGGR)