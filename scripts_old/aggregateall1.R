
aggregateAll <- function(orange){
  
  browser()
  #personalData.email 
  #subscription2.isActive 
  #subscription2.dateTimes.start
  #subscription2.plan.type 
  #subscription2.plan.internalId
  #oldSubscriptions.paidCount 
  #oldSubscriptions.paidTime 
  #oldSubscriptions.freeCount
  #oldSubscriptions.freeTime
  #oldSubscriptions.alapCount
  #oldSubscriptions.almaCount
  #oldSubscriptions.dinnyeCount
  
  #print("aggregating central data...")
  #users_central_aggr <- do.call("rbind", apply(central, 1, aggregateCentral))
  #users_central_aggr
  
  print("aggregating orange data...")
  users_orange_aggr <- apply(orange, 1, aggregateOrange)
  users_orange_aggr
  
}