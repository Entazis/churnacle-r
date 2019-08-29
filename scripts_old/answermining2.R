
answermining2 <- function(){
  #central_users_raw <<- jsonlite::stream_in(file(paste(data_path, "users_central.json",sep = "")))
  #orange_users_raw <<- jsonlite::stream_in(file(paste(data_path,"users_orange.json",sep = "")))

  #CentralUser:
  #authentication - ok
  #personal_data - ok
  #invoicing - ok
  #codes - törölni
  #_id
  drops <- c("personal_data", 
             "invoicing", "invoicing_data", "invoices", 
             "codes", "handler.braintree", "_id",
             "uniqueId", "invoice",
             "handler._id.$oid", "handler.braintree", "transaction._id.$oid",
             "subscription", "old_plans",
             "slack", "facebook", "google", "github",
             "address")
  central_u <- central_users_raw[ , !(names(central_users_raw) %in% drops)]
  
  #handler.braintree - törölni (subscription2-ben és oldSubscription-ben is van)
  #subscription2
  central_u_s2 <- central_u[["subscription2"]]
  central_u_s2 <- central_u_s2[ , !(names(central_u_s2) %in% drops)]
  #subscription2 transactions
  central_u_s2_t <- central_u_s2[["transactions"]]
  central_u_s2_t <- lapply(central_u_s2_t, function(t){
    if(is.null(dim(t))){
      return(NULL)
    }
    tr <- t[ , !(names(t) %in% drops)]
    tr_h <- tr[["handler"]][ , !(names(tr[["handler"]]) %in% c("_id", "braintree"))]
    tr[["handler"]] <- tr_h
    tr_t_c <- tr[["transaction"]][["currency"]][ , !(names(tr[["transaction"]][["currency"]]) %in% c("_id"))]
    tr[["transaction"]][["currency"]] <- tr_t_c
    tr
  })
  central_u_s2[["transactions"]] <- central_u_s2_t
  central_u[["subscription2"]] <- central_u_s2
  
  #oldSubscriptions
  central_u_os <- central_u[["oldSubscriptions"]]
  central_u_os <- lapply(central_u_os, function(s){
    sr <- s[ , !(names(s) %in% drops)]
    sr_t <- sr[["transactions"]]
    sr_t <- lapply(sr_t, function(t){
      if(is.null(dim(t))){
        return(NULL)
      }
      tr <- t[ , !(names(t) %in% drops)]
      tr_h <- tr[["handler"]][ , !(names(tr[["handler"]]) %in% c("_id", "braintree"))]
      tr[["handler"]] <- tr_h
      tr_t_c <- tr[["transaction"]][["currency"]][ , !(names(tr[["transaction"]][["currency"]]) %in% c("_id"))]
      tr[["transaction"]][["currency"]] <- tr_t_c
      tr
    })
    
    sr[["transactions"]] <- sr_t
    sr[["plan"]] <- sr[["plan"]][ , !(names(sr[["plan"]]) %in% drops)]
    sr_p_c <- sr[["plan"]][["currency"]][ , !(names(sr[["plan"]][["currency"]]) %in% c("_id"))]
    sr[["plan"]][["currency"]] <- sr_p_c
    sr_p_dft <- sr[["plan"]][["defaultTransactionHandler"]][ , !(names(sr[["plan"]][["defaultTransactionHandler"]]) %in% c("_id"))]
    sr[["plan"]][["defaultTransactionHandler"]] <- sr_p_dft
    sr
  })
  central_u[["oldSubscriptions"]] <- central_u_os
  central_u[["learning"]] <- central_u[["learning"]][ , !(names(central_u[["learning"]]) %in% "_id")]
  central_u[["locale"]] <- central_u[["locale"]][ , !(names(central_u[["locale"]]) %in% "_id")]
  central_u[["locale"]] <- central_u[["locale"]][ , !(names(central_u[["locale"]]) %in% "_id")]
  central_u[["locale"]][["baseCurrency"]] <- central_u[["locale"]][["baseCurrency"]][ , !(names(central_u[["locale"]][["baseCurrency"]]) %in% "_id")]
  central_u[["locale"]][["customFormats"]] <- central_u[["locale"]][["customFormats"]][ , !(names(central_u[["locale"]][["customFormats"]]) %in% "_id")]
  central_u[["curriculumPermissions"]] <- central_u[["curriculumPermissions"]][ , !(names(central_u[["curriculumPermissions"]]) %in% "_id")]
  central_u[["referral"]] <- central_u[["referral"]][ , !(names(central_u[["referral"]]) %in% "_id")]
  central_u[["campaign"]] <- central_u[["campaign"]][ , !(names(central_u[["campaign"]]) %in% "_id")]
  central_u[["funnelEvents"]] <- central_u[["funnelEvents"]][ , !(names(central_u[["funnelEvents"]]) %in% "_id")]
  
  #OrangeUser:
  #slack - ok
  #facebook - ok
  #google - ok
  #github - ok
  #personalData - ok
  #codes - törölni
  #handler.braintree - törölni (subscription2-ben)
  #_id
  orange_u <- orange_users_raw[ , !(names(orange_users_raw) %in% drops)]
  orange_u <- orange_u[ , !(names(orange_u) %in% c("subscription2","campaign", "__v", 
                                                   "curriculumPermissions", "referral", "locale",
                                                   "local"))]

  #join
  central_u_flat <- flatten(central_u, recursive = FALSE)
  orange_u_flat <- flatten(orange_u, recursive = FALSE)
  
  #users_co_flat <- left_join(central_u_flat, orange_u_flat, 
  #                           by = c("authentication.email_address" = "personalData.email"))
  users_co_flat2 <- merge(central_u_flat, orange_u_flat, 
                          by.x=c("authentication.email_address"),
                          by.y=c("personalData.email"))
  
  keep <- !duplicated(users_co_flat2["authentication.email_address"])
  users_co_flat2 <- users_co_flat2[keep,]
  
  users_co_flat2 <- users_co_flat2[ , !(names(users_co_flat2) %in% 
                                          c("authentication.email_address", "authentication.slack_user_id",
                                            "authentication.facebook_user_id", "authentication.is_admin",
                                            "authentication.is_active", "authentication.google_user_id",
                                            "authentication._id", "personalData.givenName",
                                            "personalData.initials", "personalData.familyName",
                                            "personalData.fullName", "personalData._id"))]
  
  users_co_1 <<- users_co_flat2[1:16500,]
  users_co_2 <- users_co_flat2[16501:33000,]
  users_co_3 <- users_co_flat2[33001:49226,]
  stream_out(users_co_1, file(paste(data_path, "users_co_1.json",sep = "")))
  stream_out(users_co_2, file(paste(data_path, "users_co_2.json",sep = "")))
  stream_out(users_co_3, file(paste(data_path, "users_co_3.json",sep = "")))
  
}