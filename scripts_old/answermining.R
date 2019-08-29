
answermining <- function(){
  central_users_raw <<- jsonlite::stream_in(file(paste(data_path, "users_central.json",sep = "")), flatten=TRUE)
  orange_users_raw <<- jsonlite::stream_in(file(paste(data_path,"users_orange.json",sep = "")), flatten=TRUE)
  
  oldSubscriptions <- central_users_raw[1]
  f1 <- function(x){
    x1 <- as.data.frame(x)
    k1 <- names(x1)
  }
  apply(oldSubscriptions, 1, f1)
  
  #remove ids/email/name/...
  users_co <- left_join(central_users_raw, orange_users_raw, 
                        by = c("authentication.email_address" = "personalData.email"))
  
  keep <- !duplicated(users_co["authentication.email_address"])
  users_co <- users_co[keep,]
  
  keep <- names(users_co)[!grepl("\\$oid",names(users_co)) & 
                             !grepl("authentication",names(users_co)) &
                             !grepl("personal_data",names(users_co)) &
                             !grepl("address",names(users_co)) &
                             !grepl("_id",names(users_co)) &
                             !grepl("oken",names(users_co)) &
                             !grepl("invoic",names(users_co)) &
                             !grepl("subscriptionId",names(users_co)) &
                             !grepl("customerId",names(users_co)) &
                             !grepl("github",names(users_co)) &
                             !grepl("google",names(users_co)) &
                             !grepl("facebook",names(users_co)) &
                             !grepl("local",names(users_co)) &
                             !grepl("slack",names(users_co)) &
                             !grepl("personalData",names(users_co)) &
                             !grepl("uniqueId",names(users_co)) &
                             !grepl("subscriptionId",names(users_co)) &
                             !grepl("\\.y",names(users_co)) &
                             !grepl("__v",names(users_co)) &
                             !grepl("codes",names(users_co)) &
                             !grepl("handler\\.braintree",names(users_co))]
  users_co <- users_co[,keep]
  names(users_co) <- gsub("\\.x", "", names(users_co))
  
  #write.table(user_next_all, file=paste(data_path,"user_next_all.csv",sep = ""), quote=FALSE, sep=",", col.names = TRUE)
  #stream_out(users_co, file(paste(data_path, "users_co.json",sep = "")))
  users_co_1 <<- users_co[1:14000,]
  users_co_2 <- users_co[14001:28000,]
  users_co_3 <- users_co[28001:43096,]
  stream_out(users_co_1, file(paste(data_path, "users_co_1.json",sep = "")))
  stream_out(users_co_2, file(paste(data_path, "users_co_2.json",sep = "")))
  stream_out(users_co_3, file(paste(data_path, "users_co_3.json",sep = "")))
  
  #names(users_co[names(users_co)[!grepl("submissions",names(users_co)) & !grepl("projectProgress",names(users_co)) & !grepl("conceptProgress",names(users_co))]])
}