
readFiles <- function(){
  orange_users <<- jsonlite::stream_in(file(paste(data_path,"users_orange.json",sep = "")), flatten=TRUE)
  central_users <<- jsonlite::stream_in(file(paste(data_path, "users_central.json",sep = "")), flatten=TRUE)
}