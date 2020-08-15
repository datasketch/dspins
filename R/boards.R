

#' @export
board_name <- function(user_id){
  if(missing(user_id)) stop("Need a user_id")
  if(nchar(user_id) <= 23)
    stop("Need a correct user_id")
  if(grepl("[^A-Za-z0-9-]",user_id))
    stop("user_id can only contain letters, numbers and dashes")
  paste0("dskt-ch-", user_id)
}

#' @export
dspins_user_board_exists <- function(user_id){
  suppressMessages(x <- aws.s3::bucket_exists(board_name(user_id)))
  as.logical(x)
}

#' @export
dspins_is_board_connected <- function(user_id){
  board_name(user_id) %in% user_board_list_local()
}




#' @export
dspins_user_board_connect <- function(user_id){
  load_env()
  if(!dspins_user_board_exists(user_id)){
    message("User board does not exist")
    # new_bucket <- tryCatch(user_bucket_create(user_id), error=function(e) e, warning=function(w) w)
    new_bucket <- tryCatch(aws.s3::put_bucket(board_name(user_id), region = "us-east-1"),
                           error=function(e) e, warning=function(w) w)
    message("User board created: ", board_name(user_id))
    if(inherits(new_bucket,"error")){
      stop(new_bucket)
    }
  }
  nm <- board_name(user_id)
  if(!nm %in% user_board_list_local()){
    board_register_s3(name = nm, bucket = nm)
  }
  nm %in% user_board_list_local()
}


user_bucket_create <- function(user_id){
  # load_env()
  message("creating bucket: ", board_name(user_id))
  cat("hello")
  # aws.s3::put_bucket(board_name(user_id), region = "us-east-1")
                     # headers = list(`x-amz-acl` = "public-read"))
}

user_board_list_local <- function(){
  x <- pins::board_list()
  x[grepl("^dskt-ch-",x)]
}

user_board_list_remote <- function(){
  # load_env()
  x <- aws.s3::bucket_list_df()
  x <- x[[1]]
  x[grepl("^dskt\\.ch\\.",x)]
}

#' @export
load_env <- function(file = ".env"){
  if(nchar(Sys.getenv("AWS_ACCESS_KEY_ID")) == 0){
    dotenv::load_dot_env()
  }
}



