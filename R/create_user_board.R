

#' @export
user_board_create <- function(bucket_id, env_file = ".env"){

  # load_env()

  if(!user_bucket_exists(bucket_id)){
    message("No bucket exists for this user. Creating bucket.")
    created <- user_bucket_create(bucket_id)
    message("Bucket created: ",paste0("dskt.ch.",bucket_id), created)
    if(!created)
      stop("Something wrong creating the bucket")
  }
  message("Registering board")
  x <- board_register_s3(name = paste0("dskt.ch.", bucket_id), bucket = paste0("dskt.ch.", bucket_id))
  message("Registered board: ", x)
  TRUE
}

user_bucket_exists <- function(username){
  # load_env()
  aws.s3::bucket_exists(paste0("dskt.ch.", username))
}


user_bucket_create <- function(username){
  # load_env()
  aws.s3::put_bucket(paste0("dskt.ch.", username),
                     region = "us-east-1"
                     #headers = list(`x-amz-acl` = "public-read")
                     )
}

