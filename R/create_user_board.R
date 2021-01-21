

#' @export
user_board_create <- function(bucket_id, env_file = ".env"){

  # load_env()

  if(!dspins_user_board_exists(bucket_id)){
    message("No folder exists for this user. Creating folder.")
    created <- user_folder_create(bucket_id)
    message("Folder created: ", bucket_id,". ", created)
    if(!created)
      stop("Something wrong creating the folder")
  }
  # message("Registering board")
  # x <- board_register_s3(name = paste0("dskt.ch.", bucket_id), bucket = paste0("dskt.ch.", bucket_id))
  # message("Registered board: ", x)
  TRUE
}


user_folder_create <- function(username){
  # load_env()
  aws.s3::put_folder(username,
                     bucket = "user.dskt.ch"
  )
}

