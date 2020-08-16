
#' @export
dspin_list <- function(bucket_id){
  # in_data_txt <- pin_find(board = board_name(bucket_id))
  # if(length(pin_list_from_bucket) > length(in_data_txt$name )){
  #   message()
  # }
  l <-tryCatch(pin_find(board = board_name(bucket_id), extended = TRUE),
           error=function(e) e, warning=function(w) w)
  if(inherits(l,"error")){
    message("No pins in board\n Error: ",
            paste0(l$call[1], l$message[1], sep = "\n"))
    return()
  }
  l
}




pin_list_from_bucket <- function(bucket_id){
  data_txt_bucket <- aws.s3::get_object("data.txt", board_name(bucket_id))
  data_txt <- rawToChar(data_txt_bucket)
  data_txt <- yaml::yaml.load(data_txt)
  bucket_files <- aws.s3::get_bucket_df(board_name(bucket_id))
  l <- strsplit(bucket_files$Key, "/")
  pins <- unlist(unique(lapply(l, `[`, 1)))
  pins[pins != "data.txt"]
}


