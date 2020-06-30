
#' @export
pin_list <- function(user_id){
  # in_data_txt <- pin_find(board = board_name(user_id))
  # if(length(pin_list_from_bucket) > length(in_data_txt$name )){
  #   message()
  # }
  pin_find(board = board_name(user_id), extended = TRUE)

}




pin_list_from_bucket <- function(user_id){
  data_txt_bucket <- aws.s3::get_object("data.txt", board_name(user_id))
  data_txt <- rawToChar(data_txt_bucket)
  data_txt <- yaml::yaml.load(data_txt)
  bucket_files <- aws.s3::get_bucket_df(board_name(user_id))
  l <- strsplit(bucket_files$Key, "/")
  pins <- unlist(unique(lapply(l, `[`, 1)))
  pins[pins != "data.txt"]
}


