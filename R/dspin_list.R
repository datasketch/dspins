
#' @export
dspin_list <- function(bucket_id){
  # in_data_txt <- pin_find(board = board_name(bucket_id))
  # if(length(pin_list_from_bucket) > length(in_data_txt$name )){
  #   message()
  # }
  l <-tryCatch(pin_find(board = "user.dskt.ch", extended = TRUE),
           error=function(e) e, warning=function(w) w)
  if(inherits(l,"error")){
    message("No pins in board\n Error: ",
            paste0(l$call[1], l$message[1], sep = "\n"))
    return()
  }
  # l %>% filter(str_detect(path, paste0("^", bucket_id, "/")))
  l %>% dplyr::filter(grepl(paste0("^", bucket_id, "/"), path))
}
