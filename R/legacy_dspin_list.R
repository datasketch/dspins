
#' @export
legacy_dspin_list <- function(folder = NULL, bucket_id = NULL){
  .Deprecated("dspin_list")
  if(is.null(bucket_id)){
    message("No bucket_id specified. Using 'user.dskt.ch' by default.")
    bucket_id <- "user"
  }

  if(is.null(folder)){
    stop("Need a folder to retrieve list of pins.")
  }

  boardname <- board_name(bucket_id, folder)

  board <- board_get(boardname)

  datatxt_refresh_index_ds(board)

  l <-tryCatch(pin_find(board = boardname, extended = TRUE),
           error=function(e) e, warning=function(w) w)
  if(inherits(l,"error")){
    message("No pins in board\n Error: ",
            paste0(l$call[1], l$message[1], sep = "\n"))
    return()
  }
  l
}
