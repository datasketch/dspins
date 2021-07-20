#' @export
dspin_remove <- function(slug, folder = NULL, bucket_id = NULL) {
  .Deprecated("dspin_delete")

  if(is.null(bucket_id)){
    stop("Need a bucket_id to remove pin")
  }

  if(is.null(folder)){
    stop("Need a folder to remove pin")
  }

  board <- board_name(bucket_id, folder)
  pin_remove(name = slug, board = board)
}
