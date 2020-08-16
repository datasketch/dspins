
#' @export
dspin_info <- function(name, bucket_id, extended = FALSE){
  pins::pin_info(name = name, board = board_name(bucket_id),
                 extended = extended, metadata = TRUE)
}
