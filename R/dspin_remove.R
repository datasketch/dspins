#' @export
dspin_remove <- function(slug, bucket_id, board = "user.dskt.ch") {
  name <- paste0(bucket_id, "/", slug)

  pin_remove(name = name, board = board)
}
