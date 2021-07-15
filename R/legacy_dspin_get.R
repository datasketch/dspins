#' @export
dspin_get <- function(name, board = NULL, cache = TRUE, extract = NULL, version = NULL, files = FALSE, signature = NULL, ...) {
  .Deprecated("pin_read")
  args <- list(...)
  folder <- args$folder
  bucket_id <- args$bucket_id

  if(is.null(bucket_id)){
    message("No bucket_id specified. Using 'user.dskt.ch' by default.")
    bucket_id <- "user"
  }

  if(is.null(folder)){
    stop("Need a folder to retrieve pin.")
  }

  board <- board_name(bucket_id, folder)

  pin_get(name = name,
          board = board,
          cache = cache,
          extract = extract,
          version = version,
          files = files,
          signature = signature)
}
