#' @export
dspin_get <- function(slug, bucket_id, board = NULL, cache = TRUE, extract = NULL, version = NULL, files = FALSE, signature = NULL, ...) {
  name <- paste0(bucket_id, "/", slug)

  pin_get(name = name,
          board = board,
          cache = cache,
          extract = extract,
          version = version,
          files = files,
          signature = signature)
}
