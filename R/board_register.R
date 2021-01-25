#' @export
board_register_s3_dspins <- function(folder = "",
                              bucket = Sys.getenv("AWS_BUCKET"),
                              key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                              secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                              cache = board_cache_path(),
                              host = "s3.amazonaws.com",
                              region = NULL,
                              path = NULL,
                              ...) {

  name <- paste0(bucket, "/", folder)
  board_register("s3",
                 name = name,
                 bucket = bucket,
                 key = key,
                 secret = secret,
                 cache = cache,
                 region = region,
                 path = path,
                 ...)
}
