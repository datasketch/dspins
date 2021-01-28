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
  path <- folder
  board_register("s3_dspins",
                 name = name,
                 bucket = bucket,
                 key = key,
                 secret = secret,
                 cache = cache,
                 region = region,
                 path = path,
                 ...)
}

board_initialize.s3_dspins <- function(board,
                                bucket = Sys.getenv("AWS_BUCKET"),
                                key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                                secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                                cache = NULL,
                                host = "s3.amazonaws.com",
                                path = NULL,
                                ...) {
  board$bucket <- bucket
  if (nchar(bucket) == 0) stop("The 's3_dspins' board requires a 'bucket' parameter.")

  if (nchar(key) == 0)  stop("The 's3_dspins' board requires a 'key' parameter.")
  if (nchar(secret) == 0)  stop("The 's3_dspins' board requires a 'secret' parameter.")

  s3_url <- paste0("http://", bucket, ".", host)

  board_register_datatxt(name = board$name,
                         url = s3_url,
                         cache = cache,
                         headers = pins:::s3_headers,
                         needs_index = FALSE,
                         key = key,
                         secret = secret,
                         bucket = bucket,
                         connect = FALSE,
                         browse_url = paste0("https://s3.console.aws.amazon.com/s3/buckets/", board$name, "/"),
                         host = host,
                         path = path,
                         ...)

  board_get(board$name)
}



# board_register_s3_old and board_initialize.s3_old will still exist temporarily to access the older user buckets
board_register_s3_old <- function(name = "s3",
                              bucket = Sys.getenv("AWS_BUCKET"),
                              key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                              secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                              cache = board_cache_path(),
                              host = "s3.amazonaws.com",
                              region = NULL,
                              path = NULL,
                              ...) {
  board_register("s3_old",
                 name = name,
                 bucket = bucket,
                 key = key,
                 secret = secret,
                 cache = cache,
                 region = region,
                 path = path,
                 ...)
}

board_initialize.s3_old <- function(board,
                                bucket = Sys.getenv("AWS_BUCKET"),
                                key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                                secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                                cache = NULL,
                                host = "s3.amazonaws.com",
                                ...) {
  board$bucket <- bucket
  if (nchar(bucket) == 0) stop("The 's3' board requires a 'bucket' parameter.")

  if (nchar(key) == 0)  stop("The 's3' board requires a 'key' parameter.")
  if (nchar(secret) == 0)  stop("The 's3' board requires a 'secret' parameter.")

  s3_url <- paste0("http://", board$bucket, ".", host)

  board_register_datatxt(name = board$name,
                         url = s3_url,
                         cache = cache,
                         headers = pins:::s3_headers,
                         needs_index = FALSE,
                         key = key,
                         secret = secret,
                         bucket = bucket,
                         connect = FALSE,
                         browse_url = paste0("https://s3.console.aws.amazon.com/s3/buckets/", bucket, "/"),
                         host = host,
                         ...)

  board_get(board$name)
}
