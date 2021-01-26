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
                                ...) {
  board$bucket <- bucket
  if (nchar(bucket) == 0) stop("The 's3' board requires a 'bucket' parameter.")

  if (nchar(key) == 0)  stop("The 's3' board requires a 'key' parameter.")
  if (nchar(secret) == 0)  stop("The 's3' board requires a 'secret' parameter.")

  # folder_name <- sub('.*/', '', board$name)
  # s3_url <- paste0("https://", bucket, ".", host, "/",folder_name)

  s3_url <- paste0("https://", board$bucket, ".", host)

  # class(board) <- "s3"
  # board$board <- "s3"

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
                         # browse_url = paste0("https://s3.console.aws.amazon.com/s3/buckets/", board$name, "/"),
                         host = host,
                         ...)

  board_get(board$name)
}
