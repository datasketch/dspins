#' @importFrom pins board_initialize
#' @export
board_register_s3_dspins <- function(folder = "",
                                     bucket = Sys.getenv("AWS_BUCKET"),
                                     key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                                     secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                                     cache = pins::board_cache_path(),
                                     host = "s3.amazonaws.com",
                                     region = NULL,
                                     path = NULL,
                              ...) {

  name <- paste0(bucket, "/", folder)
  path <- folder
  pins::board_register("s3_dspins",
                 name = name,
                 bucket = bucket,
                 key = key,
                 secret = secret,
                 cache = cache,
                 region = region,
                 path = path,
                 ...)

  board <- pins::board_get(name)

  datatxt_refresh_index_ds(board)
}

#' @exportS3Method dspins::board_initialize
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

  pins::board_register_datatxt(name = board$name,
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

  pins::board_get(board$name)

}
