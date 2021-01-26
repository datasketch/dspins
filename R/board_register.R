#' @export
board_register_s3_dspins <- function(folder = "",
                                     bucket = Sys.getenv("AWS_BUCKET"),
                                     key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                                     secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                                     cache = board_cache_path(),
                                     host = "s3.amazonaws.com",
                              ...) {

  name <- paste0(bucket, "/", folder)
  board_register("s3_dspins",
                 name = name,
                 bucket = bucket,
                 key = key,
                 secret = secret,
                 cache = cache,
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
  if (nchar(bucket) == 0) stop("The 's3_dspins' board requires a 'bucket' parameter.")

  if (nchar(key) == 0)  stop("The 's3_dspins' board requires a 'key' parameter.")
  if (nchar(secret) == 0)  stop("The 's3_dspins' board requires a 'secret' parameter.")

  folder_name <- sub('.*/', '', board$name)
  s3_url <- paste0("http://", bucket, ".", host, "/",folder_name)

  board_register_datatxt(name = board$name,
                         url = s3_url,
                         cache = cache,
                         headers = s3_headers,
                         needs_index = FALSE,
                         key = key,
                         secret = secret,
                         bucket = bucket,
                         connect = FALSE,
                         browse_url = paste0("https://s3.console.aws.amazon.com/s3/buckets/", board$name, "/"),
                         host = host,
                         ...)

  board_get(board$name)
}

s3_headers <- function(board, verb, path, file) {
  date <- format(Sys.time(), "%a, %d %b %Y %H:%M:%S %z")

  # allow full urls to allow arbitrary file downloads
  bucket <- board$bucket
  if (grepl("^https?://", path)) {
    path_nohttp <-  gsub("^https?://", "", path)
    path <- gsub("^[^/]+/", "", path_nohttp)
    bucket <- gsub("\\..*", "", path_nohttp)
  }

  content <- paste(
    verb,
    "",
    "application/octet-stream",
    date,
    file.path("", bucket, path),
    sep = "\n"
  )

  signature <- openssl::sha1(charToRaw(content), key = board$secret) %>%
    base64enc::base64encode()

  headers <- httr::add_headers(
    Host = paste0(bucket, ".", board$host),
    Date = date,
    `Content-Type` = "application/octet-stream",
    Authorization = paste0("AWS ", board$key, ":", signature)
  )

  headers
}


#
#
# board_register <- function(board,
#                            name = board,
#                            cache = board_cache_path(),
#                            versions = NULL,
#                            ...) {
#   params <- list(...)
#
#   inferred <- board_infer(board,
#                           board = board,
#                           name = name,
#                           register_call = params$register_call,
#                           connect = params$connect,
#                           url = params$url)
#   params$url <- NULL
#
#   new_params <- c(
#     list(inferred$board, inferred$name, cache = cache, versions = versions),
#     params,
#     url = inferred$url
#   )
#
#   board <- do.call("new_board", new_params)
#
#   board_registry_set(inferred$name, board)
#
#   if (is.null(inferred$register_call)) inferred$register_call <- board_register_code(board$name, inferred$name)
#
#   if (!identical(inferred$connect, FALSE)) board_connect(board$name, inferred$register_call)
#
#   invisible(inferred$name)
# }
#
#
# board_infer <- function(x, name = NULL, board = NULL, register_call = NULL, connect = NULL, url = NULL) {
#   inferred <- list(
#     name = name,
#     board = if (is.null(board)) name else board,
#     connect = if (is.null(connect)) !identical(name, "packages") else connect,
#     url = url,
#     register_call = register_call
#   )
#
#   # if boards starts with http:// or https:// assume this is a website board
#   if (grepl("^http://|^https://", x)) {
#     inferred$url <- x
#     inferred$board <- "datatxt"
#
#     # use only subdomain as friendly name which is also used as cache folder
#     if (is.null(name) || identical(x, name)) {
#       inferred$name <- gsub("https?://|\\..*", "", inferred$url)
#     }
#
#     inferred$register_call <- paste0("pins::board_register(board = \"datatxt\", name = \"",
#                                      inferred$name,
#                                      "\", url = \"",
#                                      inferred$url,
#                                      "\")")
#   }
#
#   if (is.null(inferred$name)) inferred$name <- x
#   if (is.null(inferred$board)) inferred$board <- x
#
#   inferred
# }
#
#
# new_board <- function(board, name, cache, versions, ...) {
#
#   if (is.null(cache)) stop("Please specify the 'cache' parameter.")
#
#   board <- structure(list(
#     board = board,
#     name = name,
#     cache = cache,
#     versions = versions
#   ),
#   class = board)
#
#   board <- board_initialize(board, cache = cache, versions = versions, ...)
#
#   board
# }
#
#
#
# board_initialize.datatxt_dspins <- function(board,
#                                      headers = NULL,
#                                      cache = board_cache_path(),
#                                      url = NULL,
#                                      needs_index = TRUE,
#                                      browse_url = url,
#                                      bucket = NULL,
#                                      index_updated = NULL,
#                                      index_randomize = FALSE,
#                                      ...) {
#   if (identical(url, NULL)) stop("The 'datatxt' board requires a 'url' parameter.")
#
#   board$url <- gsub("/?data\\.txt$|/$", "", url)
#   board$headers <- headers
#   board$needs_index <- needs_index
#   board$borwse_url <- browse_url
#   board$index_updated <- index_updated
#   board$bucket <- bucket
#   board$index_randomize <- index_randomize
#
#   for (key in names(list(...))) {
#     board[[key]] <- list(...)[[key]]
#   }
#
#   datatxt_dspins_refresh_index(board)
#
#   board
# }
#
# datatxt_dspins_refresh_index <- function(board) {
#   if (is.null(board$url)) stop("Invalid 'url' in '", board$name, "' board.")
#
#   index_file <- "data.txt"
#   if (identical(board$index_randomize, TRUE)) {
#     index_file <- paste0(index_file, "?rand=", stats::runif(1) * 10^8)
#   }
#
#   index_url <- file.path(board$url, index_file)
#
#   temp_index <- tempfile()
#   response <- httr::GET(index_url,
#                         httr::write_disk(temp_index, overwrite = TRUE),
#                         board_datatxt_dspins_headers(board, "data.txt"))
#
#   local_index <- file.path(board_local_storage(board$name, board = board), "data.txt")
#   current_index <- board_manifest_get(local_index, default_empty = TRUE)
#
#   if (httr::http_error(response)) {
#     if (!identical(board$needs_index, FALSE)) {
#       stop("Failed to retrieve data.txt file from ", board$url)
#     }
#   }
#   else {
#     new_index <- board_manifest_get(temp_index)
#
#     # retain cache when refreshing board to avoid redownloads after board_register
#     new_index <- lapply(new_index, function(new_entry) {
#       current_entry <- Filter(function(e) identical(e$path, new_entry$path), current_index)
#       if (length(current_entry) == 1) {
#         new_entry$cache <- current_entry[[1]]$cache
#       }
#       new_entry
#     })
#
#     current_index <- new_index
#   }
#
#   yaml::write_yaml(current_index, local_index)
# }
#
#
# file_path_null <- function(...) {
#   paths <- list(...)
#   paths <- Filter(Negate(is.null), paths)
#   paths[["fsep"]] <- "/"
#   do.call("file.path", paths)
# }
#
# board_datatxt_dspins_headers <- function(board, path, verb = "GET", file = NULL) {
#   if (!is.null(board$url)) {
#     # remove base url form path since S3 and others require relative paths when using custom domains
#     path <- gsub(paste0("^", board$url, "/?"), "", path)
#   }
#
#   if (is.list(board$headers)) {
#     httr::add_headers(.headers = unlist(board$headers))
#   }
#   else if (is.character(board$headers)) {
#     httr::add_headers(.headers = board$headers)
#   }
#   else if ("request" %in% class(board$headers) || is.null(board$headers)) {
#     board$headers
#   }
#   else if (is.function(board$headers)) {
#     board$headers(board, verb, path, file)
#   }
#   else {
#     stop("Unsupported '", class(board$headers)[[1]], "' class for board headers.")
#   }
# }
#
#
# board_registry_ensure <- function(register = TRUE) {
#   if (identical(.globals$boards_registered, NULL)) {
#     .globals$boards_registered <- list()
#   }
# }
#
# board_registry_list <- function() {
#   board_registry_ensure()
#
#   names(.globals$boards_registered)
# }
#
# board_registry_get <- function(name) {
#   board_registry_ensure()
#
#   .globals$boards_registered[[name]]
# }
#
# board_registry_set <- function(name, board) {
#   board_registry_ensure(FALSE)
#
#   .globals$boards_registered[[name]] <- board
# }
#
# .globals <- new.env(parent = emptyenv())
#
# board_register_code <- function(board, name) {
#   parent_idx <- 1
#   parent_call <- NULL
#   function_name <- NULL
#
#   while (parent_idx < length(sys.parents())) {
#     parent_func <- sys.function(sys.parent(parent_idx))
#     parent_call <- sys.call(sys.parent(parent_idx))
#     if (!is.function(parent_func) || !is.call(parent_call)) break;
#
#     this_parent_call <- tryCatch(match.call(definition = parent_func, call = parent_call), error = function(e) NULL)
#
#     if (is.null(this_parent_call)) break;
#     if (length(this_parent_call) < 1) break;
#
#     this_function_name <- deparse(this_parent_call[[1]])
#
#     if (!grepl("(^|::)board_register", this_function_name)) break;
#
#     parent_call <- this_parent_call
#     function_name <- this_function_name
#     parent_idx <- parent_idx + 1
#   }
#
#   header <- if (grepl("^pins::", function_name)) "" else "library(pins)\n"
#   if (is.null(parent_call)) {
#     paste0(header, "board_register(\"", board, "\", name = \"", name, "\")")
#   }
#   else {
#     main_call <- paste(deparse(parent_call, width.cutoff = 500), collapse = " ")
#     paste0(header, main_call)
#   }
# }
