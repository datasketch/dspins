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

  board_register("datatxt_dspins",name = board$name,
                         url = s3_url,
                         cache = cache,
                         headers = s3_headers,
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


board_initialize.datatxt_dspins <- function(board,
                                     headers = NULL,
                                     cache = board_cache_path(),
                                     url = NULL,
                                     needs_index = TRUE,
                                     browse_url = url,
                                     bucket = NULL,
                                     index_updated = NULL,
                                     index_randomize = FALSE,
                                     path = NULL,
                                     ...) {
  if (identical(url, NULL)) stop("The 'datatxt' board requires a 'url' parameter.")

  board$url <- gsub("/?data\\.txt$|/$", "", url)
  board$headers <- headers
  board$needs_index <- needs_index
  board$borwse_url <- browse_url
  board$index_updated <- index_updated
  board$bucket <- bucket
  board$index_randomize <- index_randomize
  board$subpath <- path

  for (key in names(list(...))) {
    board[[key]] <- list(...)[[key]]
  }

  datatxt_dspins_refresh_index(board)

  board
}

datatxt_dspins_refresh_index <- function(board) {
  if (is.null(board$url)) stop("Invalid 'url' in '", board$name, "' board.")

  index_file <- "data.txt"
  if (identical(board$index_randomize, TRUE)) {
    index_file <- paste0(index_file, "?rand=", stats::runif(1) * 10^8)
  }

  index_url <- file_path_null(board$url, board$subpath, index_file)

  temp_index <- tempfile()
  response <- httr::GET(index_url,
                        httr::write_disk(temp_index, overwrite = TRUE),
                        board_datatxt_headers(board, "data.txt"))

  local_index <- file.path(board_local_storage(board$name, board = board), "data.txt")
  current_index <- board_manifest_get(local_index, default_empty = TRUE)

  if (httr::http_error(response)) {
    if (!identical(board$needs_index, FALSE)) {
      stop("Failed to retrieve data.txt file from ", board$url)
    }
  }
  else {
    new_index <- board_manifest_get(temp_index)

    # retain cache when refreshing board to avoid redownloads after board_register
    new_index <- lapply(new_index, function(new_entry) {
      current_entry <- Filter(function(e) identical(e$path, new_entry$path), current_index)
      if (length(current_entry) == 1) {
        new_entry$cache <- current_entry[[1]]$cache
      }
      new_entry
    })

    current_index <- new_index
  }

  yaml::write_yaml(current_index, local_index)
}

file_path_null <- function(...) {
  paths <- list(...)
  paths <- Filter(Negate(is.null), paths)
  paths[["fsep"]] <- "/"
  do.call("file.path", paths)
}

board_datatxt_headers <- function(board, path, verb = "GET", file = NULL) {
  if (!is.null(board$url)) {
    # remove base url form path since S3 and others require relative paths when using custom domains
    path <- gsub(paste0("^", board$url, "/?"), "", path)
  }

  if (is.list(board$headers)) {
    httr::add_headers(.headers = unlist(board$headers))
  }
  else if (is.character(board$headers)) {
    httr::add_headers(.headers = board$headers)
  }
  else if ("request" %in% class(board$headers) || is.null(board$headers)) {
    board$headers
  }
  else if (is.function(board$headers)) {
    board$headers(board, verb, path, file)
  }
  else {
    stop("Unsupported '", class(board$headers)[[1]], "' class for board headers.")
  }
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

  # board$region <- "sa-east-1"
  if (!identical(board$region, NULL)) {
    headers <- s3_headers_v4(board, verb, path, file)
  }
  else {
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
  }

  headers
}

s3_headers_v4 <- function(board, verb, path, filepath) {
  service <- "s3"
  method <- verb
  bucket <- board$bucket
  host <- paste0(bucket, ".", board$host)
  region <- board$region
  request_parameters <- ""
  amz_storage_class <- "REDUCED_REDUNDANCY"
  if (!is.null(filepath))
    amz_content_sha256 <- digest::digest(filepath, file = TRUE, algo = "sha256")
  else
    amz_content_sha256 <- digest::digest(enc2utf8(""), serialize = FALSE, algo = "sha256")
  content_type <- "application/octet-stream"

  # Key derivation functions. See:
  # http://docs.aws.amazon.com/general/latest/gr/signature-v4-examples.html#signature-v4-examples-python
  sign <- function(key, msg) {
    openssl::sha256(charToRaw(enc2utf8(msg)), key = key)
  }

  getSignatureKey <- function(key, dateStamp, regionName, serviceName) {
    kDate <- sign(paste0("AWS4", key), dateStamp)
    kRegion <- sign(kDate, regionName)
    kService <- sign(kRegion, serviceName)
    kSigning <- sign(kService, "aws4_request")
    kSigning
  }

  # Read AWS access key from env. variables or configuration file. Best practice is NOT
  # to embed credentials in code.
  access_key <- board$key
  secret_key <- board$secret

  # Create a date for headers and the credential string
  amzdate <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "GMT")
  datestamp <- format(Sys.time(), "%Y%m%d", tz = "GMT")

  # ************* TASK 1: CREATE A CANONICAL REQUEST *************
  # http://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html

  # Step 1 is to define the verb (GET, POST, etc.)--already done.

  # Step 2: Create canonical URI--the part of the URI from domain to query
  # string (use "/" if no path)
  canonical_uri <- file.path("", path)

  # Step 3: Create the canonical query string. In this example (a GET request),
  # request parameters are in the query string. Query string values must
  # be URL-encoded (space=%20). The parameters must be sorted by name.
  # For this example, the query string is pre-formatted in the request_parameters variable.
  canonical_querystring <- request_parameters

  # Step 4: Create the canonical headers. Header names must be trimmed
  # and lowercase, and sorted in code point order from low to high.
  # Note that there is a trailing \n.
  canonical_headers <- paste0(
    # "content-type:", content_type, "\n",
    "host:", host, "\n",
    "x-amz-content-sha256:", amz_content_sha256, "\n",
    "x-amz-date:", amzdate, "\n",
    # "x-amz-storage-class:", amz_storage_class, "\n",
    "")

  # Step 5: Create the list of signed headers. This lists the headers
  # signed_headers <- "content-type;host;x-amz-content-sha256;x-amz-date;x-amz-storage-class"
  signed_headers <- "host;x-amz-content-sha256;x-amz-date"

  # Step 6: Create payload hash. In this example, the payload (body of
  # the request) contains the request parameters.
  payload_hash <- amz_content_sha256

  # Step 7: Combine elements to create canonical request
  canonical_request <- paste0(method, "\n", canonical_uri, "\n", canonical_querystring, "\n", canonical_headers, "\n", signed_headers, "\n", payload_hash)

  # ************* TASK 2: CREATE THE STRING TO SIGN*************
  # Match the algorithm to the hashing algorithm you use, either SHA-1 or
  # SHA-256 (recommended)
  algorithm <- "AWS4-HMAC-SHA256"
  credential_scope <- paste0(datestamp, "/", board$region, "/", service, "/", "aws4_request")
  string_to_sign <- paste0(algorithm, "\n",  amzdate, "\n", credential_scope, "\n",  digest::digest(enc2utf8(canonical_request), serialize = FALSE, algo = "sha256"))

  # ************* TASK 3: CALCULATE THE SIGNATURE *************
  # Create the signing key using the function defined above.
  signing_key <- getSignatureKey(secret_key, datestamp, region, service)

  # Sign the string_to_sign using the signing_key
  signature <- openssl::sha256(string_to_sign, key = signing_key) %>% as.character()

  # ************* TASK 4: ADD SIGNING INFORMATION TO THE REQUEST *************
  # Put the signature information in a header named Authorization.
  authorization_header <- paste0(
    algorithm, " ",
    "Credential=", board$key, "/", credential_scope, ", ",
    "SignedHeaders=", signed_headers, ", ",
    "Signature=", signature)

  headers <- httr::add_headers(
    # "Host" = host,
    # "Content-Type" = content_type,
    "x-amz-content-sha256" = amz_content_sha256,
    "x-amz-date" = amzdate,
    # "x-amz-storage-class" = amz_storage_class,
    "Authorization" = authorization_header
  )

  headers
}
