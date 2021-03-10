#' @export
datatxt_refresh_index_ds <- function(board) {
  if (is.null(board$url)) stop("Invalid 'url' in '", board$name, "' board.")

  index_file <- "data.txt"
  index_url <- file_path_null(board$url, board$subpath, index_file)

  index_file_get <- file_path_null(board$subpath, index_file)

  if (identical(board$index_randomize, TRUE)) {
    index_file_get <- paste0(index_file_get, "?rand=", stats::runif(1) * 10^8)
  }

  temp_index <- tempfile()
  response <- httr::GET(index_url,
                        httr::write_disk(temp_index, overwrite = TRUE),
                        board_datatxt_headers(board, index_file_get))

  local_index <- file.path(pins::board_local_storage(board$name, board = board), "data.txt")
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


board_manifest_get <- function(path, default_empty = FALSE) {
  if (!file.exists(path) && default_empty) return(list())
  suppressWarnings(yaml::read_yaml(path, eval.expr = FALSE))
}

board_manifest_load <- function(manifest) {
  suppressWarnings(yaml::yaml.load(manifest, eval.expr = FALSE))
}

board_manifest_create <- function(index, file) {
  yaml::write_yaml(index, file)
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
