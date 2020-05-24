#' @export
pin.drop <- function(drop, name = NULL, description = NULL, board = NULL, ...) {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  name <- create_slug(name) %||% drop$slug
  metadata <- drop
  metadata$name <- NULL
  metadata$description <- NULL

  args <- list(...)
  user_id <- args$user_id
  if(!is.null(user_id)){
    board <- board_name(user_id)
  } else{
    stop("Need to provide a user_id")
  }

  drop_write(drop, path)

  dspins_user_board_connect(user_id)

  upload_url <- paste0("https://s3.amazonaws.com/",file.path(board_name(user_id), drop$slug))

  board_pin_store(board, path, drop$slug, drop$description, "drop",
                  extract = FALSE,
                  metadata,...)
  upload_url
}

#' @export
pin_load.drop <- function(path, ...) {
  NULL
}

#' @export
pin_preview.drop <- function(x, ...) {
  x
}

