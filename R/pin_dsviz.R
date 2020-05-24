#' @export
pin.dsviz <- function(dv, name = NULL, description = NULL, board = NULL, ...) {
  path <- tempfile()
  path <- "tmp"
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  saveRDS(dv, file.path(path, "data.rds"), version = 2)

  dsviz_write(dv, path = path)

  metadata <- dv
  metadata$viz <- NULL
  metadata$name <- NULL
  metadata$description <- NULL

  args <- list(...)
  user_id <- args$user_id
  if(!is.null(user_id)){
    board <- board_name(user_id)
  } else {
    stop("Need a user_id to save dsviz")
  }

  upload_url <- paste0("https://s3.amazonaws.com/",board_name(user_id), dv$name)

  dspins_user_board_connect(args$user_id)

  board_pin_store(board, path, dv$slug, dv$description, "dsviz",
                  extract = FALSE,
                  metadata,...)
  upload_url
}

#' @export
pin_load.dsviz <- function(path, ...) {
  readRDS(file.path(path, "data.rds"))
}

#' @export
pin_preview.dsviz <- function(x, ...) {
  x
}



