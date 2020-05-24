#' @export
pin.drop <- function(drop, name = NULL, description = NULL, board = NULL, ...) {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  metadata <- drop
  metadata$name <- basename(drop$path)
  metadata$title <- f$name

  args <- list(...)
  if(!is.null(args$user_id)){
    board <- board_name(user_id)
  }


  upload_url <- paste0("https://s3.amazonaws.com/",board_name(user_id),"/some-file")
  upload_url <- tryCatch(board_pin_store(board, path, f$slug, f$description, "fringe",
                                         extract = FALSE,
                                         metadata,...),
                         error = function(e){
                           upload_url
                         },
                         finally = {
                           # message("Fringe uploaded to: ", upload_url)
                         })
  upload_url
}

#' @export
pin_load.drop <- function(path, ...) {
  readRDS(file.path(path, "data.rds"))
}

#' @export
pin_preview.drop <- function(x, ...) {
  x
}

