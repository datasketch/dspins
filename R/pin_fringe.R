#' @export
pin.fringe <- function(f, name = NULL, description = NULL, board = NULL, ...) {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  saveRDS(f, file.path(path, "data.rds"), version = 2)

  fringe_write(f, path = path, overwrite_dic = TRUE)
  metadata <- f$meta
  metadata$title <- f$name
  metadata$stats <- f$stats

  args <- list(...)
  if(!is.null(args$user_id)){
    board <- board_name(user_id)
  }


  #upload_url <- paste0("https://s3.amazonaws.com/",board_name(user_id),"/some-file")
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
pin_load.fringe <- function(path, ...) {
  readRDS(file.path(path, "data.rds"))
}

#' @export
pin_preview.fringe <- function(x, ...) {
  x
}

