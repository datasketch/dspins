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
  bucket_id <- args$bucket_id

  if(is.null(bucket_id)){
    stop("Need a bucket_id to save dsviz")
  }

  board <- "user.dskt.ch"

  slug <- drop$slug
  format <- drop$format
  metadata$files <- list(list(path = glue::glue(paste0("{slug}.", format)),
                              format = format,
                              url = glue::glue("https://s3.amazonaws.com/user.dskt.ch/{bucket_id}/{slug}/{slug}.{format}"))) %>%
    setNames(format)


  metadata$share <- list(list(link =  glue::glue("https://datasketch.co/{bucket_id}/{slug}"),
                              permalink =  glue::glue("https://s3.amazonaws.com/user.dskt.ch/{bucket_id}/{slug}/{slug}.{format}"),
                              embed =  paste0('<iframe src="',
                                              glue::glue("https://s3.amazonaws.com/user.dskt.ch/{bucket_id}/{slug}/{slug}.{format}"),
                                              '" frameborder=0 width="100%" height="400px"></iframe>'))) %>%
    setNames(format)

  drop$share <- metadata$share
  drop$files <- metadata$files

  drop_write(drop, path)

  if(!dspins_is_board_connected("user"))
    stop("Board not connected. Run: dspins_user_board_connect(bucket_id)")

  name <- paste0(bucket_id,"/",slug)

  board_pin_store(board, path, name, drop$description, "drop",
                  extract = FALSE,
                  metadata,...)
  # upload_url
  drop
}

#' @export
pin_load.drop <- function(path, ...) {
  NULL
}

#' @export
pin_preview.drop <- function(x, ...) {
  x
}

