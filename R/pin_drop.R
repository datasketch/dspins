#' @export
pin.drop <- function(drop, name = NULL, description = NULL, board = NULL, ...) {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  args <- list(...)
  folder <- args$folder
  bucket_id <- args$bucket_id

  if(is.null(bucket_id)){
    message("No bucket_id specified. Using 'user.dskt.ch' by default.")
    bucket_id <- "user"
  }

  if(is.null(folder)){
    stop("Need a folder to save drop")
  }

  bucket <- bucket_name(bucket_id)
  board <- board_name(bucket_id, folder)
  slug <- drop$slug

  name <- create_slug(name) %||% slug
  metadata <- drop
  metadata$name <- NULL
  metadata$description <- NULL

  format <- drop$format
  metadata$files <- list(list(path = glue::glue(paste0("{slug}.", format)),
                              format = format,
                              url = glue::glue("https://s3.amazonaws.com/{bucket}/{folder}/{slug}/{slug}.{format}"))) %>%
    setNames(format)


  metadata$share <- list(list(link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
                              permalink =  glue::glue("https://s3.amazonaws.com/{bucket}/{folder}/{slug}/{slug}.{format}"),
                              embed =  paste0('<iframe src="',
                                              glue::glue("https://s3.amazonaws.com/{bucket}/{folder}/{slug}/{slug}.{format}"),
                                              '" frameborder=0 width="100%" height="400px"></iframe>'))) %>%
    setNames(format)

  drop$share <- metadata$share
  drop$files <- metadata$files

  drop_write(drop, path)

  if(!dspins_is_board_connected(bucket_id, folder))
    stop("Board not connected. Run: dspins_user_board_connect(bucket_id, folder)")

  board_pin_store(board, path, slug, drop$description, "drop",
                  extract = FALSE,
                  metadata,...)

  message("Saved pin")

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

