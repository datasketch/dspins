#' @importFrom pins pin
#' @exportS3Method dspins::pin
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
  metadata$title <- drop$name
  metadata$name <- NULL
  metadata$description <- NULL

  format <- drop$format

  links <- create_ds_links(slug = slug, folder = folder, formats = format, element_type = "drop", bucket_id = bucket_id)

  metadata$files <- links$files
  metadata$share <- links$share

  drop$share <- metadata$share
  drop$files <- metadata$files

  drop_write(drop, path)

  if(!dspins_is_board_connected(folder, bucket_id))
    stop("Board not connected. Run: dspins_user_board_connect(folder, bucket_id)")

  pins::board_pin_store(board, path, slug, drop$description, "drop",
                        extract = FALSE,
                        metadata,...)

  message("Saved pin")

  drop
}

#' @importFrom pins pin_load
#' @exportS3Method dspins::pin_load
pin_load.drop <- function(path, ...) {
  NULL
}

#' @importFrom pins pin_preview
#' @exportS3Method dspins::pin_preview
pin_preview.drop <- function(x, ...) {
  x
}

