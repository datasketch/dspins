#' @importFrom pins pin
#' @exportS3Method dspins::pin
pin.dsviz <- function(dv, name = NULL, description = NULL, board = NULL, ...) {
  .Deprecated("dspin_read")
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  saveRDS(dv, file.path(path, "data.rds"), version = 2)

  metadata <- dv
  metadata$title <- dv$name
  metadata$viz <- NULL
  metadata$name <- NULL
  metadata$description <- NULL

  args <- list(...)
  folder <- args$folder
  bucket_id <- args$bucket_id

  if(is.null(bucket_id)){
    message("No bucket_id specified. Using 'user.dskt.ch' by default.")
    bucket_id <- "user"
  }

  if(is.null(folder)){
    stop("Need a folder to save dsviz")
  }

  bucket <- bucket_name(bucket_id)
  board <- board_name(bucket_id, folder)
  slug <- dv$slug

  if(dv$type == "htmlwidget") formats <- c("html", "png")
  if(dv$type == "gg") formats <- c("png", "svg")

  links <- create_ds_links(slug = slug, folder = folder, formats = formats, element_type = "dsviz", bucket_id = bucket_id)

  metadata$files <- links$files
  metadata$share <- links$share

  dv$files <- metadata$files
  dv$share <- metadata$share

  dsviz_write(dv, path = path)

  if(!dspins_is_board_connected(folder, bucket_id))
    stop("Board not connected. Run: dspins_user_board_connect(folder, bucket_id)")

  pins::board_pin_store(board, path, slug, dv$description, "dsviz",
                        extract = FALSE,
                        metadata,...)

  message("Saved pin")
  message("Changing content type")

  if(dv$type == "htmlwidget"){
    change_content_type(slug = slug, format = "png", bucket = bucket, folder = folder)
    change_content_type(slug = slug, format = "html", bucket = bucket, folder = folder)
  }
  if(dv$type == "gg"){
    change_content_type(slug = slug, format = "png", bucket = bucket, folder = folder)
    change_content_type(slug = slug, format = "svg", bucket = bucket, folder = folder)
  }

  dv
}

pin_load.dsviz <- function(path, ...) {
  readRDS(file.path(path, "data.rds"))
}

pin_preview.dsviz <- function(x, ...) {
  x
}



