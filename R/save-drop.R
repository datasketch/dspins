dspin_save.drop <- function(x, slug, board, path,...){

  if (!inherits(board, "pins_board")) {
    abort("`board` must be a pin board")
  }

  args <- list(...)

  name <- create_slug(x$name) %||% slug

  metadata <- x
  metadata$type <- "drop"
  metadata$title <- x$name
  metadata$name <- NULL
  metadata$description <- NULL

  format <- x$format

  folder <- board$folder
  bucket_id <- gsub(".dskt.ch", "", board$bucket)

  links <- dspins::create_ds_links(slug = slug, folder = folder, formats = format, element_type = "drop", bucket_id = bucket_id)

  metadata$files <- links$files
  metadata$share <- links$share

  x$share <- metadata$share
  x$files <- metadata$files

  dspins::drop_write(x, path)

  metadata$description <- x$description
  metadata
}
