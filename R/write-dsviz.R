dspin_write.dsviz <- function(x, slug, board, path,...){

  if (!inherits(board, "pins_board")) {
    abort("`board` must be a pin board")
  }

  saveRDS(x, file.path(path, "data.rds"), version = 2)

  args <- list(...)

  metadata <- x
  metadata$dstype <- "dsviz"
  metadata$title <- x$name
  metadata$viz <- NULL
  metadata$name <- NULL
  metadata$description <- NULL

  if(x$type == "htmlwidget") formats <- c("html", "png")
  if(x$type == "gg") formats <- c("png", "svg")

  folder <- board$folder
  bucket_id <- gsub(".dskt.ch", "", board$bucket)

  links <- dspins::create_ds_links(slug = slug, folder = folder, formats = formats, element_type = "dsviz", bucket_id = bucket_id)

  metadata$files <- links$files
  metadata$share <- links$share

  x$files <- metadata$files
  x$share <- metadata$share

  dspins::dsviz_write(x, path = path)

  metadata$description <- x$description
  metadata
}
