dspin_write.dsviz <- function(x, slug, board, path,...){

  if (!inherits(board, "pins_board")) {
    abort("`board` must be a pin board")
  }

  args <- list(...)

  metadata <- x
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

  metadata$type <- "dsviz"
  metadata$description <- x$description
  metadata
}
