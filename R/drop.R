


drop <- function(path, name = NULL, description = NULL, ...){
  args <- list(...)
  # access = NULL,
  # license = NULL,
  # tidy = FALSE,
  # time_created = NULL,
  # time_last_updated = NULL,
  # data_preview = NULL,
  # preview_img = NULL,
  # filesize = NULL,
  # format = NULL
  l <- list(
    path = path,
    name = name %||% create_slug(args$title),
    description = NULL,
    access = args$access %||% "private",
    license = NULL,
    #time_created = NULL,
    time_last_updated = args$time_last_updated %||% unix_timestamp(),
    filesize = file.info(path)$size,
    format = file_ext(path),
    tags = args$tags,
    sources = args$sources
  )
  class(l) <- "drop"
  drop
}


