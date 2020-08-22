#' @export
pin.dsviz <- function(dv, name = NULL, description = NULL, board = NULL, ...) {
  path <- tempfile()
  #path <- "tmp"
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  saveRDS(dv, file.path(path, "data.rds"), version = 2)

  metadata <- dv
  metadata$viz <- NULL
  metadata$name <- NULL
  metadata$description <- NULL

  args <- list(...)
  bucket_id <- args$bucket_id
  if(!is.null(bucket_id)){
    board <- board_name(bucket_id)
  } else {
    stop("Need a bucket_id to save dsviz")
  }

  slug <- dv$slug

  if(dv$type == "htmlwidget") formats <- c("html", "png")
  if(dv$type == "gg") formats <- c("png", "svg")

  metadata$files <- lapply(formats, function(x){
    list(
      path = glue::glue(paste0("{slug}.",x)),
      format = x,
      url = glue::glue("https://s3.amazonaws.com/{bucket_id}.dskt.ch/{slug}/{slug}.{x}")
    )
  }) %>% setNames(formats)

  share <- list(
    html = list(
      link =  glue::glue("https://datasketch.co/{bucket_id}/{slug}"),
      permalink =  glue::glue("https://s3.amazonaws.com/{bucket_id}.dskt.ch/{slug}/{slug}.html"),
      embed =  paste0('<iframe src="',
                      glue::glue("https://s3.amazonaws.com/{bucket_id}.dskt.ch/{slug}/{slug}.html"),
                      '" frameborder=0 width="100%" height="400px"></iframe>')
    ),
    png = list(
      link =  glue::glue("https://datasketch.co/{bucket_id}/{slug}"),
      permalink =  glue::glue("https://s3.amazonaws.com/{bucket_id}.dskt.ch/{slug}/{slug}.png"),
      embed =  paste0('<iframe src="',
                      glue::glue("https://s3.amazonaws.com/{bucket_id}.dskt.ch/{slug}/{slug}.png"),
                      '"></img>')
    ),
    svg = list(
      link =  glue::glue("https://datasketch.co/{bucket_id}/{slug}"),
      permalink =  glue::glue("https://s3.amazonaws.com/{bucket_id}.dskt.ch/{slug}/{slug}.svg"),
      embed =  paste0('<img src="',
                      glue::glue("https://s3.amazonaws.com/{bucket_id}.dskt.ch/{slug}/{slug}.svg"),
                      '></img>')
    )
  )

  metadata$share <- share[formats]

  dv$files <- metadata$files
  dv$share <- metadata$share

  dsviz_write(dv, path = path)

  #upload_url <- paste0("https://s3.amazonaws.com/",board_name(bucket_id), dv$name)

  if(!dspins_is_board_connected(args$bucket_id))
    stop("Board not connected. Run: dspins_user_board_connect(bucket_id)")

  board_pin_store(board, path, slug, dv$description, "dsviz",
                  extract = FALSE,
                  metadata,...)

  if(dv$type == "htmlwidget"){
    change_content_type(slug = slug, bucket_id = bucket_id, format = "png")
    change_content_type(slug = slug, bucket_id = bucket_id, format = "html")
  }
  if(dv$type == "gg"){
    change_content_type(slug = slug, bucket_id = bucket_id, format = "png")
    change_content_type(slug = slug, bucket_id = bucket_id, format = "svg")
  }

  dv
}

#' @export
pin_load.dsviz <- function(path, ...) {
  readRDS(file.path(path, "data.rds"))
}

#' @export
pin_preview.dsviz <- function(x, ...) {
  x
}



