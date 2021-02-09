#' @export
pin.dsviz <- function(dv, name = NULL, description = NULL, board = NULL, ...) {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  saveRDS(dv, file.path(path, "data.rds"), version = 2)

  metadata <- dv
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

  url_base_path <- glue::glue("https://{bucket}/{folder}/{slug}/{slug}")

  metadata$files <- lapply(formats, function(x){
    list(
      path = glue::glue(paste0("{slug}.",x)),
      format = x,
      url = glue::glue("{url_base_path}.{x}")
    )
  }) %>% setNames(formats)

  share <- list(
    html = list(
      link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
      permalink =  glue::glue("{url_base_path}.html"),
      embed =  paste0('<iframe src="',
                      glue::glue("{url_base_path}.html"),
                      '" frameborder=0 width="100%" height="400px"></iframe>')
    ),
    png = list(
      link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
      permalink =  glue::glue("{url_base_path}.png"),
      embed =  paste0('<img src="',
                      glue::glue("{url_base_path}.png"),
                      '"></img>')
    ),
    svg = list(
      link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
      permalink =  glue::glue("{url_base_path}.svg"),
      embed =  paste0('<img src="',
                      glue::glue("{url_base_path}.svg"),
                      '></img>')
    )
  )

  metadata$share <- share[formats]

  dv$files <- metadata$files
  dv$share <- metadata$share

  dsviz_write(dv, path = path)

  if(!dspins_is_board_connected(folder, bucket_id))
    stop("Board not connected. Run: dspins_user_board_connect(folder, bucket_id)")

  board_pin_store(board, path, slug, dv$description, "dsviz",
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

#' @export
pin_load.dsviz <- function(path, ...) {
  readRDS(file.path(path, "data.rds"))
}

#' @export
pin_preview.dsviz <- function(x, ...) {
  x
}



