#' @export
pin.fringe <- function(f, name = NULL, description = NULL, board = NULL, ...) {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  saveRDS(f, file.path(path, "data.rds"), version = 2)



  data_html_path <- system.file("data_tpl.html", package = "dspins")
  file.copy(data_html_path, file.path(path, paste0(f$slug,".html")))

  slug <- f$slug

  metadata <- f$meta
  metadata$title <- f$name
  metadata$stats <- f$stats
  metadata$slug <- slug
  metadata$group <- f$group
  metadata$frtype <- as.character(f$frtype)

  args <- list(...)
  if(!is.null(args$bucket_id)){
    board <- board_name(args$bucket_id)
  }else{
    stop("Need bucket_id")
  }
  bucket_id <- args$bucket_id

  formats <- c(c("csv", "json"), args$download_formats)
  metadata$files <- lapply(formats, function(x){
    list(
      path = glue::glue("{slug}.{x}"),
      format = x,
      url = glue::glue("https://s3.amazonaws.com/{bucket_id}.dskt.ch/{slug}/{slug}.{x}")
    )
  }) %>% setNames(formats)

  metadata$share <- list(
    html = list(
      link =  glue::glue("https://datasketch.co/{bucket_id}/{slug}"),
      permalink =  glue::glue("https://s3.amazonaws.com/{bucket_id}.dskt.ch/{slug}/{slug}.html"),
      embed =  paste0('<iframe src="',
                      glue::glue("https://s3.amazonaws.com/{bucket_id}.dskt.ch/{slug}/{slug}.html"),
                      '" frameborder=0 width="100%" height="400px"></iframe>')),
    csv = list(
      link =  glue::glue("https://datasketch.co/{bucket_id}/{slug}"),
      permalink =  glue::glue("https://s3.amazonaws.com/{bucket_id}.dskt.ch/{slug}/{slug}.csv")),
    json = list(
      link =  glue::glue("https://datasketch.co/{bucket_id}/{slug}"),
      permalink =  glue::glue("https://s3.amazonaws.com/{bucket_id}.dskt.ch/{slug}/{slug}.json"))
  )

  f$files <- metadata$files
  f$share <- metadata$share
  f$meta <- metadata

  fringe_write(f, path = path, overwrite_dic = TRUE)
  fringe_write_json(f, path = path)

  credits <- args$credits %||% list(label = "Downloaded from:",
                               value = paste0("http://datasketch.co/",bucket_id,"/",slug))

  download_formats <- c("csv", "json", args$download_formats)
  message(download_formats)
  if("xlsx" %in% download_formats){
    fringe_write_xlsx(f, path = path, credits = credits)
  }


  if(!dspins_is_board_connected(args$bucket_id))
    stop("Board not connected. Run: dspins_user_board_connect(bucket_id)")

  #upload_url <- paste0("https://s3.amazonaws.com/",board_name(bucket_id),"/some-file")
  upload_url <- tryCatch(board_pin_store(board, path, f$slug, f$description, "fringe",
                                         extract = FALSE,
                                         metadata,...),
                         error = function(e){
                           e
                         },
                         finally = {
                           # message("Fringe uploaded to: ", upload_url)
                         })
  message("Saved pin")
  message("Changing content type")
  change_content_type(slug = slug, bucket_id = bucket_id, format = "html")
  change_content_type(slug = slug, bucket_id = bucket_id, format = "csv")
  change_content_type(slug = slug, bucket_id = bucket_id, format = "json")

  f$meta <- NULL
  f
}

#' @export
pin_load.fringe <- function(path, ...) {
  readRDS(file.path(path, "data.rds"))
}

#' @export
pin_preview.fringe <- function(x, ...) {
  x
}

