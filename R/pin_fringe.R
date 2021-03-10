#' @importFrom pins pin
#' @exportS3Method dspins::pin
pin.fringe <- function(f, name = NULL, description = NULL, board = NULL, ...) {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  saveRDS(f, file.path(path, "data.rds"), version = 2)

  data_html_path <- system.file("data_tpl.html", package = "dspins")
  file.copy(data_html_path, file.path(path, paste0(f$slug,".html")))

  args <- list(...)
  folder <- args$folder
  bucket_id <- args$bucket_id

  if(is.null(bucket_id)){
    message("No bucket_id specified. Using 'user.dskt.ch' by default.")
    bucket_id <- "user"
  }

  if(is.null(folder)){
    stop("Need a folder to save fringe")
  }

  bucket <- bucket_name(bucket_id)
  board <- board_name(bucket_id, folder)
  slug <- f$slug

  metadata <- f$meta
  metadata$title <- f$name
  metadata$stats <- f$stats
  metadata$slug <- slug
  metadata$group <- f$group
  metadata$frtype <- as.character(f$frtype)

  url_base_path <- glue::glue("https://{bucket}/{folder}/{slug}/{slug}")

  formats <- c(c("csv", "json"), args$download_formats)
  metadata$files <- lapply(formats, function(x){
    list(
      path = glue::glue("{slug}.{x}"),
      format = x,
      url = glue::glue("{url_base_path}.{x}")
    )
  }) %>% setNames(formats)

  metadata$share <- list(
    html = list(
      link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
      permalink =  glue::glue("{url_base_path}.html"),
      embed =  paste0('<iframe src="',
                      glue::glue("{url_base_path}.html"),
                      '" frameborder=0 width="100%" height="400px"></iframe>')),
    csv = list(
      link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
      permalink =  glue::glue("{url_base_path}.csv")),
    json = list(
      link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
      permalink =  glue::glue("{url_base_path}.json"))
  )

  f$files <- metadata$files
  f$share <- metadata$share
  f$meta <- metadata

  fringe_write(f, path = path, overwrite_dic = TRUE)
  fringe_write_json(f, path = path)

  credits <- args$credits %||% list(label = "Downloaded from:",
                               value = paste0("http://datasketch.co/",folder,"/",slug))

  download_formats <- c("csv", "json", args$download_formats)
  message(download_formats)
  if("xlsx" %in% download_formats){
    fringe_write_xlsx(f, path = path, credits = credits)
  }


  if(!dspins_is_board_connected(folder, bucket_id))
    stop("Board not connected. Run: dspins_user_board_connect(folder, bucket_id)")

  upload_url <- tryCatch(board_pin_store(board, path, slug, f$description, "fringe",
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

  change_content_type(slug = slug, format = "html", bucket = bucket, folder = folder)
  change_content_type(slug = slug, format = "csv", bucket = bucket, folder = folder)
  change_content_type(slug = slug, format = "json", bucket = bucket, folder = folder)

  f$meta <- NULL
  f
}

#' @importFrom pins pin_load
#' @exportS3Method dspins::pin_load
pin_load.fringe <- function(path, ...) {
  readRDS(file.path(path, "data.rds"))
}

#' @importFrom pins pin_preview
#' @exportS3Method dspins::pin_preview
pin_preview.fringe <- function(x, ...) {
  x
}

