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
  metadata$name <- NULL
  metadata$description <- NULL

  format <- drop$format

  url_base_path <- glue::glue("https://{bucket}/{folder}/{slug}/{slug}")

  metadata$files <- list(list(path = glue::glue(paste0("{slug}.", format)),
                              format = format,
                              url = glue::glue("{url_base_path}.{format}"))) %>%
    setNames(format)


  metadata$share <- list(list(link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
                              permalink =  glue::glue("{url_base_path}.{format}"),
                              embed =  paste0('<iframe src="',
                                              glue::glue("{url_base_path}.{format}"),
                                              '" frameborder=0 width="100%" height="400px"></iframe>'))) %>%
    setNames(format)

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

