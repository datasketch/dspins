#' Save DS elements
#'
#' Preprocesses metadata for elements of type `fringe`, `dsviz`, or `drop`
#' and saves them to given path.
#'
#' @param element Element to be saved (`fringe`, `dsviz`, or `drop`)
#' @param slug Slug of element to be saved
#' @param board `dspins_board_s3` board
#' @param path Path to save elements to
#' @param ...
#'
#' @return Metadata for element
#'
#' @export
dspin_save <- function(element, slug, board, path, ...) {
  ellipsis::check_dots_used()
  UseMethod("dspin_save")
}

dspin_save.fringe <- function(x, slug, board, path,...){

  if (!inherits(board, "pins_board")) {
    abort("`board` must be a pin board")
  }

  saveRDS(x, file.path(path, "data.rds"), version = 2)

  data_html_path <- system.file("data_tpl.html", package = "dspins")
  file.copy(data_html_path, file.path(path, paste0(slug,".html")))

  args <- list(...)

  metadata <- x$meta
  metadata$type <- "fringe"
  metadata$title <- x$name
  metadata$stats <- x$stats
  metadata$slug <- slug
  metadata$group <- x$group
  metadata$frtype <- as.character(x$frtype)

  formats <- unique(c(c("csv", "json"), args$download_formats))

  folder <- board$folder
  bucket_id <- gsub(".dskt.ch", "", board$bucket)

  links <- dspins::create_ds_links(slug = slug, folder = folder, formats = formats, element_type = "fringe", bucket_id = bucket_id)

  metadata$files <- links$files
  metadata$share <- links$share

  x$files <- metadata$files
  x$share <- metadata$share
  x$meta <- metadata

  homodatum::fringe_write(x, path = path, overwrite_dic = TRUE)
  homodatum::fringe_write_json(x, path = path)

  credits <- args$credits %||% list(label = "Downloaded from:",
                                    value = paste0("http://datasketch.co/",folder,"/",slug))

  download_formats <- c("csv", "json", args$download_formats)
  message(download_formats)
  if("xlsx" %in% download_formats){
    homodatum::fringe_write_xlsx(x, path = path, credits = credits)
  }
  metadata$description <- x$description
  metadata
}
