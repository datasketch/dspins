#' Store a DS pin
#'
#' Used by `dspin_write()` to upload local DS file(s) to `dspins_board_s3`,
#' not to be called directly.
#'
#' @param board `dspins_board_s3` board
#' @param slug Slug of element to be downloaded
#' @param path Path to which DS pins have been saved
#' @param metadata Metadata of DS pin
#'
#' @return Board
#'
#' @export
dspin_store <- function(board,
                        slug,
                        path,
                        metadata,
                        ...) {
  check_name(slug)

  all_paths <- list.files(path, full.names = TRUE)

  metadata <- c(path = list(fs::path_file(all_paths)), metadata)

  ds_s3_upload_yaml(board, fs::path(slug, "data.txt"), metadata)
  for (path in all_paths) {
    ds_s3_upload_file(board, fs::path(slug, fs::path_file(path)), path)
  }

  metadata$path <- slug
  metadata$name <- slug

  update_datatxt(metadata = metadata, board = board)

  invisible(board)
}


#' Fetch a DS pin
#'
#' Used by `dspin_read()` to download pins on `dspins_board_s3` to local cache,
#' not to be called directly.
#'
#' @param board `dspins_board_s3` board
#' @param slug Slug of element to be downloaded
#'
#' @return Metadata of element
#'
#' @export
dspin_fetch <- function(board,
                        slug,
                        ...) {

  meta <- dspin_meta(board, slug)
  cache_touch(board, meta)

  for (file in meta$path) {
    key <- fs::path(slug, file)
    ds <- ds_s3_download(board, key, immutable = FALSE)
  }

  meta
}
