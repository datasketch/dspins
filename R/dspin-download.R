#' Download DS pins
#'
#' Download a DS pin to locale cache.
#'
#' @param board `dspins_board_s3` board
#' @param name Name of element to be read
#' @param ...
#'
#' @return File paths for downloaded files
#'
#' @export
#'
#' @examples
#' \dontrun{
#' board <- ds_board_s3(user_name = "test", bucket_id = "user")
#'
#' board %>% dspin_download("slug-of-drop-pin")
#' }
dspin_download <- function(board, name, hash = NULL, ...) {
  check_board(board, "pin_download()", "pin_get()")

  meta <- dspin_fetch(board, name, ...)
  check_hash(meta, hash)

  download_dir <- meta$local$dir
  n_files <- length(meta$path)
  print(paste0(n_files, " files have been downloaded to local directory:"))
  print(download_dir)
  invisible(as.character(download_dir))
}
