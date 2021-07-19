pin_download <- function(board, name, version = NULL, hash = NULL, ...) {
  check_board(board, "pin_download()", "pin_get()")

  meta <- pin_fetch(board, name, version = version, ...)
  check_hash(meta, hash)

  paths <- meta$path %>% map_chr(~ as.character(fs::path(meta$local$dir, .x)))
  n_paths <- length(paths)
  print(paste0("Found ", n_paths, " download paths."))
  print(paths)
}
