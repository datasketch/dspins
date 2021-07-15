#' @export
pin_store.dspins_board_s3 <- function(board, slug, paths, metadata,
                                      versioned = NULL, ...) {
  check_name(slug)

  all_paths <- list.files(paths, full.names = TRUE)

  metadata$path <- fs::path_file(all_paths)

  ds_s3_upload_yaml(board, fs::path(slug, "data.txt"), metadata)
  for (path in all_paths) {
    ds_s3_upload_file(board, fs::path(slug, fs::path_file(path)), path)
  }

  metadata$path <- slug
  metadata$name <- slug

  download <- tryCatch(ds_s3_download(board, "data.txt", immutable = TRUE),
                       error = function(e){ e })

  path <- fs::path(board$cache)
  yaml_path <- fs::path(path, "data.txt")

  if(!inherits(download, "error")){
    yaml <- suppressWarnings(yaml::read_yaml(yaml_path, eval.expr = FALSE))
    slug_exists <- yaml %>% purrr::map("slug") == slug
    if(any(slug_exists)){
      slug_index <- which(slug_exists)
      yaml[[slug_index]] <- metadata
    } else {
      yaml[[length(yaml)+1]] <- metadata
    }
    write_meta(yaml, path)
    ds_s3_upload_file(board, "data.txt", yaml_path)
  } else {
    yaml <- list()
    yaml[[1]] <- metadata
    write_meta(yaml, path)
    ds_s3_upload_file(board, "data.txt", yaml_path)
  }

  invisible(board)
}



#' @export
pin_fetch.dspins_board_s3 <- function(board, name, version = NULL, ...) {


  meta <- pin_meta(board, name)
  cache_touch(board, meta)

  for (file in meta$path) {
    key <- fs::path(name, file)
    ds <- ds_s3_download(board, key, immutable = TRUE)
  }

  meta
}
