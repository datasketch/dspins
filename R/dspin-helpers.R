ds_s3_download <- function(board, key, immutable = FALSE) {
  path <- fs::path(board$cache, key)

  if (!immutable || !fs::file_exists(path)) {
    key <- paste0(board$folder, "/", key)
    resp <- board$svc$get_object(Bucket = board$bucket, Key = key)
    writeBin(resp$Body, path)
  }

  path
}

ds_s3_upload_file <- function(board, key, path) {
  body <- readBin(path, "raw", file.size(path))
  key <- paste0(board$folder, "/", key)
  board$svc$put_object(Bucket = board$bucket, Body = body, Key = key)
}

ds_s3_upload_yaml <- function(board, key, yaml) {
  body <- charToRaw(yaml::as.yaml(yaml))
  key <- paste0(board$folder, "/", key)
  board$svc$put_object(Bucket = board$bucket, Body = body, Key = key)
}

ds_s3_delete_slug <- function(board, slug) {
  dir <- paste0(board$folder, "/", slug)
  resp <- board$svc$list_objects_v2(board$bucket, Prefix = paste0(dir, "/"))
  if (resp$KeyCount == 0) {
    return(invisible())
  }

  delete <- list(Objects = map(resp$Contents, "[", "Key"))
  board$svc$delete_objects(board$bucket, Delete = delete)

  download <- tryCatch(ds_s3_download(board, "data.txt", immutable = TRUE),
                       error = function(e){ e })

  path <- fs::path(board$cache)
  yaml_path <- fs::path(path, "data.txt")

  if(!inherits(download, "error")){
    yaml <- suppressWarnings(yaml::read_yaml(yaml_path, eval.expr = FALSE))
    slugs_keep <- yaml %>% purrr::map("slug") != slug
    if(!all(slugs_keep)){
      yaml <- yaml[slugs_keep]
      write_meta(yaml, path)
      ds_s3_upload_file(board, "data.txt", yaml_path)
    }
  }

  invisible()
}


object_read <- function(meta) {
  path <- fs::path(meta$local$dir, meta$file)
  missing <- !fs::file_exists(path)

  if (any(missing)) {
    stop(paste0("Cache failure. Missing files:", path[!missing]))
  }

  if (meta$api_version == 1) {
    type <- arg_match0(meta$type, c("rds", "json", "arrow", "pickle", "csv", "file"))

    switch(type,
           rds = readRDS(path),
           json = jsonlite::read_json(path, simplifyVector = TRUE),
           arrow = arrow::read_feather(path),
           pickle = stop("'pickle' pins not supported in R"),
           csv = utils::read.csv(path, stringsAsFactors = TRUE),
           file = stop("Pin created with `pin_upload()`. Retrieve uploaded paths with `pin_download()`"
           ))
  } else {
    # used by board_rsconnect()
    type <- arg_match0(meta$type, c("default", "files", "table"))
    path <- fs::path_dir(path[[1]])

    switch(type,
           default = pin_load.default(path),
           table = pin_load.table(path),
           files = pin_load.files(path)
    )
  }
}

