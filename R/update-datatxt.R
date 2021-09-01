update_datatxt <- function(metadata, board){
  # Downloads the `data.txt` file for a given board, updates metadata and
  # uploads the updated metadata by overwriting the existing `data.txt`.
  # If the an entry already exists for the `slug` passed through `metadata$slug`,
  # the existing entry is overwritten; otherwise it is added.

  slug <- metadata$slug

  # try to download data.txt for board
  download <- tryCatch(ds_s3_download(board, "data.txt", immutable = FALSE),
                       error = function(e){ e })

  path <- fs::path(board$cache)
  yaml_path <- fs::path(path, "data.txt")

  if(!inherits(download, "error")){

    # if data.txt exists, add metadata or overwrite existing
    yaml <- suppressWarnings(yaml::read_yaml(yaml_path, eval.expr = FALSE))
    slug_exists <- yaml %>% purrr::map("slug") == slug
    if(any(slug_exists)){
      slug_index <- which(slug_exists)
      yaml[[slug_index]] <- metadata
    } else {
      yaml[[length(yaml)+1]] <- metadata
    }
  } else {

    # if data.txt does not exist, create yaml with metadata
    yaml <- list()
    yaml[[1]] <- metadata
  }

  # save and upload updated (or new) data.txt file
  write_meta(yaml, path)
  ds_s3_upload_file(board, "data.txt", yaml_path)
}
