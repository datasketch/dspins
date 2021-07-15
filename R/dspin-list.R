#' @export
pin_list.dspins_board_s3 <- function(board, ...) {
  args <- list(...)
  extended <- args$extended

  download <- tryCatch(ds_s3_download(board, "data.txt", immutable = TRUE),
                       error = function(e){ e })

  if(inherits(download, "error")) return("No pins in folder.")

  path <- fs::path(board$cache)
  yaml_path <- fs::path(path, "data.txt")
  yaml <- suppressWarnings(yaml::read_yaml(yaml_path, eval.expr = FALSE))

  ls <- yaml %>% purrr::map_chr("slug")

  if(identical(extended, TRUE)){
    ls <- jsonlite::fromJSON(jsonlite::toJSON(yaml, null = "null", auto_unbox = TRUE))
  }

  ls
}


#' @export
dspin_list <- function(folder, bucket_id = NULL) {
  .Deprecated("pin_list")

  if(is.null(bucket_id)){
    message("No bucket_id specified. Using 'user.dskt.ch' by default.")
    bucket_id <- "user"
  }

  board <- ds_board_s3(folder, bucket_id)

  pin_list(board, extended = TRUE)
}
