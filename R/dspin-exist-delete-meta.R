#' @export
pin_exists.dspins_board_s3 <- function(board, name, ...) {
  folder <- board$folder
  resp <- board$svc$list_objects_v2(board$bucket, Prefix = paste0(folder, "/", name, "/"))
  resp$KeyCount > 0
}


#' @export
pin_delete.dspins_board_s3 <- function(board, names, ...) {
  for (name in names) {
    ds_s3_delete_slug(board, name)
  }
  invisible(board)
}


#' @export
pin_meta.dspins_board_s3 <- function(board, name, version = NULL, ...) {

  check_pin_exists(board, name)

  path <- fs::path(board$cache, name)
  fs::dir_create(path)

  ds_s3_download(board, fs::path(name, "data.txt"), immutable = TRUE)
  local_meta(
    read_meta(fs::path(board$cache, name)),
    dir = path,
    version = NULL
  )
}

local_meta <- function(x, dir, version, ...) {
  x$local <- list(
    dir = dir,
    version = version,
    ...
  )
  structure(x, class = "pins_meta")
}


read_meta <- function(path) {
  path <- fs::path(path, "data.txt")

  if (!fs::file_exists(path)) {
    return(list(api_version = 1L))
  }

  yaml <- yaml::read_yaml(path, eval.expr = FALSE)
  if (is.null(yaml$api_version)) {
    yaml$api_version <- 0L
  } else if (yaml$api_version > 1) {
    abort(c(
      paste0("Metadata requires pins ", yaml$api_version, ".0.0 or greater"),
      i = "Do you need to upgrade the pins package?"
    ))
  }

  yaml
}


check_pin_exists <- function(board, name) {
  if (pin_exists(board, name)) {
    invisible()
  } else {
    abort(c(
      glue("Can't find pin called '{name}'"),
      i = "Use `pin_list()` to see all available pins in this board"
    ), class = "pins_pin_absent")
  }
}

