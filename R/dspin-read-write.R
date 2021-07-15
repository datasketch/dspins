pin_read <- function(board, name, version = NULL, ...) {
  ellipsis::check_dots_used()
  UseMethod("pin_read")
}

pin_write <- function(board, name, version = NULL, ...) {
  ellipsis::check_dots_used()
  UseMethod("pin_write")
}

#' @export
dspin_write <- function(board, name, paths, metadata, versioned = NULL, x = NULL, ...) {
  ellipsis::check_dots_used()
  UseMethod("dspin_write")
}


pin_read.dspins_board_s3 <- function(board, name, version = NULL, hash = NULL, ...) {
  ellipsis::check_dots_used()
  check_board(board, "pin_read()", "pin_get()")

  meta <- pin_fetch(board, name, version = version, ...)

  meta$file <- switch(meta$type,
                      "fringe" = meta$files$csv$path,
                      "dsviz" = "data.rds",
                      "drop" = NULL)

  meta$type <- switch(meta$type,
                      "fringe" = "csv",
                      "dsviz" = "rds",
                      "drop" = "file")

  meta$api_version <- 1

  check_hash(meta, hash)

  object_read(meta)
}


pin_write.dspins_board_s3 <- function(board,
                                      x,
                                      name = NULL,
                                      type = NULL,
                                      desc = NULL,
                                      metadata = NULL,
                                      versioned = NULL,
                                      ...) {
  ellipsis::check_dots_used()

  element_type(x)

  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  slug <- x$slug
  type <- class(x)

  metadata <- dspin_write(x, slug, board, path, ...)

  pin_store(board, slug, path, metadata, versioned = versioned, ...)

  if(type == "dsviz"){
    if(dv$type == "htmlwidget"){
      change_content_type(slug = slug, format = "png", board = board)
      change_content_type(slug = slug, format = "html", board = board)
    }
    if(dv$type == "gg"){
      change_content_type(slug = slug, format = "png", board = board)
      change_content_type(slug = slug, format = "svg", board = board)
    }

  }
}



