#' Write DS pins
#'
#' Pin a `fringe`, `dsviz`, or `drop` element to a board of type `dspins_board_s3`.
#'
#' @param board `dspins_board_s3` board
#' @param element Element to be saved (`fringe`, `dsviz`, or `drop`)
#' @param ...
#'
#' @export
#'
#' @examples
#' \dontrun{
#' board <- ds_board_s3(user_name = "test", bucket_id = "user")
#'
#' fringe_mtcars <- homodatum::fringe(mtcars, name = "Mtcars dataset")
#' board %>% dspin_write(fringe_mtcars)
#' }
dspin_write <- function(board,
                        element,
                        ...) {
  ellipsis::check_dots_used()
  check_board(board, "pin_write()", "pin()")

  # Validate element is fringe or dsviz
  element_type(element)

  # Create temp path
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  slug <- element$slug

  # Save element to local path
  metadata <- dspin_save(element, slug, board, path, ...)

  # Upload files to ds s3 board
  dspin_store(board, slug, path, metadata, ...)

  if(metadata$type == "dsviz"){
    change_content_types(metadata = metadata, board = board)
  }
}


#' Read DS pins
#'
#' Read a `fringe` or `dsviz` element to a board of type `dspins_board_s3`.
#'
#' Raises error for element of type `drop`.
#'
#' @param board `dspins_board_s3` board
#' @param name Name of element to be read
#' @param ...
#'
#' @return DS element
#'
#' @export
#'
#' @examples
#' \dontrun{
#' board <- ds_board_s3(user_name = "test", bucket_id = "user")
#'
#' board %>% dspin_read("mtcars-dataset")
#' }
dspin_read <- function(board,
                       name,
                       hash = NULL,
                       ...) {
  ellipsis::check_dots_used()
  check_board(board, "pin_read()", "pin_get()")

  meta <- dspin_fetch(board, name, ...)

  if(meta$type == "drop") stop("DS type `drop` can't be read. Retrieve uploaded paths with `dspin_download()`.")

  meta$file <- "data.rds"

  check_hash(meta, hash)

  ds_object_read(meta)
}
