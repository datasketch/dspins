#' DS pin list
#'
#' Get list of all pins saved to a board of type `dspins_board_s3`.
#'
#'
#' @param board `dspins_board_s3` board
#' @param extended Boolean to get simple or extended list, defaults to `FALSE`
#' @param ...
#'
#' @return If `extended = FALSE`, returns character vector of names of DS pins in board;
#' if `extended = TRUE`, returns `data.frame` showing contents of the board's `data.txt`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' board <- ds_board_s3(user_name = "test", bucket_id = "user")
#'
#' board %>% dspin_list()
#' board %>% dspin_list(extended = TRUE)
#' }
#' @export
dspin_list <- function(board,
                       extended = FALSE,
                       ...) {

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
