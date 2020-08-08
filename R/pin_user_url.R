#' @export
pin_user_url <- function(title, element, user_id, user_name) {
  if (is.reactive(title)) title <- title()
  if (is.reactive(element)) element <- element()

  Sys.setlocale(locale = "en_US.UTF-8")
  dv <- dsviz(element, name = title)
  dspins_user_board_connect(user_id)
  pin_url <- pin(dv, user_id = user_id)
  url <- paste0(user_name, ".datasketch.co/", title)
  if (is.null(pin_url))
    url <- "pinnotfound"
  url
}
