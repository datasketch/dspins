#' @export
pin_user_url <- function(title, element, user_id, user_name, ...) {
  if (shiny::is.reactive(title)) title <- title()
  if (shiny::is.reactive(element)) element <- element()

  Sys.setlocale(locale = "en_US.UTF-8")
  dv <- dsviz(element, name = title)
  dspins_user_board_connect(user_id)
  pin_url <- pin(dv, user_id = user_id)
  url <- paste0(user_name, ".datasketch.co/", title)
  if (is.null(pin_url))
    url <- "pinnotfound"
  url
}

#' @export
pin_fringe_url <- function(element = NULL, element_name = NULL, org_id = NULL, org_name = NULL, user_id = NULL, user_name = NULL, ...) {
  args <- as.list(match.call())[-1]
  lapply(names(args), function(s) {
    if (shiny::is.reactive(args[[s]])) {
      args[[s]] <<- do.call(args[[s]], list())
    } else {
      args[[s]] <<- args[[s]]
    }
  })
  if (is.null(args$element)) {
    stop("Element cannot be null")
  }
  if (is.null(c(args$user_id, args$org_id)) | is.null(c(args$user_name, args$org_name))) {
    stop("User, organization id or name cannot be null")
  }
  id <- args$org_id %||% args$user_id
  name <- args$org_name %||% args$user_name
  el_name <- args$element_name %||% paste0("saved_", gsub(" ", "_", substr(as.POSIXct(Sys.time()), 1, 19)))
  args$name <- el_name
  args$slug <- el_name
  if (!is_fringe(args$element)) {
    element <- fringe(na.omit(args$element))
  }
  f <- modifyList(element, args)
  Sys.setlocale(locale = "en_US.UTF-8")
  dspins_user_board_connect(f$user_id)
  message("\n\nSAVING PIN\n\n")
  pin_url <- pin(f, user_id = f$user_id)
  message("\n\nSAVED PIN\n\n", pin_url)
  url <-  paste0(f$user_name, ".datasketch.co/", f$name)
  if (is.null(pin_url)) url <- "pinnotfound"
  url
}
