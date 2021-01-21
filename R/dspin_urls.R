#' @export
dspin_urls <- function(element = NULL,
                     user_name = NULL, org_name = NULL,
                     overwrite = FALSE, ...) {
  if (is.null(element)) {
    stop("Element cannot be null")
  }
  if (is.null(user_name) && is.null(org_name)) {
    stop("User, organization id or name cannot be null")
  }

  # Make sure it works with reactives
  # args <- as.list(match.call())[-1]
  # lapply(names(args), function(s) {
  #   if (shiny::is.reactive(args[[s]])) {
  #     args[[s]] <<- do.call(args[[s]], list())
  #   } else {
  #     args[[s]] <<- args[[s]]
  #   }
  # })

  bucket_id <- org_name %||% user_name

  bucket_id <- valid_folder_name(bucket_id)

  locale <- "en_US.UTF-8"
  if(Sys.info()[['sysname']] == "Windows") {
    locale <- "English_United States"
  }
  Sys.setlocale(locale = locale)

  if(!dspins_is_board_connected("user")){
    dspins_user_board_connect("user")
  }

  # Validate element is fringe or dsviz
  element_type(element)

  el <- pin(element, bucket_id = bucket_id, ...)
  get_element_urls(el, bucket_id)
}


element_type <- function(x){
  if(homodatum::is_fringe(x)) return("fringe")
  if(is_dsviz(x)) return("dsviz")
  stop("Element must be fringe or dsviz")
}

get_element_urls <- function(element, bucket_id){

  element_slug <- element$slug
  baselink <-  file.path(paste0("https://", bucket_id, ".dskt.ch"), element_slug)
  link <-  file.path("https://datasketch.co", bucket_id, element_slug)
  el_type <- element_type(element)

  if(el_type == "dsviz"){
    viz_type <- element$viz_type
    if(viz_type == "gg")
      ext <- ".png"
    if(viz_type == "htmlwidget")
      ext <- ".html"
  }else if(el_type == "fringe"){
    ext <- ".json"
  }


  iframe_embed <- NULL
  img_html <- NULL
  permalink <- file.path(baselink, paste0(element_slug, ext))
  if(ext == ".html"){
    iframe_embed <- paste0('<iframe src="',permalink,
                           '" frameborder=0 width="100%" height="400px"></iframe>')
  }
  if(ext == ".png"){
    img_html <- paste0('<img src="',permalink,'"></img>')
  }

  list(link = link,
       permalink = permalink,
       img_html = img_html,
       iframe_embed = iframe_embed)
}


#' @export
pin_user_url <- function(title, element, bucket_id, user_name, ...) {
  if (shiny::is.reactive(title)) title <- title()
  if (shiny::is.reactive(element)) element <- element()

  locale <- "en_US.UTF-8"
  if(Sys.info()[['sysname']] == "Windows") {
    locale <- "English_United States"
  }
  Sys.setlocale(locale = locale)
  dv <- dsviz(element, name = title)
  dspins_user_board_connect(bucket_id)
  pin_url <- pin(dv, bucket_id = bucket_id)
  url <- paste0(user_name, ".datasketch.co/", title)
  if (is.null(pin_url))
    url <- "pinnotfound"
  url
}

#' @export
pin_fringe_url <- function(element = NULL, element_name = NULL, org_id = NULL, org_name = NULL, bucket_id = NULL, user_name = NULL, ...) {

  if (is.null(c(args$bucket_id, args$org_id)) | is.null(c(args$user_name, args$org_name))) {
    stop("User, organization id or name cannot be null")
  }
  id <- args$org_id %||% args$bucket_id
  name <- args$org_name %||% args$user_name
  el_name <- args$element_name %||% paste0("saved_", gsub(" ", "_", substr(as.POSIXct(Sys.time()), 1, 19)))
  args$name <- el_name
  args$slug <- el_name
  if (!is_fringe(args$element)) {
    element <- fringe(na.omit(args$element))
  }
  f <- modifyList(element, args)
  locale <- "en_US.UTF-8"
  if(Sys.info()[['sysname']] == "Windows") {
    locale <- "English_United States"
  }
  Sys.setlocale(locale = locale)
  dspins_user_board_connect(f$bucket_id)
  message("\n\nSAVING PIN\n\n")
  pin_url <- pin(f, bucket_id = f$bucket_id)
  message("\n\nSAVED PIN\n\n", pin_url)
  url <-  paste0(f$user_name, ".datasketch.co/", f$name)
  if (is.null(pin_url)) url <- "pinnotfound"
  url
}
