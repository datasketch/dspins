#' Write DS pins and return URLs
#'
#' Pin a `fringe`, `dsviz`, or `drop` element to a board of type `dspins_board_s3`
#' and return links to pin on DS profile.
#'
#' @param board `dspins_board_s3` board
#' @param element Element to be saved (`fringe`, `dsviz`, or `drop`)
#' @param ...
#'
#' @return List of links to pin in DS profile
#'
#' @export
#'
#' @examples
#' \dontrun{
#' board <- ds_board_s3(user_name = "test", bucket_id = "user")
#'
#' fringe_mtcars <- homodatum::fringe(mtcars, name = "Mtcars dataset")
#' dsurls <- board %>% dspin_urls(fringe_mtcars)
#' }
dspin_urls <- function(board,
                       element = NULL,
                       ...) {


  bucket <- board$bucket
  bucket_id <- get_bucket_id(bucket)
  folder <- board$folder

  locale <- "en_US.UTF-8"
  if(Sys.info()[['sysname']] == "Windows") {
    locale <- "English_United States"
  }
  Sys.setlocale(locale = locale)

  board %>% dspin_write(element, ...)

  if(!class(element) == "drop"){
    get_element_urls(element, folder, bucket_id)
  }
}


element_type <- function(x){
  if(homodatum::is_fringe(x)) return("fringe")
  if(is_dsviz(x)) return("dsviz")
  if(class(x) == "drop") return("drop")
  stop("Element must be fringe, dsviz or drop.")
}

get_element_urls <- function(element, folder, bucket_id){

  element_slug <- element$slug
  baselink <-  file.path(paste0("https://", bucket_id, ".dskt.ch"), folder, element_slug)
  link <-  file.path("https://datasketch.co", folder, element_slug)
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
  dspins_user_board_connect(bucket_id = bucket_id, folder = user_name)
  pin_url <- pin(dv, folder = user_name, bucket_id = bucket_id)
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
