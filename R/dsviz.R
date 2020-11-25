#' @export
dsviz <- function(viz, name = NULL, description = NULL, ...){
  args <- list(...)

  type <- dsviz_type(viz)
  if(type == "htmlwidget"){
    formats <- c("html","png")
    width <- args$width %||% "100%"
  }
  if(type == "gg"){
    formats <- c("png", "svg")
    width <- args$width %||% 600L
  }


  name <- name %||% get_dsviz_title(viz)

  dv <- list(
    name = name,
    description = description,
    slug = create_slug(name),
    type = type,
    viz_type = type,
    width = width,
    height = as.integer(args$height) %||% 400L,
    access = args$access %||% "private",
    license = NULL,
    #time_created = NULL,
    time_last_updated = args$time_last_updated %||% unix_timestamp(),
    formats = args$formats %||% formats,
    tags = args$tags,
    sources = args$sources,
    dsapp = args$dsapp,
    fringe = args$fringe,
    viz = viz
  )
  class(dv) <- "dsviz"
  dv
}

#' @export
dsviz_update_meta <- function(dv, ...){
  if(class(dv) != "dsviz") stop("Input must be of class 'dsviz'.")
  fixed <- c("viz", "type", "viz_type")
  args <- list(...)
  if(any(names(args) %in% fixed)){
    warning("Cannot update ",
            paste0(names(args)[names(args) %in% fixed], collapse = ", "),
            ". Removing from meta.")
    args <- args[!names(args) %in% fixed]
  }
  info <- list(name = args$name %||% dv$name,
               description = args$description %||% dv$description,
               slug = args$slug %||% create_slug(args$name),
               width = as.integer(args$width) %||% dv$width,
               height = as.integer(args$height) %||% dv$height,
               access = args$access %||% dv$access,
               license = NULL,
               #time_created = NULL,
               time_last_updated = args$time_last_updated %||% dv$time_last_updated,
               formats = args$formats %||% dv$formats,
               dsapp = args$dsapp %||% dv$dsapp,
               fringe = args$fringe %||% dv$fringe)
  dv <- modifyList(dv, info)
  dv$sources <- args$sources %||% dv$sources
  dv$tags <- args$tags %||% dv$tags
  dv
}

#' @export
dsviz_write <- function(dv, path, ...){
  if(!"dsviz" %in% class(dv)){
    stop("dv must be of class dsviz")
  }
  args <- list(...)
  viz <- dv$viz
  type <- dsviz_type(viz)
  viz_width <- dv$width
  viz_height <- dv$height

  viz_path <- file.path(path, dv$slug)

  if(type == "gg"){
    ggsave(paste0(viz_path,".png"), plot = viz,
           width = viz_width/100, height = viz_height/100, units = "in", dpi = 100,
           device = "png")
    ggsave(paste0(viz_path,".svg"), plot = viz,
           width = viz_width/100, height = viz_height/100, units = "in", dpi = 100,
           device = "svg")
    # ggmagic::save_ggmagic(viz, viz_path, "png",
    #                       width = viz_width %||% 6, height = viz_height %||% 4)
  }
  if(type == "htmlwidget"){
    filepath <- paste0(random_name(),".html")
    htmlwidgets::saveWidget(viz, filepath,
                            selfcontained = TRUE)
    dir.create(path, recursive = TRUE)
    file.copy(filepath, paste0(viz_path,".html"))
    if (!webshot::is_phantomjs_installed())
      webshot::install_phantomjs()
    webshot::webshot(paste0(viz_path,".html"), paste0(viz_path,".png"),
                     vwidth = viz_width, vheight = viz_height, delay = 0.2)
    file.remove(filepath)

  }
  dv$viz <- NULL
  y <- modifyList(dv, args$meta %||% list())

  if(type == "htmlwidget")
    size <- file.info(paste0(viz_path,".html"))$size
  if(type == "gg")
    size <- file.info(paste0(viz_path,".png"))$size
  y$filesize <- size
  yaml::write_yaml(y, file.path(path, paste0(basename(dv$slug),".yaml")))
}

#' @export
is_dsviz <- function(viz){
  "dsviz" %in% class(viz)
}


#' @export
dsviz_type <- function(viz){
  if(any(c("gg", "ggmagic") %in% class(viz))){
    return("gg")
  }
  if(all(c("htmlwidget") %in% class(viz))){
    return("htmlwidget")
  }
}

get_dsviz_title <- function(viz){
  type <- dsviz_type(viz)
  if(type == "htmlwidget"){
    if("highchart" %in% class(viz)){
      return(viz$x$hc_opts$title$text)
    }
  }
  if(type == "gg"){
    return(viz$labels$title)
  }
}



