#' @export
dsviz <- function(viz, name = NULL, description = NULL, ...){
  args <- list(...)

  type <- dsviz_type(viz)
  if(type == "htmlwidget")
    formats <- c("html","png")
  if(type == "gg")
    formats <- c("png", "svg")

  dv <- list(
    name = name,
    description = description,
    slug = create_slug(name),
    type = type,
    viz_type = NULL,
    width = 600L,
    height = 400L,
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
dsviz_type <- function(viz){
  if(all(c("gg") %in% class(viz))){
    return("gg")
  }
  if(all(c("htmlwidget") %in% class(viz))){
    return("htmlwidget")
  }
}






