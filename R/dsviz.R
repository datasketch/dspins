#' @export
dsviz <- function(viz, name = NULL, description = NULL, ...){
  args <- list(...)
  dv <- list(
    name = name,
    description = description,
    slug = create_slug(name),
    type = dsviz_type(viz),
    viz_type = NULL,
    access = args$access %||% "private",
    license = NULL,
    #time_created = NULL,
    time_last_updated = args$time_last_updated %||% unix_timestamp(),
    formats = args$formats,
    tags = args$tags,
    sources = args$sources,
    dsapp = args$dsapp,
    fringe = args$fringe,
    viz = viz
  )
  class(dv) <- "dsviz"
  dv
}


dsviz_write <- function(dv, path, ...){
  if(!"dsviz" %in% class(dv)){
    stop("dv must be of class dsviz")
  }
  args <- list(...)
  viz <- dv$viz
  type <- dsviz_type(viz)
  if(type == "gg"){
    ggmagic::save_ggmagic(viz, path, "png",
                          width = args$width %||% 5, height = args$height %||% 5)
  }
  if(type == "htmlwidget"){
    filepath <- paste0(random_name(),".html")
    htmlwidgets::saveWidget(viz, filepath,
                            selfcontained = TRUE)
    dir.create(path, recursive = TRUE)
    file.copy(filepath, file.path(path, paste0(dv$slug,".html")))
    file.remove(filepath)
  }
  dv$viz <- NULL
  y <- modifyList(dv, args$meta %||% list())
  y$filesize = file.info(path)$size
  yaml::write_yaml(y, file.path(path, paste0(basename(dv$slug),".yaml")))
}

dsviz_type <- function(viz){
  if(all(c("gg", "dsviz") %in% class(viz))){
    return("gg")
  }
  if(all(c("htmlwidget") %in% class(viz))){
    return("htmlwidget")
  }
}




