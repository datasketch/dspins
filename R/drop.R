#' @export
drop <- function(path, name = NULL, description = NULL, ...){
  args <- list(...)

  location <- local_or_remote(path)
  if(location == "remote"){
    files <- path
  }
  if(location == "local"){
    file_dir <- file_or_dir(path)
    if(file_dir == "dir"){
      files <- list.files(path, full.names = TRUE)
    }
    if(file_dir == "file"){
      files <- path
    }
  }
  name <- name %||% create_slug(basename(path))
  drop <- list(
    name = name,
    description = description %||% "",
    slug = create_slug(name),
    access = args$access %||% "private",
    license = NULL,
    #time_created = NULL,
    time_last_updated = args$time_last_updated %||% unix_timestamp(),
    filesize = file.info(path)$size,
    format = file_ext(path),
    tags = args$tags,
    sources = args$sources,
    files = files
  )
  class(drop) <- "drop"
  drop
}

#' @export
drop_update_meta <- function(drop, ...){
  if(class(drop) != "drop") stop("Input must be of class 'drop'.")
  fixed <- c("files", "filesize", "format")
  args <- list(...)
  if(any(names(args) %in% fixed)){
    warning("Cannot update ",
            paste0(names(args)[names(args) %in% fixed], collapse = ", "),
            ". Removing from meta.")
    args <- args[!names(args) %in% fixed]
  }
  info <- list(name = args$name %||% drop$name,
               description = args$description %||% drop$description,
               slug = args$slug %||% create_slug(args$name),
               access = args$access %||% drop$access,
               license = NULL,
               #time_created = NULL,
               time_last_updated = args$time_last_updated %||% drop$time_last_updated,
               tags = args$tags %||% drop$tags,
               sources = args$sources %||% drop$sources)
  drop <- modifyList(drop, info)
  drop
}


#' @export
drop_write <- function(drop, path = NULL, ...){
  if(!"drop" %in% class(drop)){
    stop("drop must be of class drop")
  }
  if(is.null(path)) stop("Need path")
  args <- list(...)
  dir.create(path, recursive = TRUE)
  meta_files <- lapply(drop$files, function(x){
    filename <- basename(x)
    target_path <- file.path(path, filename)
    file.copy(x, path)
    list(
      size = file.info(target_path)$size,
      name = filename
    )
  })

  y <- modifyList(drop, args$meta %||% list())
  y$meta_files <- meta_files
  yaml::write_yaml(y, file.path(path, paste0(basename(drop$slug),".yaml")))
}



file_or_dir <- function(path){
  if(dir.exists(path)) return("dir")
  if(file.exists(path)) return("file")
}

local_or_remote <- function(path){
  ifelse(grepl("^http", path), "remote", "local")
}


