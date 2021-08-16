#' @importFrom dplyr %>%


flatten_vec_chr <- function(x, sep = ","){
  paste0(flatten_chr(as.list(x)), collapse = sep)
}

removeNull <- function(l) Filter(Negate(is.null), l)


eq_dataframe <- function(d1,d2){
  all(names(d1) == names(d2), nrow(d1) == nrow(d2), ncol(d1) == ncol(d2))
}


#' @export
random_name <- function(n = 10, ext = "") {
  if(nchar(ext) != 0) ext <- paste0(".",ext)
  paste0(paste0(sample(c(LETTERS,letters,0:9), n, TRUE),collapse = ""),ext)
}

remove_accents <- function (string){
  accents <- "àèìòùÀÈÌÒÙáéíóúýÁÉÍÓÚÝäëïöüÄËÏÖÜâêîôûÂÊÎÔÛñÑç"
  translation <- "aeiouAEIOUaeiouyAEIOUYaeiouAEIOUaeiouAEIOUnNc"
  chartr(accents, translation, string)
}

#' @export
create_slug <- function (x){
  x <- gsub("[^[:alnum:]]", "-", x)
  x <- remove_accents(x)
  x <- tolower(x)
  x <- gsub("-+", "-", x)
  x <- gsub("+-$", "", x)
  x <- gsub("^-.", "", x)
  x
}

file_ext <- function (x){
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

#' @export
unix_timestamp <- function(){
  as.integer(as.POSIXct(Sys.time()))
}

#' @export
unix_timestamp_to_date <- function(x){
  as.POSIXct(as.numeric(x), origin="1970-01-01")
}

#' @export
`%||%` <- function (x, y) {
  suppressWarnings({
    if (is.empty(x))
      return(y)
    else if (is.null(x) || is.na(x))
      return(y)
    else if (class(x) == "character" && all(nchar(x) == 0))
      return(y)
    else x
  })
}

#' @export
is.empty <- function (x) {
  !as.logical(length(x))
}

#' @export
is.url <- function(x){
  grepl("^http", x)
}


change_content_types <- function(metadata, board){

  slug <- metadata$slug
  viz_type <- metadata$viz_type

  if(is.null(viz_type)) stop("Need viz type to change content type.")
  if(!viz_type %in% c("gg", "htmlwidget")) stop("Viz type must be `gg` or `htmlwidget`.")

  if(viz_type == "htmlwidget"){
    change_content_type(slug = slug, format = "png", board = board)
    change_content_type(slug = slug, format = "html", board = board)
  }
  if(viz_type == "gg"){
    change_content_type(slug = slug, format = "png", board = board)
    change_content_type(slug = slug, format = "svg", board = board)
  }
}


change_content_type <- function(slug, format, board){

  bucket <- board$bucket
  folder <- board$folder

  content_type <- list(csv = "text/csv",
                       html = "text/html",
                       json = "application/json",
                       png = "image/png",
                       svg = "image/svg+xml")

  file_name <- glue::glue("{folder}/{slug}/{slug}.{format}")

  aws.s3::copy_object(from_object = file_name,
                      to_object = file_name,
                      from_bucket = bucket,
                      to_bucket = bucket,
                      headers = list(`Content-Type` = content_type[[format]],
                                     `x-amz-metadata-directive` = "REPLACE"))

}


cache_touch <- function(board, meta) {
  path <- fs::path(meta$local$dir, "data.txt")
  if (fs::file_exists(path)) {
    fs::file_touch(path)
  } else {
    fs::file_create(path)
  }

}


check_hash <- function(meta, hash) {
  if (is.null(hash)) {
    return()
  }

  pin_hash <- pin_hash(fs::path(meta$local$dir, meta$file))
  if (!is_prefix(hash, pin_hash)) {
    abort(paste0(
      "Specified hash '", hash, "' doesn't match pin hash '", pin_hash, "'"
    ))
  }
}

pin_hash <- function(paths) {
  if (length(paths) == 1) {
    hash_file(paths)
  } else {
    hashes <- map_chr(paths, hash_file)
    hash(hashes)
  }
}

hash_file <- function(path) {
  digest::digest(file = path, algo = "xxhash64")
}

check_board <- function(x, v1, v0) {
  if (!inherits(x, "dspins_board_s3")) {
    abort("`board` must be a dspin board")
  }

  if (!1 %in% x$api) {
    this_not_that(v0, v1)
  }
}

this_not_that <- function(this, that) {
  abort(glue("Use `{this}` with this board, not `{that}`"))
}

check_name <- function(x) {
  if (!is.character(x)) {
    abort("`name` must be a string")
  }

  if (grepl("\\\\|/", x, perl = TRUE)) {
    abort("`name` can not contain slashes")
  }
}

write_meta <- function(x, path) {
  path <- fs::path(path, "data.txt")
  write_yaml(x, path)
}

write_yaml <- function(x, path) {
  x <- to_utf8(x)
  yaml::write_yaml(x, path)
}

to_utf8 <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) {
      names(x) <- enc2utf8(names(x))
    }
    lapply(x, to_utf8)
  } else if (is.character(x)) {
    enc2utf8(x)
  } else {
    x
  }
}
