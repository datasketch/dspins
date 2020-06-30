
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



#' @import pins
NULL
