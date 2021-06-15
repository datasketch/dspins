#' Create ds links
#'
#' Create list of links to files in ds profile and to share element.
#'
#' @param slug Slug of element
#' @param folder Folder of user
#' @param bucket AWS S3 bucket
#' @param formats File formats
#' @param element_type Element type ('dsviz', 'fringe', or 'drop')
#'
#' @return List containing a list of links to files in ds profile and a list of links to share element
#' @export
create_ds_links <- function(slug, folder, formats, element_type, bucket_id = "user"){

  bucket <- bucket_name(bucket_id)

  files <- create_file_links(slug = slug, folder = folder, bucket = bucket, formats = formats, element_type = element_type)
  share <- create_share_links(slug = slug, folder = folder, bucket = bucket, formats = formats, element_type = element_type)

  list(files = files,
       share = share)
}


create_file_links <- function(slug, folder, bucket, formats, element_type){

  url_base_path <- get_url_base_path(bucket = bucket, folder = folder, slug = slug)

  if(element_type %in% c("dsviz", "fringe")){

    lapply(formats, function(x){
      list(
        path = glue::glue(paste0("{slug}.",x)),
        format = x,
        url = glue::glue("{url_base_path}.{x}")
      )
    }) %>% setNames(formats)

  } else if(element_type == "drop"){

    format <- formats

    list(list(path = glue::glue(paste0("{slug}.", format)),
              format = format,
              url = glue::glue("{url_base_path}.{format}"))) %>%
      setNames(format)

  }

}


create_share_links <- function(slug, folder, bucket, formats, element_type){

  url_base_path <- get_url_base_path(bucket = bucket, folder = folder, slug = slug)


  if(element_type == "dsviz"){

    share <- list(
      html = list(
        link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
        permalink =  glue::glue("{url_base_path}.html"),
        embed =  paste0('<iframe src="',
                        glue::glue("{url_base_path}.html"),
                        '" frameborder=0 width="100%" height="400px"></iframe>')
      ),
      png = list(
        link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
        permalink =  glue::glue("{url_base_path}.png"),
        embed =  paste0('<img src="',
                        glue::glue("{url_base_path}.png"),
                        '"></img>')
      ),
      svg = list(
        link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
        permalink =  glue::glue("{url_base_path}.svg"),
        embed =  paste0('<img src="',
                        glue::glue("{url_base_path}.svg"),
                        '></img>')
      )
    )
    share[formats]

  } else if (element_type == "fringe"){

    list(
      html = list(
        link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
        permalink =  glue::glue("{url_base_path}.html"),
        embed =  paste0('<iframe src="',
                        glue::glue("{url_base_path}.html"),
                        '" frameborder=0 width="100%" height="400px"></iframe>')),
      csv = list(
        link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
        permalink =  glue::glue("{url_base_path}.csv")),
      json = list(
        link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
        permalink =  glue::glue("{url_base_path}.json"))
    )

  } else if (element_type == "drop"){

    format <- formats

    list(list(link =  glue::glue("https://datasketch.co/{folder}/{slug}"),
              permalink =  glue::glue("{url_base_path}.{format}"),
              embed =  paste0('<iframe src="',
                              glue::glue("{url_base_path}.{format}"),
                              '" frameborder=0 width="100%" height="400px"></iframe>'))) %>%
      setNames(format)

  }



}


get_url_base_path <- function(bucket, folder, slug){
  glue::glue("https://{bucket}/{folder}/{slug}/{slug}")
}
