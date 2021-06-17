test_that("User url", {

  user_name <- "test"
  slug <- "some-slug"

  url_base <- get_url_base_path("user.dskt.ch", folder = user_name, slug = slug)
  share_link <- paste0("https://datasketch.co/",user_name, "/", slug)

  # FRINGES
  element_type <- "fringe"
  formats <- c("csv", "json")
  paths <- paste0(slug, ".", formats)
  urls <- paste0(url_base, ".", formats)

  links <- create_ds_links(slug = slug, folder = user_name, formats = formats, element_type = element_type)

  expect_equal(links$files$csv$path, paths[1])
  expect_equal(links$files$json$path, paths[2])

  expect_equal(links$files$csv$format, formats[1])
  expect_equal(links$files$json$format, formats[2])

  expect_equal(links$files$csv$url, urls[1])
  expect_equal(links$files$json$url, urls[2])

  expect_equal(links$share$csv$link, share_link)
  expect_equal(links$share$json$link, share_link)

  expect_equal(links$share$csv$permalink, urls[1])
  expect_equal(links$share$json$permalink, urls[2])



  # DSVIZ HTMLWIDGETS
  element_type <- "dsviz"
  formats <- c("html", "png")
  paths <- paste0(slug, ".", formats)
  urls <- paste0(url_base, ".", formats)
  links <- create_ds_links(slug = slug, folder = user_name, formats = formats, element_type = element_type)

  expect_equal(links$files$html$path, paths[1])
  expect_equal(links$files$png$path, paths[2])

  expect_equal(links$files$html$format, formats[1])
  expect_equal(links$files$png$format, formats[2])

  expect_equal(links$files$html$url, urls[1])
  expect_equal(links$files$png$url, urls[2])

  expect_equal(links$share$html$link, share_link)
  expect_equal(links$share$png$link, share_link)

  expect_equal(links$share$html$permalink, urls[1])
  expect_equal(links$share$png$permalink, urls[2])

  expect_equal(links$share$html$embed,
               paste0("<iframe src=\"",urls[1],"\" frameborder=0 width=\"100%\" height=\"400px\"></iframe>"))
  expect_equal(links$share$png$embed,
               paste0("<img src=\"",urls[2],"\"></img>"))


  # pin viz
  element_type <- "dsviz"
  formats <- c("png", "svg")
  paths <- paste0(slug, ".", formats)
  urls <- paste0(url_base, ".", formats)
  links <- create_ds_links(slug = slug, folder = user_name, formats = formats, element_type = element_type)

  expect_equal(links$files$png$path, paths[1])
  expect_equal(links$files$svg$path, paths[2])

  expect_equal(links$files$png$format, formats[1])
  expect_equal(links$files$svg$format, formats[2])

  expect_equal(links$files$png$url, urls[1])
  expect_equal(links$files$svg$url, urls[2])

  expect_equal(links$share$png$link, share_link)
  expect_equal(links$share$svg$link, share_link)

  expect_equal(links$share$png$permalink, urls[1])
  expect_equal(links$share$svg$permalink, urls[2])

  expect_equal(links$share$png$embed,
               paste0("<img src=\"",urls[1],"\"></img>"))
  expect_equal(links$share$svg$embed,
               paste0("<img src=\"",urls[2],"></img>"))



})
