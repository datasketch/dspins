test_that("dsviz hgchmagic", {

  bucket_id <- "testuser"
  folder <- "test"
  board <- ds_board_s3(user_name = folder, bucket_id = bucket_id)

  library(hgchmagic)
  current_title <- paste0("Sample hgdviz - ", as.character(Sys.time()))
  current_slug <- create_slug(current_title)
  viz <- hgchmagic::hgch_bar_Cat(tibble(a = c("a","b")))

  dv <- dsviz(viz, name = current_title)

  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  slug <- dv$slug

  meta_info_pin <- dspin_save(dv, slug, board, path)

  # test meta data
  url_base_path <- paste0("https://",bucket_id,".dskt.ch/",folder,"/",current_slug,"/",current_slug)
  url_share <- paste0("https://datasketch.co/",folder,"/",current_slug)

  expect_equal(unlist(meta_info_pin$formats), c("html", "png"))

  expect_equal(meta_info_pin$files$html$path, paste0(current_slug,".html"))
  expect_equal(meta_info_pin$files$html$url, paste0(url_base_path,".html"))

  expect_equal(meta_info_pin$files$png$path, paste0(current_slug,".png"))
  expect_equal(meta_info_pin$files$png$url, paste0(url_base_path,".png"))

  expect_equal(meta_info_pin$share$html$link, url_share)
  expect_equal(meta_info_pin$share$html$permalink, paste0(url_base_path,".html"))
  expect_equal(meta_info_pin$share$html$embed, paste0("<iframe src=\"",paste0(url_base_path,".html"),"\" frameborder=0 width=\"100%\" height=\"400px\"></iframe>"))

  expect_equal(meta_info_pin$share$png$link, url_share)
  expect_equal(meta_info_pin$share$png$permalink, paste0(url_base_path,".png"))
  expect_equal(meta_info_pin$share$png$embed, paste0("<img src=\"",paste0(url_base_path,".png"),"\"></img>"))

})

test_that("dsviz ggmagic", {

  bucket_id <- "testuser"
  folder <- "test"
  board <- ds_board_s3(user_name = folder, bucket_id = bucket_id)

  library(ggmagic)
  current_title <- paste0("Sample ggdviz - ", as.character(Sys.time()))
  current_slug <- create_slug(current_title)
  viz <- gg_pie_Cat(tibble(a = c("a","b")))

  dv <- dsviz(viz, name = current_title)

  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  slug <- dv$slug

  meta_info_pin <- dspin_save(dv, slug, board, path)

  # test meta data
  url_base_path <- paste0("https://",bucket_id,".dskt.ch/",folder,"/",current_slug,"/",current_slug)
  url_share <- paste0("https://datasketch.co/",folder,"/",current_slug)

  expect_equal(unlist(meta_info_pin$formats), c("png", "svg"))

  expect_equal(meta_info_pin$files$svg$path, paste0(current_slug,".svg"))
  expect_equal(meta_info_pin$files$svg$url, paste0(url_base_path,".svg"))

  expect_equal(meta_info_pin$files$png$path, paste0(current_slug,".png"))
  expect_equal(meta_info_pin$files$png$url, paste0(url_base_path,".png"))

  expect_equal(meta_info_pin$share$svg$link, url_share)
  expect_equal(meta_info_pin$share$svg$permalink, paste0(url_base_path,".svg"))
  expect_equal(meta_info_pin$share$svg$embed, paste0("<img src=\"",paste0(url_base_path,".svg"),"></img>"))

  expect_equal(meta_info_pin$share$png$link, url_share)
  expect_equal(meta_info_pin$share$png$permalink, paste0(url_base_path,".png"))
  expect_equal(meta_info_pin$share$png$embed, paste0("<img src=\"",paste0(url_base_path,".png"),"\"></img>"))

})

test_that("dsviz_update_meta", {

  title <- "Sample chart"
  viz <- hgchmagic::hgch_bar_Cat(tibble(a = c("a","b")))

  dv0 <- dsviz(viz, name = title)

  expect_equal(dv0$name, "Sample chart")

  dv1 <- dsviz_update_meta(dv0, name = "Another chart name")

  expect_equal(dv1$name, "Another chart name")
  expect_equal(dv1$slug, "another-chart-name")
  expect_equal(dv1$access, "private")

  dv2 <- dsviz_update_meta(dv1, name = "Sample chart", slug="new_chart")
  dv3 <- dsviz_update_meta(dv0, slug = "new_chart")
  expect_equal(dv2, dv3)

  sources <- list(title = "source name", path = "url-of-source")

  dv4 <- dsviz(viz, sources = sources)
  expect_equal(dv4$sources, sources)

  update_sources <- list(list(title = "another source", path = "url-of-source"),
                         list(title = "add one more", path = "url-of-this-source"))

  dv5 <- dsviz_update_meta(dv4, name = "this chart", sources = update_sources)
  expect_equal(dv5$sources, update_sources)
  expect_equal(dv5$name, "this chart")

  tag <- list('one tag')
  tags <- list(c('one tag', 'another tag'))
  dv6 <- dsviz_update_meta(dv0, tags = tag)
  dv7 <- dsviz_update_meta(dv6, tags = tags)
  expect_equal(dv6$tags, tag)
  expect_equal(dv7$tags, tags)

  expect_warning(dsviz_update_meta(dv0, viz_type = "ggplot"),
                 "Cannot update viz_type. Removing from meta.")

  some_fringe <- homodatum::fringe(data.frame(a = c(1,2,3), name = "a name"))
  expect_error(dsviz_update_meta(some_fringe, name = "another name"),
               "Input must be of class 'dsviz'.")

})
