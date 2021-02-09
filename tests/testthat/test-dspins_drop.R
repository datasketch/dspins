test_that("dspins drop", {

  bucket_id <- "testuser"
  folder <- "test"
  expect_true(dspins_user_board_connect(folder = folder, bucket_id = bucket_id))
  expect_true(dspins_user_board_connect(folder = folder, bucket_id = "user"))

  sample_path <- system.file("drop_sample", package = "dspins")
  path <- file.path(sample_path, "sample.txt")

  current_title <- paste0("Sample drop - ", as.character(Sys.time()))
  current_slug <- create_slug(current_title)
  dp <- drop(sample_path, name = current_title)
  pin_url <- pin(dp, folder = folder, bucket_id = bucket_id)

  pins <- dspins::dspin_list(folder, bucket_id)
  expect_true(any(pins$name == current_slug))

  # test meta data
  url_base_path <- paste0("https://",bucket_id,".dskt.ch/",folder,"/",current_slug,"/",current_slug)
  url_share <- paste0("https://datasketch.co/",folder,"/",current_slug)

  meta_info_pin <- pins %>% filter(name == current_slug)

  files_paths <- sub('.*\\/', '',  meta_info_pin$files_paths[[1]])
  expect_true(length(setdiff(files_paths, c("sample.html", "sample.jpg", "sample.pdf", "sample.png", "sample.txt"))) == 0)

  expect_equal(meta_info_pin$files$`1`$path, paste0(current_slug,"."))
  expect_equal(meta_info_pin$files$`1`$url, paste0(url_base_path,"."))

  expect_equal(meta_info_pin$share$`1`$link, url_share)
  expect_equal(meta_info_pin$share$`1`$permalink, paste0(url_base_path,"."))
  expect_equal(meta_info_pin$share$`1`$embed, paste0("<iframe src=\"",paste0(url_base_path,"."),"\" frameborder=0 width=\"100%\" height=\"400px\"></iframe>"))

  # test errors and warnings
  expect_error(pin(dp), "Need a folder to save drop")

  expect_message(pin(dp, folder = folder), "No bucket_id specified. Using 'user.dskt.ch' by default.")

})

test_that("drop_update_meta", {

  title <- "Some file"
  sample_path <- system.file("drop_sample", package = "dspins")
  path <- file.path(sample_path, "sample.txt")

  dp0 <- drop(sample_path, name = title)

  expect_equal(dp0$name, "Some file")

  dp1 <- drop_update_meta(dp0, name = "Another file name")

  expect_equal(dp1$name, "Another file name")
  expect_equal(dp1$slug, "another-file-name")
  expect_equal(dp1$access, "private")

  dp2 <- drop_update_meta(dp1, name = "Some file", slug="new_file")
  dp3 <- drop_update_meta(dp0, slug = "new_file")
  expect_equal(dp2, dp3)

  sources <- list(title = "source name", path = "url-of-source")

  dp4 <- drop(sample_path, sources = sources)
  expect_equal(dp4$sources, sources)

  update_sources <- list(list(title = "another source", path = "url-of-source"),
                         list(title = "add one more", path = "url-of-this-source"))

  dp5 <- drop_update_meta(dp4, name = "this file", sources = update_sources)
  expect_equal(dp5$sources, update_sources)
  expect_equal(dp5$name, "this file")

  tag <- list('one tag')
  tags <- list(c('one tag', 'another tag'))
  dp6 <- drop_update_meta(dp0, tags = tag)
  dp7 <- drop_update_meta(dp6, tags = tags)
  expect_equal(dp6$tags, tag)
  expect_equal(dp7$tags, tags)

  expect_warning(drop_update_meta(dp0, filesize = 300),
                 "Cannot update filesize. Removing from meta.")

  some_fringe <- homodatum::fringe(data.frame(a = c(1,2,3), name = "a name"))
  expect_error(drop_update_meta(some_fringe, name = "another name"),
               "Input must be of class 'drop'.")

})
