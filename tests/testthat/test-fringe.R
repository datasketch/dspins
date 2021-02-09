test_that("fringe", {

  bucket_id <- "testuser"
  folder <- "test"
  expect_true(dspins_user_board_connect(folder = folder, bucket_id = bucket_id))
  expect_true(dspins_user_board_connect(folder = folder, bucket_id = "user"))

  library(homodatum)
  current_title <- paste0("Sample fringe - ", as.character(Sys.time()))
  current_slug <- create_slug(current_title)
  df <- homodatum::sample_data("Cat")

  f <- fringe(df, name = current_title)
  pin_url <- pin(f, folder = folder, bucket_id = bucket_id)

  pins <- dspins::dspin_list(folder, bucket_id)
  expect_true(any(pins$title == current_title))

  # test meta data
  url_base_path <- paste0("https://",bucket_id,".dskt.ch/",folder,"/",current_slug,"/",current_slug)
  url_share <- paste0("https://datasketch.co/",folder,"/",current_slug)

  meta_info_pin <- pins %>% filter(name == current_slug)
  expect_equal(names(meta_info_pin$stats), c("nrow", "ncol"))

  expect_equal(meta_info_pin$files$csv$path, paste0(current_slug,".csv"))
  expect_equal(meta_info_pin$files$csv$url, paste0(url_base_path,".csv"))

  expect_equal(meta_info_pin$files$json$path, paste0(current_slug,".json"))
  expect_equal(meta_info_pin$files$json$url, paste0(url_base_path,".json"))

  expect_equal(meta_info_pin$share$csv$link, url_share)
  expect_equal(meta_info_pin$share$csv$permalink, paste0(url_base_path,".csv"))

  expect_equal(meta_info_pin$share$json$link, url_share)
  expect_equal(meta_info_pin$share$json$permalink, paste0(url_base_path,".json"))

  # test errors and warnings
  expect_error(pin(f), "Need a folder to save fringe")

  expect_message(pin(f, folder = folder), "No bucket_id specified. Using 'user.dskt.ch' by default.")

})
