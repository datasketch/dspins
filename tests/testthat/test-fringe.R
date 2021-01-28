test_that("fringe", {

  bucket_id <- "testuser"
  folder <- "test"
  expect_true(dspins_user_board_connect(bucket_id = bucket_id, folder = folder))
  expect_true(dspins_user_board_connect(bucket_id = "user", folder = folder))

  library(homodatum)
  current_title <- paste0("Sample fringe - ", as.character(Sys.time()))
  df <- homodatum::sample_data("Cat")

  f <- fringe(df, name = current_title)
  pin_url <- pin(f, folder = folder, bucket_id = bucket_id)

  pins <- dspins::dspin_list(folder, bucket_id)
  expect_true(any(pins$title == current_title))

  expect_error(pin(f), "Need a folder to save fringe")

  expect_message(pin(f, folder = folder), "No bucket_id specified. Using 'user.dskt.ch' by default.")

})
