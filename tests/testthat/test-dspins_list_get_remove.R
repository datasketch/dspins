test_that("dspins, list ", {

  #options(pins.verbose = FALSE)

  bucket_id <- "testuser"
  folder <- "test"
  expect_true(dspins_user_board_connect(folder = folder, bucket_id = bucket_id))
  expect_true(dspins_user_board_connect(bucket_id = "user", folder = folder))

  #local
  pinned_mtcars <- pin(mtcars)
  pin_remove("mtcars", board = "local")

  library(homodatum)
  data <- data.frame(book = c("Black", "Red"), value = 1:2)
  f <- fringe(data, name = "Some file 2", description = "I just made this files up",
               license = "CC-BY", date_created = unix_timestamp(),
              access = "private")

  pinned_fringe <- pin(f, folder = folder, bucket_id = bucket_id)

  expect_error(dspin_list(bucket_id = "testuser"), "Need a folder to retrieve list of pins.")

  expect_message(dspin_list(folder = "test"), "No bucket_id specified. Using 'user.dskt.ch' by default.")

  all_pins <- dspin_list(folder, bucket_id)

  expect_true("some-file-2" %in% all_pins$path)

})

test_that("dspins, get ", {

  library(hgchmagic)
  title <- "Another viz"
  viz <- hgchmagic::hgch_bar_Cat(tibble(a = c("a","b")))

  bucket_id <- "testuser"
  folder <- "test"

  dv <- dsviz(viz, name = title)
  pin_url <- pin(dv, folder = folder, bucket_id = bucket_id)
  pin_url <- pin(dv, folder = folder)

  expect_error(dspin_get("another-viz"), "Need a folder to retrieve pin.")

  expect_message(dspin_get("another-viz", folder = folder), "No bucket_id specified. Using 'user.dskt.ch' by default.")

  pins <- dspin_get("another-viz", folder = folder, bucket_id = bucket_id)

})

test_that("dspins, remove ", {

  bucket_id <- "testuser"
  folder <- "test"

  library(homodatum)
  current_title <- paste0("mtcars-remove")
  f <- fringe(mtcars, name = current_title)

  pinned_f <- pin(f, folder = folder, bucket_id = bucket_id)

  expect_error(dspin_remove(current_title, bucket_id = bucket_id), "Need a folder to remove pin")

  expect_error(dspin_remove(current_title, folder = folder), "Need a bucket_id to remove pin")

  dspin_remove(current_title, folder = folder, bucket_id = bucket_id)

})
