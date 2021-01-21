test_that("dspins, list, get, remove", {

  #options(pins.verbose = FALSE)

  bucket_id <- "test-dspins-do-not-delete"

  expect_true(dspins_user_board_connect("user"))

  pinned_mtcars <- pin(mtcars, bucket_id = bucket_id)

  library(homodatum)
  data <- data.frame(book = c("Black", "Red"), value = 1:2)
  f <- fringe(data, name = "Some file 2", description = "I just made this files up",
               license = "CC-BY", date_created = unix_timestamp(),
              access = "private")

  #pin(f) # local board by default

  pinned_fringe <- pin(f, bucket_id = bucket_id) # local board by default

  all_pins <- dspin_list(bucket_id)
  pin_remove("mtcars", board = "local")

  get <- dspin_get("some-file-2", bucket_id = bucket_id)

})

test_that(" remove ", {

  bucket_id <- "test-dspins-do-not-delete"
  library(homodatum)
  current_title <- paste0("mtcars-remove")
  f <- fringe(mtcars, name = current_title)

  pinned_f <- pin(f, bucket_id = bucket_id)

  dspin_remove(current_title, bucket_id = bucket_id)



})
