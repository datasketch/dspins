test_that("dspins, list, get ", {

  #options(pins.verbose = FALSE)

  bucket_id <- paste0(rep("0",24),collapse = "")

  expect_true(dspins_user_board_connect(bucket_id))

  pinned_mtcars <- pin(mtcars, board = board_name(bucket_id))

  library(homodatum)
  data <- data.frame(book = c("Black", "Red"), value = 1:2)
  f <- fringe(data, name = "Some file 2", description = "I just made this files up",
               license = "CC-BY", date_created = unix_timestamp(),
              access = "private")

  #pin(f) # local board by default

  pinned_fringe <- pin(f, bucket_id = bucket_id) # local board by default

  all_pins <- pin_list(bucket_id)
  pin_remove("mtcars", board = board_name(bucket_id))



})
