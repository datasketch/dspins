test_that("dspins, list, get ", {

  #options(pins.verbose = FALSE)

  user_id <- paste0(rep("0",24),collapse = "")

  expect_true(dspins_user_board_connect(user_id))

  pinned_mtcars <- pin(mtcars, board = board_name(user_id))

  library(homodatum)
  data <- data.frame(book = c("Black", "Red"), value = 1:2)
  f <- fringe(data, name = "Some file 2", description = "I just made this files up",
               license = "CC-BY", date_created = unix_timestamp(),
              access = "private")

  #pin(f) # local board by default

  pinned_fringe <- pin(f, user_id = user_id) # local board by default

  all_pins <- pin_list(user_id)
  pin_remove("mtcars", board = board_name(user_id))



})
