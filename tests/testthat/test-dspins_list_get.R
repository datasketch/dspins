test_that("dspins, list, get ", {

  #options(pins.verbose = FALSE)

  user_id <- "test-user"
  dspins_user_board_connect(user_id)

  pin(mtcars, board = board_name("test-user"))

  library(homodatum)
  data <- data.frame(book = c("Black", "Red"), value = 1:2)
  f <- fringe(data, name = "Some file 2", description = "I just made this files up",
               license = "CC-BY", date_created = unix_timestamp(),
              access = "private")

  #pin(f) # local board by default

  pin_url <- pin(f, user_id = "test-user") # local board by default

  pinned <- pin_list(board = board_name(user_id))
  pin_remove("mtcars", board = board_name(user_id))



})
