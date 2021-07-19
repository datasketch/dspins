test_that("dspins, list ", {

  bucket_id <- "testuser"
  folder <- "test"
  expect_silent(board <- ds_board_s3(user_name = folder, bucket_id = bucket_id))

  library(homodatum)
  title <- "Some file in list"
  slug <- create_slug(title)
  data <- data.frame(book = c("Black", "Red"), value = 1:2)
  f <- fringe(data, name = title, description = "I just made this files up",
               license = "CC-BY", date_created = unix_timestamp(),
              access = "private")

  board %>% pin_write(f)

  all_pins_names <- board %>% pin_list()
  all_pins <- board %>% pin_list(extended = TRUE)

  expect_true(slug %in% all_pins_names)

  expect_equal(class(all_pins), "data.frame")
  expect_true(title %in% all_pins$title)
  expect_true(slug %in% all_pins$slug)
  expect_true(slug %in% all_pins$slug)
  expect_true(all(unique(all_pins$type) %in% c("fringe", "dsviz", "drop")))

})

