test_that("dspins, exists ", {

  bucket_id <- "testuser"
  folder <- "test"
  board <- ds_board_s3(user_name = folder, bucket_id = bucket_id)

  library(homodatum)
  data <- data.frame(book = c("Black", "Red"), value = 1:2)
  f <- fringe(data, name = "Some file exists", description = "I just made this files up",
               license = "CC-BY", date_created = unix_timestamp(),
              access = "private")

  board %>% pin_write(f)

  expect_true(board %>% pin_exists("some-file-exists"))

})


test_that("dspins, delete ", {

  bucket_id <- "testuser"
  folder <- "test"
  board <- ds_board_s3(user_name = folder, bucket_id = bucket_id)

  current_title <- paste0("mtcars-delete")
  f <- fringe(mtcars, name = current_title)

  board %>% pin_write(f)

  board %>% pin_delete(current_title)

  expect_false(board %>% pin_exists(current_title))

})


test_that("dspins, meta ", {

  bucket_id <- "testuser"
  folder <- "test"
  board <- ds_board_s3(user_name = folder, bucket_id = bucket_id)

  current_title <- "Some file meta"
  slug <- create_slug(current_title)
  data <- mtcars
  f <- fringe(mtcars, name = current_title)

  board %>% pin_write(f)

  meta <- board %>% pin_meta("some-file-meta")

  expect_equal(meta$type, "fringe")
  expect_equal(meta$title, current_title)
  expect_equal(meta$stats, list(nrow = nrow(mtcars), ncol = ncol(mtcars)))
  expect_equal(meta$slug, slug)
  expect_equal(meta$group, as.character(f$group))
  expect_equal(meta$frtype, as.character(f$frtype))

})
