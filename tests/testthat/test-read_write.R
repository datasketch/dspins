test_that("dspins, write fringe ", {

  user_name <- "test"
  bucket_id <- "testuser"
  board <- ds_board_s3(user_name = user_name, bucket_id = bucket_id)

  library(homodatum)
  title <- paste0("Sample fringe - ", as.character(Sys.time()))
  slug <- create_slug(title)
  data <- data.frame(book = c("Black", "Red"), value = 1:2)

  f <- fringe(data, name = title, description = "I just made this files up",
              license = "CC-BY", date_created = unix_timestamp(),
              access = "private")

  board %>% pin_write(f)

  pin_exists <- board %>% pin_exists(slug)

  expect_true(pin_exists)

  pin <- board %>% pin_read(slug)

  expect_equal(class(pin), "fringe")

})

test_that("dspins, write, read dsviz ", {

  user_name <- "test"
  bucket_id <- "testuser"
  board <- ds_board_s3(user_name = user_name, bucket_id = bucket_id)

  library(hgchmagic)
  title <- paste0("Sample hg dsviz - ", as.character(Sys.time()))
  slug <- create_slug(title)
  hgviz <- hgchmagic::hgch_bar_Cat(tibble(a = c("a","b")))

  hgdv <- dsviz(hgviz, name = title)

  board %>% pin_write(hgdv)

  pin_exists <- board %>% pin_exists(slug)

  expect_true(pin_exists)

  pin <- board %>% pin_read(slug)

  expect_equal(class(pin), "dsviz")


  library(ggmagic)
  title <- paste0("Sample gg dsviz - ", as.character(Sys.time()))
  slug <- create_slug(title)
  ggviz <- ggmagic::gg_bar_Cat(tibble(a = c("a","b")))

  ggdv <- dsviz(ggviz, name = title)

  board %>% pin_write(ggdv)

  pin_exists <- board %>% pin_exists(slug)

  expect_true(pin_exists)

  pin <- board %>% pin_read(slug)

  expect_equal(class(pin), "dsviz")

})

test_that("dspins, write, read, download drop ", {

  user_name <- "test"
  bucket_id <- "testuser"
  board <- ds_board_s3(user_name = user_name, bucket_id = bucket_id)

  sample_path <- system.file("drop_sample", package = "dspins")
  path <- file.path(sample_path, "sample.txt")

  title <- paste0("Sample drop - ", as.character(Sys.time()))
  slug <- create_slug(title)
  dp <- drop(sample_path, name = title)

  board %>% pin_write(dp)

  pin_exists <- board %>% pin_exists(slug)

  expect_true(pin_exists)

  expect_error(board %>% pin_read(slug), "DS type `drop` can't be read.")

  expect_output(board %>% pin_download(slug), "Found")

})

