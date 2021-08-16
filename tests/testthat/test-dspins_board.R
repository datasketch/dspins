test_that("valid names", {


  folder <- ""
  expect_error(validate_folder_name(folder)) # Cannot be empty

  folder <- "test_board"
  expect_error(validate_folder_name(folder)) # can only contain letters, numbers and dashes

  folder <- "test"
  expect_equal(validate_folder_name(folder), "test")

  bucket_id <- ""
  expect_error(bucket_name(bucket_id)) # Cannot be empty

  bucket_id <- "test_user"
  expect_error(bucket_name(bucket_id)) # can only contain letters, numbers and dashes

  bucket_id <- "testuser"
  expect_equal(bucket_name(bucket_id), "testuser.dskt.ch")

  bucket_id <- "testuser"
  folder <- ""
  expect_error(board <- ds_board_s3(user_name = folder, bucket_id = bucket_id))

  bucket_id <- "testuser"
  folder <- "test_board"
  expect_error(board <- ds_board_s3(user_name = folder, bucket_id = bucket_id))

  bucket_id <- ""
  folder <- "testboard"
  expect_error(board <- ds_board_s3(user_name = folder, bucket_id = bucket_id))

  bucket_id <- "test_user"
  folder <- "testboard"
  expect_error(board <- ds_board_s3(user_name = folder, bucket_id = bucket_id))

})

test_that("dspins boards", {

  bucket_id <- "testuser"
  folder <- "test"
  bucket <- bucket_name(bucket_id)

  expect_silent(board <- ds_board_s3(user_name = folder, bucket_id = bucket_id))

  expect_true(is_dspins_board_s3(board))

  expected_cache <- paste0("s3-",bucket,"/", folder)

  expect_equal(board$board, "dspins_board_s3")
  expect_equal(board$name, "s3")
  expect_equal(board$bucket, bucket)
  expect_equal(board$folder, folder)
  expect_false(board$versioned)
  expect_true(grepl(expected_cache, board$cache))

})
