test_that("valid names", {


  folder <- ""
  expect_error(valid_folder_name(folder)) # Cannot be empty

  folder <- "test_board"
  expect_error(valid_folder_name(folder)) # can only contain letters, numbers and dashes

  folder <- "test"
  expect_equal(valid_folder_name(folder), "test")

  bucket_id <- ""
  expect_error(bucket_name(bucket_id)) # Cannot be empty

  bucket_id <- "test_user"
  expect_error(bucket_name(bucket_id)) # can only contain letters, numbers and dashes

  bucket_id <- "testuser"
  expect_equal(bucket_name(bucket_id), "testuser.dskt.ch")

  bucket_id <- "testuser"
  folder <- ""
  expect_error(dspins_user_board_connect(folder = folder, bucket_id = bucket_id))

  bucket_id <- "testuser"
  folder <- "test_board"
  expect_error(dspins_user_board_connect(folder = folder, bucket_id = bucket_id))

  bucket_id <- ""
  folder <- "testboard"
  expect_error(dspins_user_board_connect(folder = folder, bucket_id = bucket_id))

  bucket_id <- "test_user"
  folder <- "testboard"
  expect_error(dspins_user_board_connect(folder = folder, bucket_id = bucket_id))

})

test_that("dspins boards", {

  bucket_id <- "testuser"
  folder <- "test"
  expect_true(dspins_user_board_connect(folder = folder, bucket_id = bucket_id))
  expect_true(dspins_is_board_connected(folder = folder, bucket_id = bucket_id))

  bucket <- bucket_name(bucket_id)
  board_name <- board_name(bucket_id, folder)

  board <- pins::board_get(board_name)

  expect_equal(board$board, "datatxt")
  expect_equal(board$name, board_name)
  expect_equal(board$url, paste0("http://",bucket, ".s3.amazonaws.com"))
  expect_equal(board$borwse_url, paste0("https://s3.console.aws.amazon.com/s3/buckets/",board_name,"/"))
  expect_equal(board$bucket, bucket)
  expect_equal(board$subpath, folder)

})
