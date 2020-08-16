test_that("dspins boards", {


  bucket_id <- ""
  expect_error(board_name(bucket_id)) # Cannot be empty

  bucket_id <- "test_user"
  expect_error(board_name(bucket_id)) # can only contain letters, numbers and dashes

  bucket_id <- "test"
  expect_equal(board_name(bucket_id), "test.dskt.ch")

  # Load env

  # use dotenv::load_dot_env() when running interactively
  dotenv::load_dot_env("../../.env")

  # Existing user
  expect_true(dspins_user_board_exists(bucket_id))
  expect_true(dspins_user_board_connect(bucket_id))
  expect_true(dspins_is_board_connected(bucket_id))

  # Non existing user

  bucket_id <- paste0("test",sample(1e10,1))

  expect_false(dspins_user_board_exists(bucket_id))
  expect_false(dspins_is_board_connected(bucket_id))

  expect_true(dspins_user_board_connect(bucket_id)) # connect
  expect_true(dspins_is_board_connected(bucket_id))

  # dspins_user_board_connect(bucket_id)
  aws.s3::delete_bucket(board_name(bucket_id))

})
