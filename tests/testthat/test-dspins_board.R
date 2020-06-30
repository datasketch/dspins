test_that("dspins boards", {


  user_id <- "test_user"
  expect_error(board_name(user_id)) # can only contain letters, numbers and dashes

  user_id <- "test-user"
  expect_equal(board_name(user_id), "dskt-ch-test-user")

  # Load env

  load_env()

  # Existing user
  expect_true(dspins_user_board_exists(user_id))
  expect_false(dspins_is_board_connected(user_id))

  expect_true(dspins_user_board_connect(user_id))
  expect_true(dspins_is_board_connected(user_id))

  # Non existing user

  user_id <- paste0("test-user-",sample(100:900,1))

  expect_false(dspins_user_board_exists(user_id))
  expect_false(dspins_is_board_connected(user_id))

  expect_true(dspins_user_board_connect(user_id)) # connect
  expect_true(dspins_is_board_connected(user_id))


  #
  # user_id <-
  # dspins_user_board_connect(user_id)
  # aws.s3::delete_bucket(board_name(user_id))

})
