test_that("dspins boards", {


  user_id <- "test_user"
  expect_error(board_name(user_id))

  user_id <- "test-user"
  expect_equal(board_name(user_id), "dskt-ch-test-user")

  # Non existing user
  expect_false(dspins_user_board_exists(user_id))
  expect_false(dspins_is_board_connected(user_id))

  expect_true(dspins_user_board_connect(user_id))

})
