test_that("ds boards", {

  dotenv::load_dot_env()
  user_id <- paste0(rep("0",24),collapse = "")

  dspins_user_board_connect(user_id)

  pins <- dspins::pin_list(user_id)

})
