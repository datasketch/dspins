test_that("ds boards", {

  library(pins)

  # use dotenv::load_dot_env() when running interactively
  dotenv::load_dot_env("../../.env")

  user_id <- paste0(rep("0",24),collapse = "")

  dspins_user_board_connect(user_id)

  pins <- dspins::pin_list(user_id)

  library(hgchmagic)
  h <- hgch_bar_Cat(data.frame(x = letters[1:3]))
  h
  dv <- dsviz(h, name = "Viz title 222", description = "estofunciona2")
  #dsviz_write(dv, "tmp/viz")
  pin_url <- pin(dv, user_id = user_id)

  pin_userfirngel_url <- "brandon.datasketh.co/viz-ttitile"


})
