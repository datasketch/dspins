test_that("ds boards", {

  library(pins)

  # use dotenv::load_dot_env() #when running interactively
  dotenv::load_dot_env("../../.env")

  bucket_id <- "test2"

  expect_true(dspins_user_board_connect(bucket_id))
  expect_true(dspins_is_board_connected(bucket_id))

  pins <- dspins::dspin_list(bucket_id)


  # Fringes work

  library(homodatum)
  current_title <- paste0("Mtcars - ", as.character(Sys.time()))
  f <- fringe(mtcars, name = current_title)
  #drop_write(dp, path = "tmp/sample_path")

  # Save pin

  #aws.s3::put_object("inst/drop_sample/sample.txt",
  #                   bucket = board_name(bucket_id))

  this_pin <- pin(f, bucket_id = bucket_id, acl = "public")
  pins <- dspins::dspin_list(bucket_id)
  expect_true(any(pins$title == current_title))


  # dsviz

  # htmlwidgets
  library(hgchmagic)
  current_title <- paste0("Htmlwidget - ", as.character(Sys.time()))
  h <- hgch_bar_Cat(data.frame(x = letters[1:3]), title = current_title)
  h
  dv <- dsviz(h, name = current_title, description = "This is an htmlwidget")
  #dsviz_write(dv, "tmp/viz")
  pin_url <- pin(dv, bucket_id = bucket_id)


  # ggvis
  library(ggmagic)
  current_title <- paste0("Ggmagic - ", as.character(Sys.time()))
  g <- gg_bar_CatNum(data.frame(x = letters[1:3], y = 1:3), title = current_title)
  g
  dv <- dsviz(g, name = current_title, description = "This is an htmlwidget")
  #dsviz_write(dv, "tmp/viz")
  pin_url <- pin(dv, bucket_id = bucket_id)


})






