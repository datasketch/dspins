test_that("ds boards", {

  library(pins)

  # use dotenv::load_dot_env() #when running interactively
  dotenv::load_dot_env("../../.env")
  bucket_id <- paste0(rep("0",24),collapse = "")
  expect_true(dspins_user_board_connect(bucket_id))
  pins <- dspins::pin_list(bucket_id)


  # Fringes work

  bucket_id <- paste0(rep("0",24),collapse = "")

  library(homodatum)
  current_title <- paste0("Mtcars - ", as.character(Sys.time()))
  f <- fringe(mtcars, name = current_title)
  #drop_write(dp, path = "tmp/sample_path")

  # Test bucket_id has 24 chars in board_name
  expect_error(pin(f, bucket_id = "", acl = "public"), "Need a correct bucket_id")

  # Save pin
  this_pin <- pin(f, bucket_id = bucket_id, acl = "public")
  pins <- dspins::pin_list(bucket_id)
  expect_true(any(pins$title == current_title))


  # dsviz

  bucket_id <- paste0(rep("0",24),collapse = "")

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






