test_that("User url", {

  user_name <- "test"
  bucket_id <- "testuser"

  expect_true(dspins_user_board_connect(folder = user_name, bucket_id = bucket_id))

  # FRINGES

  library(homodatum)
  current_title <-  "Example data"
  f <- fringe(x = data.frame(1:3), name = current_title)
  urls <- get_element_urls(f, folder = user_name, bucket_id)

  ## Urls generation
  expect_equal(paste0("https://datasketch.co/",user_name, "/", f$slug), urls$link)
  expect_equal(paste0("https://s3.amazonaws.com/",bucket_id,".dskt.ch/",user_name,"/", f$slug, "/", f$slug, ".json"),
               urls$permalink)
  # Pin url
  urls <- dspin_urls(element = f, user_name = user_name, bucket_id = bucket_id, download_formats = "xlsx")
  expect_equal(urls$link, paste0("https://datasketch.co/",user_name,"/", f$slug))
  expect_equal(urls$permalink, paste0("https://s3.amazonaws.com/",bucket_id,".dskt.ch/",user_name,"/",f$slug,"/",f$slug,".json"))

  # DSVIZ HTMLWIDGETS

  library(hgchmagic)
  hg <- hgch_bar_Cat(data.frame(Thinks = c("Rocks", "Paper", "Cuts")), title = "Nice chart")

  expect_error(get_element_urls(hg, folder = user_name, bucket_id = bucket_id), "Element must be fringe or dsviz")
  dvhg <- dsviz(hg, height = 600)
  expect_equal(dvhg$height, 600L)

  ## Urls generation
  urls <- get_element_urls(dvhg, folder = user_name, bucket_id = bucket_id)
  expect_equal(urls$permalink, paste0("https://s3.amazonaws.com/",bucket_id,".dskt.ch/",user_name,"/", dvhg$slug, "/", dvhg$slug, ".html"))
  expect_equal(urls$iframe_embed,
               paste0("<iframe src=\"",paste0("https://s3.amazonaws.com/",bucket_id,".dskt.ch/",user_name,"/", dvhg$slug, "/", dvhg$slug, ".html"),"\" frameborder=0 width=\"100%\" height=\"400px\"></iframe>"))

  # pin viz
  urls <- dspin_urls(element = dvhg, user_name = user_name, bucket_id = bucket_id)
  expect_equal(urls$link, paste0("https://datasketch.co/",user_name,"/", dvhg$slug))
  expect_equal(urls$permalink, paste0("https://s3.amazonaws.com/",bucket_id,".dskt.ch/",user_name,"/",dvhg$slug,"/",dvhg$slug,".html"))

  # DSVIZ GGMAGIC
  library(ggmagic)
  gg <- gg_bar_Cat(d = data.frame(x=c("a","a","b")), title = "Another Chart")
  dvgg <- dsviz(gg)

  ## Urls generation
  urls <- get_element_urls(dvgg, folder = user_name, bucket_id = bucket_id)
  expect_equal(urls$permalink, paste0("https://s3.amazonaws.com/",bucket_id,".dskt.ch/",user_name,"/",dvgg$slug,"/",dvgg$slug,".png"))

  # pin viz
  urls <- dspin_urls(element = dvgg, user_name = user_name, bucket_id = bucket_id)
  expect_equal(urls$link, paste0("https://datasketch.co/",user_name,"/", dvgg$slug))
  expect_equal(urls$permalink, paste0("https://s3.amazonaws.com/",bucket_id,".dskt.ch/",user_name,"/",dvgg$slug,"/",dvgg$slug,".png"))


})
