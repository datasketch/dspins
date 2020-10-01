test_that("User url", {

  # PIN URLS
  user_name <- "test"


  # FRINGES

  library(homodatum)
  f <- fringe(x = data.frame(1:3))
  urls <- get_element_urls(f, bucket_id = "brandon")

  ## Urls generation
  expect_equal(paste0("https://datasketch.co/brandon/", f$slug), urls$link)
  expect_equal(paste0("https://brandon.dskt.ch/", f$slug, "/", f$slug, ".json"),
               urls$permalink)
  # Pin url
  urls <- dspin_urls(element = f, user_name = "test", download_formats = "xlsx")
  expect_equal(urls$link, "https://datasketch.co/test/data-frame-1-3")
  expect_equal(urls$permalink, "https://test.dskt.ch/data-frame-1-3/data-frame-1-3.json")

  info <- dspin_info("new-data", user_name)
  info$path


  # DSVIZ HTMLWIDGETS

  library(hgchmagic)
  hg <- hgch_bar_Cat(data.frame(Thinks = c("Rocks", "Paper", "Cuts")), title = "Nice chart")

  expect_error(get_element_urls(hg, bucket_id = "brandon"), "Element must be fringe or dsviz")
  dvhg <- dsviz(hg, height = 600)
  expect_equal(dvhg$height, 600L)

  ## Urls generation
  urls <- get_element_urls(dvhg, bucket_id = "brandon")
  expect_equal(urls$permalink, "https://brandon.dskt.ch/nice-chart/nice-chart.html")
  expect_equal(urls$iframe_embed,
               "<iframe src=\"https://brandon.dskt.ch/nice-chart/nice-chart.html\" frameborder=0 width=\"100%\" height=\"400px\"></iframe>")

  # pin viz
  urls <- dspin_urls(element = dvhg, user_name = "test")
  expect_equal(urls$link, paste0("https://datasketch.co/test/", dvhg$slug))
  expect_equal(urls$permalink, paste0("https://test.dskt.ch/", dvhg$slug,"/", dvhg$slug,".html"))


  # DSVIZ GGMAGIC


  library(ggmagic)
  gg <- gg_bar_Cat(d = data.frame(x=c("a","a","b")), title = "Another Chart")
  dvgg <- dsviz(gg)

  ## Urls generation
  urls <- get_element_urls(dvgg, bucket_id = "brandon")
  expect_equal(urls$permalink, "https://brandon.dskt.ch/another-chart/another-chart.png")

  # pin viz
  urls <- dspin_urls(element = dvgg, user_name = "test")
  expect_equal(urls$link, paste0("https://datasketch.co/test/", dvgg$slug))
  expect_equal(urls$permalink, paste0("https://test.dskt.ch/", dvgg$slug,"/", dvgg$slug,".png"))


})
