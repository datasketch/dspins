test_that("User url", {

  library(hgchmagic)
  title <- "MyBeautifulPlot"
  element <- hgch_bar_Cat(data.frame(Thinks = c("Rocks", "Paper", "Cuts")))
  user_id <- paste0(rep("0",24),collapse = "")
  user_name <- "Brandom"
  url_create <- pin_user_url(title, element, user_id, user_name)
  expect_equal(url_create, paste0(user_name, ".datasketch.co/", title))

})
