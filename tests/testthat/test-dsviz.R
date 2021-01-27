test_that("dsviz", {

  library(hgchmagic)
  title <- "Sample chart"
  viz <- hgchmagic::hgch_bar_Cat(tibble(a = c("a","b")))

  bucket_id <- "testuser"
  folder <- "test"

  dv <- dsviz(viz, name = title)
  pin_url <- pin(dv, folder = folder, bucket_id = bucket_id)

  expect_message(pin(dv), "Need a folder to save dsviz")

  expect_message(pin(dv, folder = folder), "No bucket_id specified. Using 'user.dskt.ch' by default.")

  ####
  # library(ggmagic)
  # viz <- gg_pie_Cat(tibble(a = c("a","b")))


})

test_that("dsviz_update_meta", {

  title <- "Sample chart"
  viz <- hgchmagic::hgch_bar_Cat(tibble(a = c("a","b")))

  dv0 <- dsviz(viz, name = title)

  expect_equal(dv0$name, "Sample chart")

  dv1 <- dsviz_update_meta(dv0, name = "Another chart name")

  expect_equal(dv1$name, "Another chart name")
  expect_equal(dv1$slug, "another-chart-name")
  expect_equal(dv1$access, "private")

  dv2 <- dsviz_update_meta(dv1, name = "Sample chart", slug="new_chart")
  dv3 <- dsviz_update_meta(dv0, slug = "new_chart")
  expect_equal(dv2, dv3)

  sources <- list(title = "source name", path = "url-of-source")

  dv4 <- dsviz(viz, sources = sources)
  expect_equal(dv4$sources, sources)

  update_sources <- list(list(title = "another source", path = "url-of-source"),
                         list(title = "add one more", path = "url-of-this-source"))

  dv5 <- dsviz_update_meta(dv4, name = "this chart", sources = update_sources)
  expect_equal(dv5$sources, update_sources)
  expect_equal(dv5$name, "this chart")

  tag <- list('one tag')
  tags <- list(c('one tag', 'another tag'))
  dv6 <- dsviz_update_meta(dv0, tags = tag)
  dv7 <- dsviz_update_meta(dv6, tags = tags)
  expect_equal(dv6$tags, tag)
  expect_equal(dv7$tags, tags)

  expect_warning(dsviz_update_meta(dv0, viz_type = "ggplot"),
                 "Cannot update viz_type. Removing from meta.")

  some_fringe <- homodatum::fringe(data.frame(a = c(1,2,3), name = "a name"))
  expect_error(dsviz_update_meta(some_fringe, name = "another name"),
               "Input must be of class 'dsviz'.")

})
