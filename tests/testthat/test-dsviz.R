test_that("dsviz", {

  library(hgchmagic)
  title <- "Sample chart"
  viz <- hgchmagic::hgch_bar_Cat(tibble(a = c("a","b")))

  dv <- dsviz(viz, name = title)
  #dsviz_write(dv, "tmp")
  pin_url <- pin(dv, user_id = "test-user")

  dv <- dsviz(viz, name = "Another Viz")
  #dsviz_write(dv, "tmp")
  pin_url <- pin(dv, user_id = "test-user")

  myviz <- pin_get("another-viz")
  myviz$viz

  ####
  # library(ggmagic)
  # viz <- gg_pie_Cat(tibble(a = c("a","b")))


})
