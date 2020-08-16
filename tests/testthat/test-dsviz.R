test_that("dsviz", {

  library(hgchmagic)
  title <- "Sample chart"
  viz <- hgchmagic::hgch_bar_Cat(tibble(a = c("a","b")))

  bucket_id <- "test"

  dv <- dsviz(viz, name = title)
  #dsviz_write(dv, "tmp/viz")
  pin_url <- pin(dv, bucket_id = bucket_id)

  dv <- dsviz(viz, name = "Another Viz")
  #dsviz_write(dv, "tmp")
  pin_url <- pin(dv, bucket_id = bucket_id)

  myviz <- pin_get("another-viz")
  myviz$viz

  ####
  # library(ggmagic)
  # viz <- gg_pie_Cat(tibble(a = c("a","b")))


})
