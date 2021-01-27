test_that("fringe", {

  library(homodatum)
  title <- "Sample fringe"
  df <- homodatum::sample_data("Cat")

  bucket_id <- "testuser"
  folder <- "test"

  f <- fringe(df, name = title)
  pin_url <- pin(f, folder = folder, bucket_id = bucket_id)

  f <- fringe(df, name = "Another fringe")
  pin_url <- pin(f, folder = folder, bucket_id = bucket_id)

  fringe <- dspin_get("another-fringe", folder = folder, bucket_id = bucket_id)
  fringe$data

  expect_message(pin(dv), "Need a folder to save fringe")

  expect_message(pin(f, folder = folder), "No bucket_id specified. Using 'user.dskt.ch' by default.")

})
