test_that("dspins drop", {

  sample_path <- system.file("drop_sample", package = "dspins")
  path <- file.path(sample_path, "sample.txt")

  dp <- drop(sample_path)
  #drop_write(dp, path = "tmp/sample_path")

  bucket_id <- "test"
  pin(dp, bucket_id = bucket_id, acl = "public")


  dp <- drop(path)


})
