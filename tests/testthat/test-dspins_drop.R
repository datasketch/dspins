test_that("dspins drop", {

  sample_path <- system.file("drop_sample", package = "dspins")
  path <- file.path(sample_path, "sample.txt")

  dp <- drop(sample_path)
  #drop_write(dp, path = "tmp/sample_path")

  pin(dp, user_id = "test-user")


  dp <- drop(path)




})
