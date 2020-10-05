test_that("dspins drop", {

  sample_path <- system.file("drop_sample", package = "dspins")
  path <- file.path(sample_path, "sample.txt")

  dp <- drop(sample_path)
  #drop_write(dp, path = "tmp/sample_path")

  bucket_id <- "test"
  pin(dp, bucket_id = bucket_id, acl = "public")


  dp <- drop(path)


})

test_that("drop_update_meta", {

  title <- "Some file"
  sample_path <- system.file("drop_sample", package = "dspins")
  path <- file.path(sample_path, "sample.txt")

  dp0 <- drop(sample_path, name = title)

  expect_equal(dp0$name, "Some file")

  dp1 <- drop_update_meta(dp0, name = "Another file name")

  expect_equal(dp1$name, "Another file name")
  expect_equal(dp1$slug, "another-file-name")
  expect_equal(dp1$access, "private")

  dp2 <- drop_update_meta(dp1, name = "Some file", slug="new_file")
  dp3 <- drop_update_meta(dp0, slug = "new_file")
  expect_equal(dp2, dp3)

  sources <- list(title = "source name", path = "url-of-source")

  dp4 <- drop(sample_path, sources = sources)
  expect_equal(dp4$sources, sources)

  update_sources <- list(list(title = "another source", path = "url-of-source"),
                         list(title = "add one more", path = "url-of-this-source"))

  dp5 <- drop_update_meta(dp4, name = "this file", sources = update_sources)
  expect_equal(dp5$sources, update_sources)
  expect_equal(dp5$name, "this file")

  expect_warning(drop_update_meta(dp0, filesize = 300),
                 "Cannot update filesize. Removing from meta.")

  some_fringe <- homodatum::fringe(data.frame(a = c(1,2,3), name = "a name"))
  expect_error(drop_update_meta(some_fringe, name = "another name"),
               "Input must be of class 'drop'.")

})
