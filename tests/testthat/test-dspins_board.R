test_that("dspins boards", {


  bucket_id <- ""
  expect_error(valid_folder_name(bucket_id)) # Cannot be empty

  bucket_id <- "test_user"
  expect_error(valid_folder_name(bucket_id)) # can only contain letters, numbers and dashes

  bucket_id <- "test"
  expect_equal(valid_folder_name(bucket_id), "test")

})
