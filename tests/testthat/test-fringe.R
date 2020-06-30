test_that("multiplication works", {

  library(homodatum)
  f <- fringe(mtcars, name = "mtcars")
  #drop_write(dp, path = "tmp/sample_path")

  pin(f, user_id = "test-user", acl = "public")

})
